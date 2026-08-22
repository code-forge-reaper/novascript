"""
NovaScript Interpreter – redesigned for cleaner class system, const support,
and reduced duplication.

Architecture overview
---------------------
* Tokenizer (class Tokenizer) – lexical analysis + recursive-descent parser.
  All AST construction lives here.  New syntax → extend the parse_* methods.
* Interpreter – evaluation / execution.
  - Environment / Var          → lexical scoping & constants (Var.const)
  - NovaClass / Proxy / bind_parameters → OOP with inheritance, private,
    const properties, and shared parameter binding
  - execute_stmt / evaluate_expr → the two main dispatch points

Class system redesign (from scratch improvements)
-------------------------------------------------
* `const` modifier now accepted on class properties (static or instance).
* Const properties are tracked on the instance (`__const__` set) and
  re-assignment raises NovaError.
* Private members still protected via current_class_stack + _private_* sets.
* Shared `bind_parameters()` helper eliminates repeated positional/keyword/
  default/compact binding logic across functions, methods, constructors,
  static methods and super calls.
* Method and constructor creation is cleaner and more consistent.
* Scope encapsulation remains lexical via Environment parent chains;
  instance data lives on the instance dict (or Python object) with
  __DefiningClass for identity and access checks.

How to add a new language feature
---------------------------------
1. Add the AST node class in nodes.py (inherit from Statement or Expression).
2. Extend the parser (Tokenizer.parse_statement / parse_* methods) to produce the node.
3. Prefer registering a handler:

       self._stmt_handlers["MyNewStmt"] = self._stmt_MyNewStmt

   (or _expr_handlers).  Falling through the if/elif chains still works.
4. Dynamic method binding continues to work via the __nova_ast__ marker on
   callable wrappers (see AssignmentExpr / LambdaDecl).
"""

import os
import sys
import pathlib
from nodes import *

ROOT = os.path.dirname(os.path.realpath(__file__))
LIBS_PATH = os.path.join(ROOT, "libs")

sys.path.insert(0, ROOT)
sys.path.insert(0, LIBS_PATH)
sys.path.insert(0, os.getcwd())


class Proxy:
    def __init__(self, set_func, get_func, instance, interpreter, cls):
        self._set = set_func
        self._get = get_func
        self.defining = instance
        self.interpreter = interpreter  # Need reference for stack
        self.cls = cls  # The defining class for private access

    def __str__(self):
        return str(self.get())

    def __repr__(self):
        return str(self.get())

    def get(self):
        if not self._get:
            raise NovaError(None, "Property has no getter.")
        try:
            if self.cls and self.interpreter:
                self.interpreter.current_class_stack.append(self.cls)
            return self._get()
        finally:
            if self.cls and self.interpreter and self.interpreter.current_class_stack:
                self.interpreter.current_class_stack.pop()

    def set(self, value):
        if not self._set:
            raise NovaError(None, "Property has no setter.")
        try:
            if self.cls and self.interpreter:
                self.interpreter.current_class_stack.append(self.cls)
            return self._set(value)
        finally:
            if self.cls and self.interpreter and self.interpreter.current_class_stack:
                self.interpreter.current_class_stack.pop()


import pprint


def dprint(str: str, node: Token):
    if os.environ.get("debugMode", "") == "Pretty":
        pprint.pprint(" " * node.column + f"- {str} {node.to_dict()}")
    elif os.environ.get("debugMode", "") == "Node":
        pprint.pprint(" " * node.column + f"- {str} {node.to_json()}")
    elif os.environ.get("debugMode", "") == "Simple":
        print(" " * node.column + node.__str__())


from nodes import *
import uuid
from collections.abc import Iterable, Callable
import importlib
import copy

impLib = importlib.import_module

__no_variable_set__ = object()

import math
import re
import array
import datetime, os
import time, json
from urllib.parse import unquote, quote
import struct, builtins

from collections.abc import Iterable


def _format(obj):
    # Custom string overload
    if isinstance(obj, dict) and "__Str" in obj:
        return obj["__Str"]()

    # NovaScript object instance
    if isinstance(obj, dict):
        if "__DefiningClass" in obj:
            cls = obj["__DefiningClass"]
            public = cls.get_public_instance_members(obj)
            return _format(public)

        # Regular dict (hide internal members)
        return {
            k: _format(v)
            for k, v in obj.items()
            if not (isinstance(k, str) and k.startswith("__"))
        }

    # Lists, tuples, sets, etc.
    if isinstance(obj, Iterable) and not isinstance(obj, (str, bytes)):
        return [_format(x) for x in obj]

    return obj


# --- Global Initialization ---
def init_globals(interpreter, globals_env):
    def pprint(*stuff):
        print(*(_format(x) for x in stuff))

    def _slice(ofWho, start, end=None):
        if end is not None:
            return ofWho[start:end]
        else:
            return ofWho[start:]

    def shift(x: list):
        v = x.pop(0)
        return v

    class Logger:
        @staticmethod
        def info(*args):
            now = datetime.datetime.now().strftime("%H:%M:%S")
            print(f"[info at: {now}]:", *args)

        @staticmethod
        def warn(*args):
            now = datetime.datetime.now().strftime("%H:%M:%S")
            print(f"[warn at: {now}]:", *args, file=sys.stderr)

        @staticmethod
        def error(*args):
            now = datetime.datetime.now().strftime("%H:%M:%S")
            print(f"[error at: {now}]:", *args, file=sys.stderr)

    class Is:
        @staticmethod
        def string(s):
            return isinstance(s, str)

        @staticmethod
        def number(n):
            return isinstance(n, (int, float))

        @staticmethod
        def instance(what, parent):
            return isinstance(what, parent)

        @staticmethod
        def int(n):
            return isinstance(n, (int))

        @staticmethod
        def float(n):
            return isinstance(n, (float))

        @staticmethod
        def boolean(b):
            return isinstance(b, bool)

        @staticmethod
        def array(a):
            return isinstance(a, list)  # renamed to be more idiomatic

        @staticmethod
        def dict(a):
            return isinstance(a, dict)

        @staticmethod
        def callable(c):
            return callable(c)

    # Has.<relation>(target, value) -> {target} Has {value} <relation>
    class Has:
        @staticmethod
        def key(d, k):
            if not isinstance(d, dict):
                raise TypeError(f"Expected dict, got {type(d).__name__}")
            return k in d

        @staticmethod
        def value(d, v):
            return v in d.values()

        @staticmethod
        def inside(target, what):
            return what in target  # works for list, str, set, tuple

        @staticmethod
        def attr(target, what):
            return hasattr(target, what)

    # args = sys.argv[2:] # Skip script name and NovaScript file name

    class Convert:
        @staticmethod
        def toInt(s):
            return int(s)

        @staticmethod
        def toTuple(s):
            return tuple(s)

        @staticmethod
        def toFloat(s):
            return float(s)

        @staticmethod
        def toNumber(s):
            if not isinstance(s, str):
                raise ValueError(f"this function expects a string, got {type(s)}")
            try:
                return int(s)
            except ValueError:
                return float(s)

        @staticmethod
        def toStr(thing):
            if isinstance(thing, list):
                return "".join(thing)
            else:
                return str(thing)

        @staticmethod
        def toBool(s):
            if isinstance(s, str):
                return s.lower() in ("true", "1", "yes")
            return bool(s)

        @staticmethod
        def toArray(s, n=None):
            if n:
                return array.array(s, n)
            return list(s)

        @staticmethod
        def toDict(s):
            return dict(s)

        @staticmethod
        def toChar(i):
            return chr(i)

        @staticmethod
        def toCharCode(s):
            return ord(s)

        @staticmethod
        def toBytes(s, c=None):
            if isinstance(s, str):
                return bytes(s, c)
            return bytes(s)

        @staticmethod
        def toByteArray(s, enc="utf8"):
            # Already bytes/bytearray → pass through
            if isinstance(s, (bytes, bytearray)):
                return bytearray(s)

            # int → single byte
            if isinstance(s, int):
                return bytearray([s & 0xFF])

            # float → 8-byte little-endian double
            if isinstance(s, float):
                return bytearray(struct.pack("<d", s))

            # iterable of ints → convert each to byte
            if isinstance(s, (list, tuple)):
                return bytearray([x & 0xFF for x in s])

            # string → encode
            if isinstance(s, str):
                return bytearray(s, enc)

            raise TypeError(f"Convert.toBytes: unsupported type {type(s)}")

    mmath = {}
    for k, v in math.__dict__.items():
        if not k.startswith("_"):
            mmath[k] = v
    mmath["abs"] = abs
    mmath["max"] = max
    mmath["min"] = min

    def load(path: str, env=None):  # const <modname> = load("modname")
        if not env:
            env = {}
        # ── FORCE Python import ──
        if path.startswith("py:"):
            py_path = path[3:].replace("/", ".")
            if py_path in interpreter.modules_loaded:
                return interpreter.modules_loaded[py_path]

            result = impLib(py_path)
            interpreter.modules_loaded[py_path] = result
            return result

        # ── Try Nova first ──
        nova_path = path if path.endswith(".nova") else path + ".nova"

        possible_locations = [
            nova_path,
            os.path.join(os.path.dirname(interpreter.file), nova_path),
            os.path.join(os.getcwd(), nova_path),
            os.path.join(LIBS_PATH, nova_path),
        ]

        file_path = None
        for candidate in possible_locations:
            if os.path.isfile(candidate):
                file_path = candidate
                break

        # ── If Nova module found → load it ──
        if file_path:
            if (
                file_path in interpreter.modules_loaded
                and "ignore loaded module" not in env
            ):
                return interpreter.modules_loaded[file_path]
            with open(file_path) as f:
                source = f.read()
            imported_interpreter = Interpreter(source, file_path)
            imported_env = Environment(globals_env)
            imported_env.define("exports", {})
            for k, v in env.items():
                imported_env.define(k, v)
            imported_env.localsOnly = True

            imported_interpreter.globals = imported_env
            imported_interpreter.globals.define("__IS_MAIN__", False, True)
            imported_interpreter.interpret()

            result = {k: v for k, v in imported_env.get("exports").value.items()}
            if "ignore loaded module" not in env:
                interpreter.modules_loaded[file_path] = result
            return result

        # ── Fallback: Python import (LAST RESORT) ──
        py_path = path.replace("/", ".")
        if py_path in interpreter.modules_loaded:
            return interpreter.modules_loaded[py_path]

        try:
            result = impLib(py_path)
            interpreter.modules_loaded[py_path] = result
            return result
        except Exception as e:
            raise RuntimeError(
                f"Cannot find module: {path}\n"
                f"Tried Nova:\n  "
                + "\n  ".join(possible_locations)
                + f"\nTried Python import: {py_path}\nError: {e}",
            )

    class Runtime:
        @staticmethod
        def dumpGlobals():
            from pprint import pprint

            print("global stuff = ")
            pprint(interpreter.globals)

        @staticmethod
        def exit(code=0):
            sys.exit(code)

        args = sys.argv[1:]

        @staticmethod
        def regex(pattern, options=""):
            return re.compile(pattern, 0 if "i" not in options else re.IGNORECASE)

        @staticmethod
        def env(key=None):
            if key:
                return os.environ.get(key)
            return dict(os.environ)  # Return a copy of the environment variables

        @staticmethod
        def panic(reason, *rest):
            if rest:
                reason = reason.format(*rest)  # Pythonic way to format
            raise Exception(reason)

        # When True, printing a function/lambda dumps its full AST node.
        # When False (default), functions print as <function name> / <lambda>.
        # Toggle from Nova with:  Runtime.showFunctionAst = true
        showFunctionAst = False

    global _RUNTIME_REF
    _RUNTIME_REF = Runtime

    class Fs:
        @staticmethod
        def read(path, opts=None):
            if not opts:
                opts = opts = {"mode": "r", "encoding": "utf8"}
            with open(path, **opts) as f:
                return f.read()

        @staticmethod
        def open(path, opts=None):
            if not opts:
                opts = opts = {"mode": "r", "encoding": "utf8"}
            return open(
                path, **opts
            )  # interpreter turns nova's objects into dicts when passing back to python, so this is safe

        @staticmethod
        def write(path, contents, opts=None):
            if not opts:
                opts = opts = {"mode": "w", "encoding": "utf8"}
            with open(path, **opts) as f:
                f.write(contents)

        @staticmethod
        def join(*s):
            return os.path.join(*s)

        @staticmethod
        def exists(path):
            return os.path.exists(path)

        @staticmethod
        def listdir(path="."):
            return os.listdir(path)

        @staticmethod
        def isdir(path):
            return os.path.isdir(path)

    class Uri:
        @staticmethod
        def decode(s):
            return unquote(s)

        @staticmethod
        def encode(s):
            return quote(s)

    class Time:
        @staticmethod
        def now(res="ns"):
            t = time.time_ns()

            if res == "ns":
                return t
            elif res == "ms":
                return t / 1_000_000
            elif res == "sec":
                return t / 1_000_000_000
            else:
                raise ValueError(f"unknown {res=}")

        @staticmethod
        def str():
            return str(datetime.datetime.now())

        @staticmethod
        def sleep(secs):
            return time.sleep(secs)

        @staticmethod
        def monotonic(resolution="ns"):
            if resolution == "ns":
                return time.perf_counter_ns()  # High-resolution time in nanoseconds
            elif resolution == "ms":
                return time.perf_counter_ns() / 1_000_000
            elif resolution == "sec":
                return time.perf_counter_ns() / 1_000_000_000
            else:
                raise ValueError(f"unknown {resolution = }")

    Time.sleep = staticmethod(time.sleep)

    class Object:
        @staticmethod
        def keys(obj):
            return list(obj.keys())

        @staticmethod
        def get(obj, key, default=None):
            return obj.get(key, default)

        @staticmethod
        def values(obj):
            return list(obj.values())

        @staticmethod
        def delete(x, y):
            del x[y]

        @staticmethod
        def items(obj):
            return list(obj.items())

        @staticmethod
        def setattr(obj, key, value):
            setattr(obj, key, value)

        @staticmethod
        def getattr(obj, key):
            return getattr(obj, key)

        @staticmethod
        def hasattr(obj, key):
            return hasattr(obj, key)

        @staticmethod
        def delattr(obj, key):
            builtins.delattr(obj, key)

        @staticmethod
        def attrs(obj):
            return dir(obj)

        @staticmethod
        def type(obj):
            return type(obj)

    builtin_values = {
        name: obj
        for name, obj in vars(builtins).items()
        if isinstance(obj, type) and issubclass(obj, BaseException)
    }
    builtin_values["NovaError"] = NovaError
    builtin_values.update(BUILTIN_VAR_TYPES)

    for e, v in builtin_values.items():
        globals_env.define(
            e, v
        )  # more for error checking and better controll over what to raise instead of Runtime.panic
    # name | value | const?
    globals_env.define("print", pprint)
    globals_env.define("write", lambda t, to: print(t, file=to))
    globals_env.define("input", input)
    globals_env.define("tuple", tuple)
    globals_env.define("shift", shift)
    globals_env.define("Logger", Logger)
    globals_env.define("Is", Is)
    globals_env.define("Has", Has)
    globals_env.define("Convert", Convert)
    globals_env.define("len", len)
    globals_env.define("hex", hex)
    globals_env.define("json", json)
    globals_env.define("load", load)
    globals_env.define("range", range)
    globals_env.define("slice", _slice)
    globals_env.define("math", mmath)
    globals_env.define("NaN", math.nan)
    globals_env.define("nil", None, True)
    globals_env.define("Runtime", Runtime)
    globals_env.define("Uri", Uri)
    globals_env.define("iter", iter)

    def nx(it):
        try:
            return next(it)
        except StopIteration:
            return None

    nx.__name__ = "next"
    globals_env.define("next", nx)
    globals_env.define("Fs", Fs)
    globals_env.define("Object", Object)
    globals_env.define("Time", Time)

    def r(x):
        raise x

    r.__name__ = "raise"
    globals_env.define("raise", r)


# Holds the Runtime class after init_globals so FuncWrapp can read
# Runtime.showFunctionAst without a circular import / hard dependency.
_RUNTIME_REF = None


class FuncWrapp:
    def __init__(self, func, desc_repr, desc_str, node, name=None):
        self.func = func
        self.desc_repr = desc_repr
        self.desc_str = desc_str
        self._name = name
        self._node = node

    def __call__(self, *args, **kw):
        return self.func(*args, **kw)

    def _short_name(self):
        if self._name:
            return self._name
        if self._node is not None and getattr(self._node, "name", None):
            return self._node.name
        return "lambda"

    def _want_ast(self):
        rt = _RUNTIME_REF
        if rt is None:
            return False
        return bool(getattr(rt, "showFunctionAst", False))

    def __str__(self):
        if self._want_ast() and self.desc_str is not None:
            try:
                return self.desc_str()
            except Exception:
                pass
        return f"<function {self._short_name()}>"

    def __repr__(self):
        if self._want_ast() and self.desc_repr is not None:
            try:
                return self.desc_repr()
            except Exception:
                pass
        return f"<function {self._short_name()}>"


class Var:
    def __init__(
        self, name, value, const=False, annotation=None, private=False, origin=None
    ):
        self.name = name
        self.value = value
        self.const = const
        self.type_annotation = annotation
        self.private = private
        self.origin = origin

    def __repr__(self):
        const_flag = "const " if self.const else ""
        type_info = f" {self.type_annotation} " if self.type_annotation else ""
        return f"Var({const_flag}{self.name}{type_info}= {self.value!r})"


# --- Environment for variable scoping ---
class Environment:
    def __init__(self, parent=None):
        self.values: dict[str, Var] = {
            # name, value, const
            "true": Var("true", True, True),
            "false": Var("false", False, True),
        }
        self.parent = parent
        self.deferred = []
        self.locked = False
        self.localsOnly = False

    def lock(self):
        self.locked = True

    def unlock(self):
        self.locked = False

    def add_deferred(self, stmt):
        if self.locked:
            raise NovaError(stmt, "Cannot add deferred statement to locked environment")
        self.deferred.append(stmt)

    def execute_deferred(self, interpreter):
        if self.locked:
            raise NovaError(
                None, "Cannot execute deferred statements in locked environment"
            )
        # Execute in reverse order of deferral
        while self.deferred:
            stmt = self.deferred.pop()
            interpreter.execute_stmt(stmt, self)

    def define(self, name: str, value, const=False, typeAnnotation=None):
        if self.locked:
            raise NovaError(None, "Cannot define variable in locked environment")
        self.values[name] = Var(name, value, const, typeAnnotation)

    def has(self, name: str):
        if name in self.values:
            return True
        elif self.parent and not self.localsOnly:
            return self.parent.has(name)
        else:
            return False

    def assign(self, name: str, value, tok):
        if self.locked:
            raise NovaError(tok, "Cannot assign to variable in locked environment")

        if name in self.values:
            var = self.values[name]

            # Check for constant violation (existing logic)
            if var.const:
                raise NovaError(tok, f"Cannot re-assign constant variable '{name}'")

            check_type(var.type_annotation, value, tok)
            var.value = value

        elif self.parent:
            if self.localsOnly:
                raise NovaError(
                    tok,
                    f"Cannot modify variable '{name}', since it is not created in this scope",
                )
            self.parent.assign(name, value, tok)
        else:
            raise NovaError(tok, f"Undefined variable {name}")

    def get(self, name: str, tok=None):
        if name in self.values:
            return self.values[name]
        elif self.parent:
            return self.parent.get(name, tok)
        else:
            if tok:
                raise NovaError(tok, f"Undefined variable {name}")
            raise Exception(
                f"Undefined variable {name}"
            )  # Fallback for internal errors without token

    def __repr__(self):
        v = {}
        for k in self.values:
            vv = self.values[k]
            if k not in ["true", "false"]:
                v[k] = vv
        return str(v)


def bind_parameters(
    parameters, args, kwargs, interpreter, env, context_name="function", token=None
):
    """
    Common helper to bind positional + keyword + default + compact parameters.
    Returns a dict of {param_name: value} after type checks.
    Raises NovaError on missing args, duplicates, unexpected kwargs, type errors.
    """
    compact_param = None
    normal_params = []
    for p in parameters:
        if getattr(p, "is_compact", False):
            compact_param = p
            break
        normal_params.append(p)

    num_normal = len(normal_params)
    values = {}

    # 1. Positional → normal params
    for i, param in enumerate(normal_params):
        if i < len(args):
            values[param.name] = args[i]

    # 2. Remaining positional → compact
    if compact_param:
        if len(args) > num_normal:
            values[compact_param.name] = list(args[num_normal:])
        else:
            values[compact_param.name] = []

    # 3. Keyword arguments
    param_names = {p.name for p in parameters}
    for kw_name, kw_val in kwargs.items():
        if kw_name not in param_names:
            raise NovaError(
                token or None,
                f"Unexpected keyword argument '{kw_name}' in {context_name}",
            )
        if kw_name in values:
            raise NovaError(
                token or None,
                f"Parameter '{kw_name}' given both positionally and by keyword in {context_name}",
            )
        values[kw_name] = kw_val

    # 4. Defaults + type checks
    result = {}
    for param in parameters:
        if param.name in values:
            arg_val = values[param.name]
        elif getattr(param, "is_compact", False):
            continue  # already set to []
        else:
            if param.default is not None:
                arg_val = interpreter.evaluate_expr(param.default, env)
            else:
                raise NovaError(
                    param,
                    f"Missing argument for parameter '{param.name}' in {context_name}",
                )
        if param.annotation_type:
            check_type(param.annotation_type, arg_val, param)
        result[param.name] = arg_val

    return result


class NovaClass:
    """
    Redesigned class system with:
    - Support for `const` properties (instance and static)
    - Proper encapsulation via private tracking + current_class_stack
    - Cleaner method/constructor binding using shared bind_parameters helper
    - Scope-aware environments for methods
    """

    def __init__(self, name, super_class, interpreter, env):
        self.name = name
        self.super_class = super_class  # NovaClass or Python type
        self.interpreter = interpreter
        self.env = env  # definition environment (lexical parent)
        self.static_members = {"name": name}  # runtime static values
        self._static_const = set()  # names of const static members
        self._private_properties = set()
        self._private_methods = set()
        self._const_properties = set()  # instance const property names
        self.instance_properties = {}  # name -> PropertyDefinition AST
        self.instance_methods = {}  # name -> MethodDefinition AST
        self.constructor_def = None

        # Determine the top-most Python root, if any
        self._python_root = self._find_python_root()

    def get_public_instance_members(self, instance):
        result = {}
        # Properties (skip private)
        for name, prop_def in self.instance_properties.items():
            if name not in self._private_properties:
                value = (
                    instance[name]
                    if isinstance(instance, dict)
                    else getattr(instance, name, None)
                )
                result[name] = value
        # Methods (skip private)
        for name, method_def in self.instance_methods.items():
            if name not in self._private_methods:
                value = (
                    instance[name]
                    if isinstance(instance, dict)
                    else getattr(instance, name, None)
                )
                result[name] = value
        return result

    def get_public_static_members(self):
        # Return a shallow copy; callers should not mutate const ones
        return dict(self.static_members)

    def _find_python_root(self):
        """Return the most foundational Python superclass, or None."""
        cls = self
        root = None
        while cls is not None:
            if isinstance(cls.super_class, type):
                root = cls.super_class
            cls = cls.super_class if isinstance(cls.super_class, NovaClass) else None
        return root

    def __call__(self, *args):
        return self.instantiate(args)

    # ------------------------------------------------------------------
    #  Unified instance creation
    # ------------------------------------------------------------------
    def instantiate(self, args):
        if self._python_root is not None:
            instance = self._python_root.__new__(self._python_root)
            setattr(instance, "__DefiningClass", self)
        else:
            instance = {}
            instance["__DefiningClass"] = self

        # Walk the NovaClass hierarchy from the root Python subclass down,
        # adding all Nova properties and methods.
        self._build_instance_layout(instance)

        # Run constructor chain (most derived first, super calls move up)
        self._run_constructor_chain(args, instance)
        return instance

    # ------------------------------------------------------------------
    #  Build layout: properties + methods in order (top-down)
    # ------------------------------------------------------------------
    def _build_instance_layout(self, instance):
        # Recursively build from superclass first (proper inheritance chain)
        if isinstance(self.super_class, NovaClass):
            self.super_class._build_instance_layout(instance)

        # Track const/private on the instance for runtime checks (dict instances)
        if isinstance(instance, dict):
            if "__const__" not in instance:
                instance["__const__"] = set()
            if "__private__" not in instance:
                instance["__private__"] = set()

        # Add properties
        for prop_name, prop_def in self.instance_properties.items():
            value = None
            if isinstance(prop_def, PropertyHandler):
                _env = Environment(self.env)
                _env.define("self", instance)
                _env.define("__DefiningClass", self)
                self.interpreter.execute_stmt(prop_def.setter, _env)
                self.interpreter.execute_stmt(prop_def.getter, _env)
                set_func = _env.get("set").value
                get_func = _env.get("get").value
                dprint("PropertyHandler:", prop_def)
                value = Proxy(set_func, get_func, instance, self.interpreter, self)
            elif prop_def.initializer:
                value = self.interpreter.evaluate_expr(prop_def.initializer, self.env)

            self._set_instance_attr(instance, prop_name, value)

            # Mark const / private for later assignment checks
            # PropertyHandler has neither is_const nor is_private; use getattr
            if isinstance(instance, dict):
                if getattr(prop_def, "is_const", False):
                    instance["__const__"].add(prop_name)
                    self._const_properties.add(prop_name)
                if getattr(prop_def, "is_private", False):
                    instance["__private__"].add(prop_name)

        # Add methods
        for method_name, method_def in self.instance_methods.items():
            bound = self._create_method(method_def, instance)
            self._set_instance_attr(instance, method_name, bound)
            if isinstance(instance, dict) and method_def.is_private:
                instance["__private__"].add(method_name)

    @staticmethod
    def _set_instance_attr(instance, name, value):
        if isinstance(instance, dict):
            instance[name] = value
        else:
            setattr(instance, name, value)

    # ------------------------------------------------------------------
    #  Method binding (with super support)
    # ------------------------------------------------------------------
    def _create_method(self, method_def, instance):
        interpreter = self.interpreter
        env = self.env
        super_class = self.super_class
        defining_cls = self  # capture for nested lambdas / private checks

        def bound_method(*method_args, **method_kwargs):
            # Push onto the *defining* interpreter's stack so that:
            # 1. private access checks inside the method body succeed, and
            # 2. any lambda created inside the method captures this class
            #    (critical when the method is invoked from a different Interpreter,
            #     e.g. after load()).
            interpreter.current_class_stack.append(defining_cls)
            try:
                method_env = Environment(env)
                method_env.define("self", instance)

                # Build 'super' object for method calls
                if super_class:
                    super_obj = {}
                    if isinstance(super_class, NovaClass):
                        for sm_name, sm_def in super_class.instance_methods.items():

                            def make_super_method(mdef=sm_def, sname=sm_name):
                                def super_call(*super_args, **super_kwargs):
                                    super_env = Environment(super_class.env)
                                    super_env.define("self", instance)
                                    # Shared binding helper
                                    bound = bind_parameters(
                                        mdef.parameters,
                                        super_args,
                                        super_kwargs,
                                        interpreter,
                                        super_class.env,
                                        context_name=f"super method '{sname}'",
                                        token=mdef,
                                    )
                                    for pname, pval in bound.items():
                                        super_env.define(pname, pval)
                                    result = interpreter.execute_block(
                                        mdef.body, super_env
                                    )
                                    if isinstance(result, ReturnFlow):
                                        return result.value
                                    return None

                                return super_call

                            super_obj[sm_name] = make_super_method()
                    elif isinstance(super_class, type):
                        # Expose Python superclass methods
                        for attr_name in dir(super_class):
                            if not attr_name.startswith("_"):
                                attr = getattr(super_class, attr_name, None)
                                if callable(attr):

                                    def python_super_call(
                                        *args, aname=attr_name, **kwargs
                                    ):
                                        method = getattr(super_class, aname)
                                        try:
                                            return method(instance, *args, **kwargs)
                                        except Exception as e:
                                            raise NovaError(
                                                None,
                                                f"Error calling super Python method '{aname}': {e}",
                                            )

                                    super_obj[attr_name] = python_super_call
                    method_env.define("Base", super_obj)

                # Bind method parameters using shared helper
                bound_values = bind_parameters(
                    method_def.parameters,
                    method_args,
                    method_kwargs,
                    interpreter,
                    env,
                    context_name=f"method '{method_def.name}'",
                    token=method_def,
                )
                for pname, pval in bound_values.items():
                    method_env.define(pname, pval)

                result = interpreter.execute_block(method_def.body, method_env)
                if isinstance(result, ReturnFlow):
                    return result.value
                return None
            finally:
                if interpreter.current_class_stack:
                    interpreter.current_class_stack.pop()

        bound_method.__DefiningClass = self  # store the class for access checks
        if method_def.is_private:
            bound_method.__is_private__ = True
        return bound_method

    # ------------------------------------------------------------------
    #  Constructor chain
    # ------------------------------------------------------------------
    def _run_constructor_chain(self, args, instance):
        """Start the chain from the most derived class."""
        if self.constructor_def:
            self._execute_constructor(self.constructor_def, args, instance)

    def _execute_constructor(self, constructor_def, args, instance):
        constructor_env = Environment(self.env)
        constructor_env.define("self", instance)

        # Define 'super' for constructor
        if self.super_class:
            if isinstance(self.super_class, NovaClass):
                parent = self.super_class

                def super_constructor_call(*super_args):
                    if parent.constructor_def:
                        parent._execute_constructor(
                            parent.constructor_def, super_args, instance
                        )

                constructor_env.define("Base", super_constructor_call)
            elif isinstance(self.super_class, type):
                parent_type = self.super_class

                def super_constructor_call(*super_args):
                    try:
                        parent_type.__init__(instance, *super_args)
                    except Exception as e:
                        raise ValueError(
                            f"Error calling superclass Python constructor: {e}",
                        )

                constructor_env.define("Base", super_constructor_call)
            else:
                raise ValueError(
                    f"Invalid superclass type: {type(self.super_class).__name__}",
                )

        # Bind constructor parameters via shared helper
        bound = bind_parameters(
            constructor_def.parameters,
            args,
            {},  # constructors currently positional-only from call site
            self.interpreter,
            self.env,
            context_name=f"constructor of '{self.name}'",
            token=constructor_def,
        )
        for pname, pval in bound.items():
            constructor_env.define(pname, pval)

        self.interpreter.execute_block(constructor_def.body, constructor_env)


BUILTIN_VAR_TYPES: dict[str, object] = {
    "string": str,
    "number": (int, float),
    "bool": bool,
    "function": Callable,
    "list": list,
    "int": int,
    "float": float,
}

BUILTIN_VAR_TYPES_INFER: dict[str, str] = {
    "str": "string",
    "bool": "bool",
    "function": "function",
    "list": "list",
    "int": "int",
    "float": "float",
}

CUSTOM_TYPES = {}  # Dictionary for custom types


def check_type(expected, value, token):
    if expected in ["any", None]:
        return
    # Handle array types: int[], string[][], list[], etc.
    if isinstance(expected, str) and expected.endswith("[]"):
        base_type = expected[:-2]
        if not isinstance(value, list):
            raise NovaError(
                token,
                f"Type mismatch: expected array type '{expected}', got {type(value).__name__}",
            )
        for elem in value:
            check_type(base_type, elem, token)
        return
    custom_type_definition = BUILTIN_VAR_TYPES.get(expected, None)
    if not custom_type_definition:
        custom_type_definition = CUSTOM_TYPES.get(expected)
        if custom_type_definition is None:
            raise NovaError(token, f"unknown type definition: {expected}")
        check_custom_type(expected, custom_type_definition, value, token)
    else:
        if not isinstance(value, custom_type_definition):
            raise NovaError(
                token,
                f"Type mismatch: expected {custom_type_definition}, got {type(value).__name__}",
            )


def check_custom_type(expected, custom_type_definition, value, token):
    if custom_type_definition:
        if not isinstance(value, dict):
            raise NovaError(
                token,
                f"Type mismatch: expected custom type '{expected}', got {type(value).__name__}",
            )
        missing = []

        # Collect missing properties
        for prop_def in custom_type_definition.properties:
            if prop_def.name not in value:
                missing.append(prop_def)

        if missing:
            missing_list = ""
            for f in missing:
                missing_list += f"\n- {f.name} ({f.type})"
            raise NovaError(
                token,
                f"Type mismatch: custom type '{expected}' is missing properties: {missing_list}",
            )

        # Now check each property type
        for prop_def in custom_type_definition.properties:
            check_type(prop_def.type, value[prop_def.name], token)


def opToName(op):
    def up(st):
        s = list(st)
        s[0] = s[0].upper()
        return "__" + "".join(s)

    matches = {
        "+": up("add"),
        "-=": up("setDec"),
        "+=": up("setInc"),
        "*=": up("setMul"),
        "-": up("sub"),
        "*": up("mul"),
        "/": up("div"),
        "%": up("mod"),
        "^": up("xor"),
        "<<": up("lshift"),
        ">>": up("rshift"),
        "==": up("eq"),
        "!=": up("ne"),
        "<": up("lt"),
        ">": up("gt"),
        "<=": up("le"),
        ">=": up("ge"),
    }
    return matches.get(op, None)


class Tokenizer:
    keywords = [
        "var",
        "const",
        "if",
        "else",
        "elseif",
        "end",
        "break",
        "continue",
        "func",
        "local",
        "return",
        "as",
        "export",
        "scope",
        "while",
        "until",
        "for",
        "do",
        "in",
        "test",
        "failed",
        "defer",
        "matches",
        "private",
        "strict",
        "default",
        "using",
        "def",
        "define",
        "enum",
        "assert",
        "class",
        "record",
        "inherits",
        "static",
        "new",
        "with",
        "unpack",
        "compact",
        "property",
    ]

    # --- Tokenization ---
    def __init__(self, source, file):
        tokens = []
        i = 0
        line = 1
        self.current = 0

        col = 1
        length = len(source)

        while i < length:
            char = source[i]
            start_col = col
            next_char = source[i + 1] if i + 1 < length else ""
            # --- Shebang support (must be at file start) ---
            if (
                line == 1
                and col == 1
                and char == "#"
                and i + 1 < length
                and source[i + 1] == "!"
            ):
                # Consume everything until newline or EOF
                while i < length and source[i] != "\n":
                    i += 1
                # If we stopped at a newline, consume it so the whitespace handler doesn't see it
                if i < length and source[i] == "\n":
                    i += 1
                    line += 1
                    col = 1
                # else: reached EOF without newline – that's fine, just stop
                continue
            # Skip whitespace.
            if char.isspace():
                if char == "\n":
                    line += 1
                    col = 1
                else:
                    col += 1
                i += 1
                continue

            # --- Comment support ---
            if char == "/" and next_char == "/":  # Single-line comment
                while i < length and source[i] != "\n":
                    i += 1
                continue
            if char == "/" and next_char == "*":  # Multi-line comment
                i += 2
                col += 2
                while i < length and not (
                    source[i] == "*" and i + 1 < length and source[i + 1] == "/"
                ):
                    if source[i] == "\n":
                        line += 1
                        col = 1
                    else:
                        col += 1
                    i += 1
                if i + 1 < length and source[i] == "*" and source[i + 1] == "/":
                    i += 2
                    col += 2
                else:
                    raise NovaError(
                        Token("error", "Unterminated comment", file, line, col),
                        "Unterminated multi-line comment",
                    )
                continue

            # --- Operators (prioritize longer matches) ---
            matched_operator = False
            two_char_ops = [
                "+=",
                ">>",
                "<<",
                "-=",
                "*=",
                "/=",
                "%=",
                "==",
                "!=",
                "<=",
                ">=",
                "&&",
                "||",
                "->",
                "=>",
                "**",
            ]
            current_two_char = char + next_char

            if current_two_char in two_char_ops:
                tokens.append(
                    Token("operator", current_two_char, file, line, start_col)
                )
                i += 2
                col += 2
                matched_operator = True
            else:
                # Single-character operators that might be part of two-char ops, or stand alone
                potential_single_ops = "=+-*/%^<>!.|&"
                if char in potential_single_ops:
                    tokens.append(Token("operator", char, file, line, start_col))
                    i += 1
                    col += 1
                    matched_operator = True
                else:
                    # Other single-character punctuation/operators
                    other_single_ops = "()[]{},:#$@"
                    if other_single_ops.find(char) != -1:  # Use find for string check
                        tokens.append(Token("operator", char, file, line, start_col))
                        i += 1
                        col += 1
                        matched_operator = True

            if matched_operator:
                continue
            # Numbers (decimal, hex, unicode, binary)

            if char.isdigit():

                def collect_digits(valid, allow_underscore=True):
                    nonlocal i, col

                    digits = ""

                    while i < length:
                        c = source[i]

                        if allow_underscore and c == "_":
                            i += 1
                            col += 1
                            continue

                        if c not in valid:
                            break

                        digits += c
                        i += 1
                        col += 1

                    if not digits:
                        raise NovaError(
                            Token("error", "Invalid numeric literal", file, line, col),
                            "Invalid numeric literal",
                        )
                    return digits

                def read_digits(valid, base, allow_underscore=True):
                    return int(collect_digits(valid, allow_underscore), base)

                # prefixed numbers
                if i + 1 < length and source[i] == "0":
                    prefix = source[i + 1].lower()

                    if prefix in ("x", "u", "b"):
                        i += 2
                        col += 2

                        if prefix in ("x", "u"):
                            value = read_digits("0123456789abcdefABCDEF", 16)

                            if prefix == "x":
                                tokens.append(
                                    Token("number", value, file, line, start_col)
                                )
                            else:
                                tokens.append(
                                    Token("string", chr(value), file, line, start_col)
                                )

                        elif prefix == "b":
                            bits = ""
                            while i < length and source[i] in "01":
                                bits += source[i]
                                i += 1
                                col += 1

                            tokens.append(
                                Token("number", int(bits, 2), file, line, start_col)
                            )

                        continue

                # decimal / float (with underscores, no trailing dot)
                int_part = collect_digits("0123456789")  # integer part mandatory
                if source[i].isalpha():
                    raise NovaError(
                        Token("error", "Invalid integer", file, line, col),
                        "This is not a valid integer",
                    )

                if i < length and source[i] == ".":
                    i += 1
                    col += 1
                    # fractional part must contain at least one digit (underscores allowed but must resolve to digits)
                    try:
                        frac_part = collect_digits("0123456789")
                    except NovaError:
                        # collect_digits raised because no digits found -> that's a trailing dot
                        raise NovaError(
                            Token("error", "Invalid numeric literal", file, line, col),
                            "Trailing dot in numeric literal",
                        )
                    if source[i].isalpha():
                        raise NovaError(
                            Token("error", "Invalid float", file, line, col),
                            "This is not a valid float",
                        )

                    num_str = f"{int_part}.{frac_part}"
                    tokens.append(
                        Token("number", float(num_str), file, line, start_col)
                    )
                else:
                    # plain integer
                    tokens.append(
                        Token("number", int(int_part, 10), file, line, start_col)
                    )
                continue
            if char in ['"', "'", "`"]:
                quote = char
                i += 1
                value = []

                while i < length:
                    c = source[i]

                    if c == quote:
                        break

                    if c == "\\":
                        i += 1
                        if i >= length:
                            raise NovaError(
                                Token("error", "Unterminated escape", file, line, col),
                                "Unterminated escape sequence",
                            )

                        esc = source[i]

                        # Check for octal escape (digit 0-7)
                        if source[i] in "01234567":
                            # Read up to 3 octal digits
                            oct_digits = []
                            for _ in range(3):
                                if i < length and source[i] in "01234567":
                                    oct_digits.append(source[i])
                                    i += 1
                                else:
                                    break
                            octal_str = "".join(oct_digits)
                            value.append(chr(int(octal_str, 8)))
                            # Continue the outer loop – do not increment i again here
                            continue  # skip the final i+=1 of the loop

                        escapes = {
                            "n": "\n",
                            "t": "\t",
                            "r": "\r",
                            "\\": "\\",
                            '"': '"',
                            "'": "'",
                        }
                        if quote != "`":
                            if esc not in escapes:
                                raise NovaError(
                                    Token(
                                        "error",
                                        "Invalid escape sequence",
                                        file,
                                        line,
                                        col,
                                    ),
                                    f"Invalid escape sequence \\{esc}",
                                )
                            value.append(escapes[esc])
                        else:
                            value.append("\\" + esc)
                    else:
                        value.append(c)

                    if c == "\n":
                        line += 1
                        col = 1
                    else:
                        col += 1

                    i += 1

                if i >= length or source[i] != quote:
                    raise NovaError(
                        Token("error", "Unterminated string", file, line, col),
                        "Unterminated string literal",
                    )

                i += 1
                col += 1
                if quote == "`":
                    tokens.append(
                        Token("string", "".join(value), file, line, start_col)
                    )
                else:
                    tokens.append(
                        Token("string", r"".join(value), file, line, start_col)
                    )
                continue

            # Identifiers, keywords, booleans.
            if char.isalpha() or char == "_":
                id_str = ""
                while i < length and (source[i].isalnum() or source[i] == "_"):
                    id_str += source[i]
                    i += 1
                    col += 1
                if id_str in self.keywords:
                    tokens.append(Token("keyword", id_str, file, line, start_col))
                elif id_str == "between":
                    tokens.append(Token("operator", "between", file, line, start_col))
                else:
                    tokens.append(Token("identifier", id_str, file, line, start_col))
                continue

            raise NovaError(
                Token("error", char, file, line, col), f"Unexpected character: {char}"
            )

        self.tokens = tokens

    # --- Token Parser Helpers ---
    def get_next_token(self):
        return self.tokens[self.current] if self.current < len(self.tokens) else None

    def consume_token(self):
        token = self.get_next_token()
        if token:
            self.current += 1
        return token

    def expect_type(self, type_str):
        token = self.get_next_token()
        if not token or token.type != type_str:
            raise NovaError(
                token,
                f"Expected token type {type_str}, got {token if token else 'EOF'}",
            )
        return token

    def expect_token(self, value):
        token = self.get_next_token()
        if not token:
            last_token = (
                self.tokens[self.current - 1]
                if self.current > 0
                else Token("EOF", "EOF", self.file, 1, 1)
            )
            raise NovaError(last_token, f"Expected token '{value}', got EOF")
        if token.value != value:
            raise NovaError(token, f"Expected token '{value}', got {token.value}")
        return token

    def _consume_array_brackets(self, base_type: str) -> str:
        """After consuming a type identifier, eat any trailing [] pairs.

        Examples: int -> int[]  |  string -> string[][]
        """
        result = base_type
        while (
            self.current < len(self.tokens)
            and self.tokens[self.current].value == "["
            and self.current + 1 < len(self.tokens)
            and self.tokens[self.current + 1].value == "]"
        ):
            self.consume_token()  # [
            self.consume_token()  # ]
            result += "[]"
        return result

    def parse_argument_list(self):
        """Parse a comma-separated argument list (already inside the parentheses).

        Supports three forms:
          - unpack <expr>          → ExplodeExpr
          - identifier : <expr>    → NamedArg
          - <expr>                 → normal positional argument
        """
        args = []
        if self.get_next_token() and self.get_next_token().value != ")":
            while True:
                nt = self.get_next_token()
                if (
                    nt
                    and nt.type == "identifier"
                    and self.current + 1 < len(self.tokens)
                    and self.tokens[self.current + 1].value == ":"
                ):
                    # Named argument: name: value
                    name_tok = self.consume_token()
                    self.consume_token()  # consume ':'
                    value = self.parse_expression()
                    args.append(
                        NamedArg(
                            name_tok.value,
                            value,
                            name_tok.file,
                            name_tok.line,
                            name_tok.column,
                        )
                    )
                elif nt and nt.value == "unpack":
                    token = self.consume_token()
                    args.append(
                        ExplodeExpr(
                            self.parse_expression(),
                            token.file,
                            token.line,
                            token.column,
                        )
                    )
                else:
                    args.append(self.parse_expression())

                if self.get_next_token() and self.get_next_token().value == ",":
                    self.consume_token()
                else:
                    break
        return args

    def build_fn(self, token):
        self.consume_token()
        func_name_token = self.expect_type("identifier")
        func_name = func_name_token.value
        self.consume_token()
        self.expect_token("(")
        self.consume_token()

        func_parameters = []
        if self.get_next_token() and self.get_next_token().value != ")":
            while True:
                is_compact = False
                if self.get_next_token() and self.get_next_token().value == "compact":
                    self.consume_token()  # consume 'compact'
                    is_compact = True

                param_token = self.expect_type("identifier")
                param_name = param_token.value
                self.consume_token()
                annotation_type = None
                if self.get_next_token() and self.get_next_token().type == "identifier":
                    if is_compact:
                        raise NovaError(token, "'compact' does not support this")
                    type_token = self.get_next_token()
                    if (
                        type_token.value in BUILTIN_VAR_TYPES
                        or type_token.value in CUSTOM_TYPES
                    ):
                        annotation_type = type_token.value
                        self.consume_token()
                        annotation_type = self._consume_array_brackets(annotation_type)

                default_expr = None
                if self.get_next_token() and self.get_next_token().value == "=":
                    if is_compact:
                        raise NovaError(token, "'compact' does not support this")
                    self.consume_token()
                    default_expr = self.parse_expression()
                    if annotation_type:
                        raise NovaError(
                            token,
                            "Cannot have both explicit type annotation and a default value. Consider removing the type annotation to allow type inference from the default value.",
                        )

                    if default_expr.type == "Literal":
                        inferred_type = type(default_expr.value).__name__
                        annotation_type = BUILTIN_VAR_TYPES_INFER.get(
                            inferred_type, None
                        )
                    else:
                        annotation_type = None

                func_parameters.append(
                    Parameter(
                        param_name,
                        annotation_type,
                        default_expr,
                        is_compact,
                        param_token.file,
                        param_token.line,
                        param_token.column,
                        param_token.type,
                        param_token.value,
                    )
                )

                if self.get_next_token() and self.get_next_token().value == ",":
                    self.consume_token()
                else:
                    break

        self.expect_token(")")
        self.consume_token()
        if self.get_next_token().value == "=":
            t = self.consume_token()
            func_body = [ReturnStmt(self.parse_expression(), t.file, t.line, t.column)]
        else:
            func_body = self.parse_block_until(["end"])
            self.expect_token("end")
            self.consume_token()

        return FuncDecl(
            func_name,
            func_parameters,
            func_body,
            token.file,
            token.line,
            token.column,
        )

    # --- Parsing Helpers ---
    def parse_block_until(self, terminators=None):
        if terminators == None:
            terminators = []
        statements = []
        while self.current < len(self.tokens):
            token = self.get_next_token()
            if not token:
                last_token = (
                    self.tokens[self.current - 1]
                    if self.current > 0
                    else Token("EOF", "EOF", self.file, 1, 1)
                )
                raise NovaError(last_token, "Unexpected end of input")
            if (token.type == "keyword" and token.value in terminators) or (
                token.type == "operator" and token.value in terminators
            ):
                break
            stmt = self._parse_statement()
            # CustomTypeDeclStmt returns None
            if stmt:
                statements.append(stmt)
        return statements

    def parse_block(self):
        return self.parse_block_until()

    def consume_expected(self, type_str):
        self.expect_type(type_str)
        return self.consume_token()

    def parse_class_definition(self):
        class_token = self.consume_token()  # consume 'class'
        name_token = self.expect_type("identifier")
        name = name_token.value
        self.consume_token()  # consume class name

        superclass_name = None
        if self.get_next_token() and self.get_next_token().value == "inherits":
            self.consume_token()  # consume 'inherits'

            # --- MODIFIED LOGIC START ---
            # Parse the fully qualified superclass name, e.g., "module.sub.ClassName"
            name_parts = []

            name_part_token = self.expect_type("identifier")
            name_parts.append(name_part_token.value)
            self.consume_token()  # Consume the first identifier

            while self.get_next_token() and self.get_next_token().value == ".":
                self.consume_token()  # Consume the dot
                name_part_token = self.expect_type("identifier")
                name_parts.append(name_part_token.value)
                self.consume_token()  # Consume the next identifier

            superclass_name = ".".join(name_parts)
            # --- MODIFIED LOGIC END ---

        body = []
        while self.get_next_token() and self.get_next_token().value != "end":
            member_token = self.get_next_token()
            is_static = False
            is_private = False
            is_const = False
            # print(member_token)
            while True:
                member_token = self.get_next_token()

                if member_token.type == "keyword" and member_token.value == "static":
                    self.consume_token()  # consume 'static'
                    is_static = True

                elif member_token.type == "keyword" and member_token.value == "private":
                    if is_static:
                        raise NovaError(
                            member_token, "Cannot have a private static method/variable"
                        )
                    self.consume_token()  # consume 'private'
                    is_private = True
                elif member_token.type == "keyword" and member_token.value == "const":
                    self.consume_token()  # consume 'const'
                    is_const = True
                else:
                    break

            if (
                self.get_next_token()
                and self.get_next_token().type == "keyword"
                and self.get_next_token().value == "var"
            ):
                # Property Definition
                self.consume_token()  # consume 'var'
                prop_name_token = self.expect_type("identifier")
                prop_name = prop_name_token.value
                self.consume_token()

                type_annotation = None
                if (
                    self.get_next_token()
                    and self.get_next_token().type == "operator"
                    and self.get_next_token().value == ":"
                ):
                    self.consume_token()  # consume ':'
                    type_token = self.expect_type("identifier")
                    self.consume_token()
                    type_annotation = self._consume_array_brackets(type_token.value)

                initializer = None
                if (
                    self.get_next_token()
                    and self.get_next_token().type == "operator"
                    and self.get_next_token().value == "="
                ):
                    self.consume_token()  # consume '='
                    initializer = self.parse_expression()
                body.append(
                    PropertyDefinition(
                        prop_name,
                        type_annotation,
                        initializer,
                        is_static,
                        is_private,
                        is_const,
                        prop_name_token.file,
                        prop_name_token.line,
                        prop_name_token.column,
                    )
                )
            elif (
                self.get_next_token()
                and self.get_next_token().type == "keyword"
                and self.get_next_token().value == "property"
            ):
                if is_private:
                    raise NovaError(self.get_next_token(), "property cannot be private")
                self.consume_token()
                _name = self.consume_expected("identifier")
                setter = None
                getter = None
                p = [self._parse_statement(), self._parse_statement()]
                for node in p:
                    if node.type != "FuncDecl":
                        raise NovaError(
                            node, "only functions are allowed in this context"
                        )
                    if node.name == "set":
                        setter = node
                    elif node.name == "get":
                        getter = node
                    else:
                        raise NovaError(node, "only 'set' and 'get' are allowed")

                if not setter or not getter:
                    raise NovaError(node, "Expected a getter and setter")
                body.append(
                    PropertyHandler(
                        _name.value,
                        getter,
                        setter,
                        _name.file,
                        _name.line,
                        _name.column,
                    )
                )
            elif (
                self.get_next_token()
                and self.get_next_token().type == "keyword"
                and self.get_next_token().value == "func"
            ):
                # Method Definition
                is_constructor = False
                method_token = self.get_next_token()
                func = self.build_fn(method_token)
                if func.name == "init":
                    is_constructor = True

                body.append(
                    MethodDefinition(
                        func.name,
                        func.parameters,
                        func.body,
                        is_static,
                        is_constructor,
                        is_private,
                        method_token.file,
                        method_token.line,
                        method_token.column,
                    )
                )
            else:
                raise NovaError(
                    member_token,
                    f"Unexpected token in class body: {member_token.value}",
                )

        # if not has_initializer:
        #     raise NovaError(
        #         class_token, "this class has no `init` initializer function"
        #     )

        self.expect_token("end")
        self.consume_token()  # consume 'end'

        return ClassDefinition(
            name,
            superclass_name,
            body,
            class_token.file,
            class_token.line,
            class_token.column,
        )

    def parse_object_definition(self):
        # This mirrors 'class' parsing but returns an ObjectDecl node
        token = self.consume_token()  # consume 'object'

        name_token = self.expect_type("identifier")
        name = name_token.value
        self.consume_token()

        body = self.parse_block_until(["end"])
        self.expect_token("end")
        self.consume_token()

        return ObjectDecl(name, body, token.file, token.line, token.column)

    # --- Parsing Statements and Expressions ---
    def _parse_statement(self):
        return self.parse_statement()

    def parse_statement(self):
        token = self.get_next_token()
        if not token:
            last_token = (
                self.tokens[self.current - 1]
                if self.current > 0
                else Token("EOF", "EOF", self.file, 1, 1)
            )
            raise NovaError(last_token, "Unexpected end of input")

        return self._builtinHandler(token)

    def _builtinHandler(self, token):
        if token.type == "keyword":
            if token.value == "defer":
                self.consume_token()
                defer_body = self.parse_block_until(["end"])
                self.expect_token("end")
                self.consume_token()
                return DeferStmt(defer_body, token.file, token.line, token.column)
            elif token.value == "assert":
                self.consume_token()
                assert_expr = self.parse_expression()
                t = None
                self.expect_token(",")
                self.consume_token()
                self.expect_type("string")
                t = self.consume_token().value
                return AssertStmt(assert_expr, t, token.file, token.line, token.column)
            elif token.value == "define":
                self.consume_token()  # consume 'define'
                custom_type_name_token = self.expect_type("identifier")
                custom_type_name = custom_type_name_token.value
                copyFroms = []
                self.consume_token()  # consume type name
                if self.get_next_token().value == ":":
                    self.consume_token()
                    self.expect_token("[")
                    self.consume_token()  # [
                    while self.get_next_token() and self.get_next_token().value != "]":
                        self.expect_type("identifier")
                        copyFroms.append(self.consume_token())

                    self.expect_token("]")
                    self.consume_token()

                self.expect_token("=")
                self.consume_token()  # consume '='

                self.expect_token("{")
                self.consume_token()  # consume '{'

                properties = []
                while self.get_next_token() and self.get_next_token().value != "}":
                    prop_name_token = self.expect_type("identifier")
                    prop_name = prop_name_token.value
                    self.consume_token()  # consume property name

                    self.expect_token(":")
                    self.consume_token()  # consume ':'

                    prop_type_token = self.expect_type("identifier")
                    prop_type = prop_type_token.value
                    self.consume_token()  # consume property type

                    properties.append(CustomTypeProperty(prop_name, prop_type))

                    if self.get_next_token() and self.get_next_token().value == ",":
                        self.consume_token()  # consume ','
                    else:
                        break

                self.expect_token("}")
                self.consume_token()  # consume '}'
                # for k in properties:
                #    print("before",k)
                for t in copyFroms:
                    # print(t)
                    tt = CUSTOM_TYPES.get(t.value)
                    if tt is None:
                        raise NovaError(t, f"Unknown type '{t.value}'")
                    for k in tt.properties:
                        if not k in properties:
                            properties.append(k)
                #        print("during", k)
                # properties.extend(tt.properties)
                # for k in properties:
                #    print("after", k)
                custom_type_stmt = CustomTypeDeclStmt(
                    custom_type_name, properties, token.file, token.line, token.column
                )
                CUSTOM_TYPES[custom_type_stmt.name] = CustomType(
                    custom_type_stmt.name,
                    custom_type_stmt.definition,
                    custom_type_stmt.file,
                    custom_type_stmt.line,
                    custom_type_stmt.column,
                )
                # print(CUSTOM_TYPES)

                return None
            elif token.value == "var":
                self.consume_token()
                var_name_token = self.expect_type("identifier")
                var_name = var_name_token.value
                self.consume_token()

                var_annotation_type = None
                if self.get_next_token() and self.get_next_token().type == "identifier":
                    if (
                        self.get_next_token().value in BUILTIN_VAR_TYPES
                        or self.get_next_token().value in CUSTOM_TYPES
                    ):  # Parentheses for clarity
                        var_annotation_type = self.get_next_token().value
                        self.consume_token()
                        var_annotation_type = self._consume_array_brackets(
                            var_annotation_type
                        )
                    else:
                        raise NovaError(
                            self.get_next_token(),
                            f"not a type: {self.get_next_token()}\n\ntypes: {BUILTIN_VAR_TYPES = }\n{CUSTOM_TYPES = }",
                        )

                self.expect_token("=")
                self.consume_token()
                var_initializer = self.parse_expression()
                return VarDeclStmt(
                    var_name,
                    var_annotation_type,
                    var_initializer,
                    token.file,
                    token.line,
                    token.column,
                )

            elif token.value == "const":
                self.consume_token()
                var_name_token = self.expect_type("identifier")
                var_name = var_name_token.value
                self.consume_token()

                var_annotation_type = None
                if self.get_next_token() and self.get_next_token().type == "identifier":
                    if (
                        self.get_next_token().value in BUILTIN_VAR_TYPES
                        or self.get_next_token().value in CUSTOM_TYPES
                    ):  # Parentheses for clarity
                        var_annotation_type = self.get_next_token().value
                        self.consume_token()
                        var_annotation_type = self._consume_array_brackets(
                            var_annotation_type
                        )
                    else:
                        raise NovaError(
                            self.get_next_token(),
                            f"not a type: {self.get_next_token()}\n\ntypes: {BUILTIN_VAR_TYPES = }\n{CUSTOM_TYPES = }",
                        )

                self.expect_token("=")
                self.consume_token()
                var_initializer = self.parse_expression()
                return ConstDeclStmt(
                    var_name,
                    var_annotation_type,
                    var_initializer,
                    token.file,
                    token.line,
                    token.column,
                )

            elif token.value == "matches":
                self.consume_token()
                strict = False
                if (
                    self.get_next_token()
                    and self.get_next_token().type == "keyword"
                    and self.get_next_token().value == "strict"
                ):
                    self.consume_token()
                    strict = True
                switch_expr = self.parse_expression()
                cases = []
                self.expect_token("do")
                self.consume_token()
                while (
                    self.get_next_token()
                    # and not self.get_next_token().type == "keyword"
                    and not self.get_next_token().value in ["default", "end"]
                ):
                    case_expr = self.parse_expression()
                    if self.get_next_token().value == ",":
                        self.consume_token()
                        case_expr = [case_expr]
                        while (
                            self.get_next_token()
                            # and not self.get_next_token().type == "keyword"
                            and not self.get_next_token().value == ":"
                        ):
                            case_expr.append(self.parse_expression())
                            if (
                                self.get_next_token()
                                # and not self.get_next_token().type == "keyword"
                                and self.get_next_token().value == ","
                            ):
                                self.expect_token(",")
                                self.consume_token()
                    self.expect_token(":")
                    self.consume_token()
                    case_body = self.parse_block_until(["end"])
                    self.expect_token("end")
                    self.consume_token()
                    if isinstance(case_expr, list):
                        for cc in case_expr:
                            c = Case(cc, case_body)
                            cases.append(c)
                    else:
                        c = Case(case_expr, case_body)
                        cases.append(c)

                if (
                    self.get_next_token()
                    and self.get_next_token().type == "keyword"
                    and self.get_next_token().value == "default"
                ):
                    if strict:
                        raise NovaError(
                            self.consume_token(),
                            "Cannot use 'default' when strict is enabled",
                        )
                    self.consume_token()
                    default_body = self.parse_block_until(["end"])
                    self.expect_token("end")
                    self.consume_token()
                    cases.append(Case(None, default_body))  # None for default case_expr
                self.expect_token("end")
                self.consume_token()
                return SwitchStmt(
                    switch_expr, cases, strict, token.file, token.line, token.column
                )

            elif token.value == "using":
                self.consume_token()
                if self.get_next_token().value in ["[", "{"]:
                    # print(values, self.get_next_token())
                    # print(self.get_next_token())
                    token = self.get_next_token()
                    if token.value == "[":
                        self.consume_token()
                        values = []
                        while (
                            self.get_next_token() and self.get_next_token().value != "]"
                        ):
                            # Parse the fully qualified name, e.g., "module.sub.name"
                            name_parts = []
                            name_part_token = self.expect_type("identifier")
                            name_parts.append(name_part_token.value)
                            self.consume_token()  # Consume the first identifier

                            while (
                                self.get_next_token()
                                and self.get_next_token().value == "."
                            ):
                                self.consume_token()  # Consume the dot
                                name_part_token = self.expect_type("identifier")
                                name_parts.append(name_part_token.value)
                                self.consume_token()  # Consume the next identifier

                            full_name = ".".join(name_parts)
                            values.append(full_name)
                            if (
                                self.get_next_token()
                                and self.get_next_token().value == ","
                            ):
                                self.consume_token()
                            else:
                                break
                        self.expect_token("]")
                        self.consume_token()
                        return UsingStmt(values, token.file, token.line, token.column)
                    else:
                        values = self.parse_expression()
                        return UsingStmt(values, token.file, token.line, token.column)
                else:
                    expr = self.parse_expression()
                    return UsingStmt(expr, token.file, token.line, token.column)

            elif token.value == "test":
                self.consume_token()
                try_block = self.parse_block_until(["failed"])
                self.expect_token("failed")
                self.consume_token()
                error_var_token = self.expect_type("identifier")
                error_var = error_var_token.value
                self.consume_token()
                catch_block = self.parse_block_until(["end"])
                self.expect_token("end")
                self.consume_token()
                return TryStmt(
                    try_block,
                    error_var,
                    catch_block,
                    token.file,
                    token.line,
                    token.column,
                )

            elif token.value == "with":
                self.consume_token()
                expr = self.parse_expression()
                self.expect_token("as")
                self.consume_token()
                alias = self.expect_type("identifier").value
                self.consume_token()
                self.expect_token("do")
                self.consume_token()
                body = self.parse_block_until(["end"])
                self.expect_token("end")
                self.consume_token()
                return WithStmt(expr, alias, body, token.file, token.line, token.column)

            elif token.value == "for":
                self.consume_token()
                for_var_token = self.expect_type("identifier")
                for_variable = for_var_token.value
                self.consume_token()
                if self.get_next_token() and self.get_next_token().value == "in":
                    self.expect_token("in")
                    self.consume_token()
                    for_each_list_expr = self.parse_expression()
                    self.expect_token("do")
                    self.consume_token()
                    for_each_body = self.parse_block_until(["end"])
                    self.expect_token("end")
                    self.consume_token()
                    return ForEachStmt(
                        for_variable,
                        for_each_list_expr,
                        for_each_body,
                        token.file,
                        token.line,
                        token.column,
                    )
                else:
                    self.expect_token("=")
                    self.consume_token()
                    for_start_expr = self.parse_expression()
                    self.expect_token(",")
                    self.consume_token()
                    for_end_expr = self.parse_expression()
                    for_step_expr = None
                    if self.get_next_token() and self.get_next_token().value == ",":
                        self.consume_token()
                        for_step_expr = self.parse_expression()
                    self.expect_token("do")
                    self.consume_token()
                    for_body = self.parse_block_until(["end"])
                    self.expect_token("end")
                    self.consume_token()
                    return ForStmt(
                        for_variable,
                        for_start_expr,
                        for_end_expr,
                        for_step_expr,
                        for_body,
                        token.file,
                        token.line,
                        token.column,
                    )

            elif token.value == "while":
                self.consume_token()
                while_condition = self.parse_expression()
                while_body = self.parse_block_until(["end"])
                self.expect_token("end")
                self.consume_token()
                return WhileStmt(
                    while_condition, while_body, token.file, token.line, token.column
                )
            elif token.value == "until":
                self.consume_token()
                while_condition = self.parse_expression()
                self.expect_token("do")
                self.consume_token()
                while_body = self.parse_block_until(["end"])
                self.expect_token("end")
                self.consume_token()
                return UntilStmt(
                    while_condition, while_body, token.file, token.line, token.column
                )

            elif token.value == "break":
                self.consume_token()
                return BreakStmt(token.file, token.line, token.column)

            elif token.value == "continue":
                self.consume_token()
                return ContinueStmt(token.file, token.line, token.column)
            elif token.value == "export":
                self.consume_token()  # consume 'export'
                inner_stmt = (
                    self.parse_statement()
                )  # parse the declaration or expression that follows
                return ExportStmt(inner_stmt, token.file, token.line, token.column)
            elif token.value == "if":
                self.consume_token()
                if_condition = self.parse_expression()
                if if_condition.type in ["AssignmentExpr"]:
                    raise NovaError(
                        if_condition, "you cannot do assinment ops in 'if' condition"
                    )
                then_block = self.parse_block_until(["else", "elseif", "end"])

                else_if_blocks = []
                else_block = None

                while (
                    self.get_next_token()
                    and self.get_next_token().type == "keyword"
                    and self.get_next_token().value == "elseif"
                ):
                    self.consume_token()
                    elseif_condition = self.parse_expression()
                    if elseif_condition.type in ["AssignmentExpr"]:
                        raise NovaError(
                            if_condition,
                            "you cannot do assinment ops in 'elseif' condition",
                        )
                    elseif_body = self.parse_block_until(["else", "elseif", "end"])
                    else_if_blocks.append(
                        {"condition": elseif_condition, "body": elseif_body}
                    )

                if (
                    self.get_next_token()
                    and self.get_next_token().type == "keyword"
                    and self.get_next_token().value == "else"
                ):
                    self.consume_token()
                    else_block = self.parse_block_until(["end"])

                self.expect_token("end")
                self.consume_token()

                return IfStmt(
                    if_condition,
                    then_block,
                    else_block,
                    else_if_blocks if else_if_blocks else None,
                    token.file,
                    token.line,
                    token.column,
                )

            elif token.value == "scope":
                self.consume_token()
                namespace_name_token = self.expect_type("identifier")
                namespace_name = namespace_name_token.value
                self.consume_token()
                namespace_body = self.parse_block_until(["end"])
                self.expect_token("end")
                self.consume_token()
                return ScopeStmt(
                    namespace_name, namespace_body, token.file, token.line, token.column
                )

            elif token.value == "func":
                return self.build_fn(token)
            elif token.value == "local":
                self.consume_token()
                return LocalFuncDecl(
                    self.build_fn(self.get_next_token()),
                    token.file,
                    token.line,
                    token.column,
                )
            elif token.value == "return":
                self.consume_token()
                return_expression = None
                next_token = self.get_next_token()
                if next_token is not None:
                    # Only parse an expression if the next token can possibly start one.
                    # It must either be a non-keyword, or be one of the keywords that can begin an expression.
                    if not (
                        next_token.type == "keyword"
                        and next_token.value not in ("new", "def", "enum")
                    ):
                        return_expression = self.parse_expression()
                return ReturnStmt(
                    return_expression, token.file, token.line, token.column
                )
            elif token.value == "class":
                return self.parse_class_definition()
            elif token.value == "record":
                return self.parse_object_definition()

        # --- Expression statement (fallback) ---
        expr = self.parse_expression()
        return ExpressionStmt(expr, token.file, token.line, token.column)

    # --- Expression Parsing (Recursive Descent) ---
    def parse_expression(self):
        return self.parse_assignment()

    def parse_assignment(self):
        expr = self.parse_logical_or()  # This is the left-hand side of the assignment
        next_token = self.get_next_token()

        # Check if the next token is an assignment operator (simple or compound)
        if (
            next_token
            and next_token.type == "operator"
            and next_token.value in ["=", "+=", "-=", "*=", "/=", "%="]
        ):
            assignment_op_token = (
                self.consume_token()
            )  # Consume the assignment operator token
            value_expr = (
                self.parse_assignment()
            )  # Recursively parse the right-hand side

            # Ensure the target is something assignable
            if not isinstance(
                expr, (Identifier, PropertyAccess, ArrayAccess, ArrayLiteral)
            ):
                raise NovaError(
                    expr, f"Invalid assignment target: Cannot assign to {expr.type}"
                )

            # Create the AssignmentExpr AST node
            return AssignmentExpr(
                expr,
                value_expr,
                assignment_op_token.value,
                assignment_op_token.file,
                assignment_op_token.line,
                assignment_op_token.column,
            )
        return expr  # If no assignment operator, it's just a logical OR expression

    def parse_logical_or(self):
        expr = self.parse_logical_and()
        while (
            self.get_next_token()
            and self.get_next_token().type == "operator"
            and self.get_next_token().value == "||"
        ):
            operator_token = self.consume_token()
            operator = operator_token.value
            right = self.parse_logical_and()
            # Corrected: operator_token.token.line should be operator_token.line
            expr = BinaryExpr(
                operator,
                expr,
                right,
                operator_token.file,
                operator_token.line,
                operator_token.column,
            )
        return expr

    def parse_logical_and(self):
        expr = self.parse_pipeFn()
        while (
            self.get_next_token()
            and self.get_next_token().type == "operator"
            and self.get_next_token().value == "&&"
        ):
            operator_token = self.consume_token()
            operator = operator_token.value
            right = self.parse_pipeFn()
            expr = BinaryExpr(
                operator,
                expr,
                right,
                operator_token.file,
                operator_token.line,
                operator_token.column,
            )
        return expr

    def parse_pipeFn(self):
        expr = self.parse_equality()
        while (
            self.get_next_token()
            and self.get_next_token().type == "operator"
            and self.get_next_token().value in ["->", "=>"]
        ):
            operator_token = self.consume_token()
            operator = operator_token.value
            right = self.parse_equality()
            c = PipeExpr if operator == "->" else MapExpr
            expr = c(
                expr,
                right,
                operator_token.file,
                operator_token.line,
                operator_token.column,
            )
        return expr

    def parse_equality(self):
        expr = self.parse_comparison()
        while (
            self.get_next_token()
            and self.get_next_token().type == "operator"
            and self.get_next_token().value in ["==", "!="]
        ):
            operator_token = self.consume_token()
            operator = operator_token.value
            right = self.parse_comparison()
            expr = BinaryExpr(
                operator,
                expr,
                right,
                operator_token.file,
                operator_token.line,
                operator_token.column,
            )
        return expr

    def parse_comparison(self):
        expr = self.parse_term()
        while (
            self.get_next_token()
            and self.get_next_token().type == "operator"
            and self.get_next_token().value
            in ["<", "<=", ">", ">=", "<<", ">>", "|", "&", "between"]
        ):
            operator_token = self.consume_token()
            operator = operator_token.value
            right = self.parse_term()
            expr = BinaryExpr(
                operator,
                expr,
                right,
                operator_token.file,
                operator_token.line,
                operator_token.column,
            )
        return expr

    def parse_term(self):
        expr = self.parse_factor()
        while (
            self.get_next_token()
            and self.get_next_token().type == "operator"
            and self.get_next_token().value in ["+", "-"]
        ):
            operator_token = self.consume_token()
            operator = operator_token.value
            right = self.parse_factor()
            expr = BinaryExpr(
                operator,
                expr,
                right,
                operator_token.file,
                operator_token.line,
                operator_token.column,
            )
        return expr

    def parse_factor(self):
        expr = self.parse_unary()
        while (
            self.get_next_token()
            and self.get_next_token().type == "operator"
            and self.get_next_token().value in ["*", "**", "/", "%", "^"]
        ):
            operator_token = self.consume_token()
            operator = operator_token.value
            right = self.parse_unary()
            expr = BinaryExpr(
                operator,
                expr,
                right,
                operator_token.file,
                operator_token.line,
                operator_token.column,
            )
        return expr

    def parse_unary(self):
        if (
            self.get_next_token()
            and self.get_next_token().type == "operator"
            and self.get_next_token().value in ["-", "!", "#"]
        ):
            operator_token = self.consume_token()
            operator = operator_token.value
            right = self.parse_unary()
            return UnaryExpr(
                operator,
                right,
                operator_token.file,
                operator_token.line,
                operator_token.column,
            )
        return self.parse_call_member_expression()

    def parse_call_member_expression(self):
        expr = self.parse_primary()

        while True:
            next_token = self.get_next_token()
            if not next_token:
                break

            if next_token.value == ".":
                dot_token = self.consume_token()  # consume "."
                prop_token = self.expect_type("identifier")
                prop_name = prop_token.value
                self.consume_token()  # consume identifier

                # Check for method call
                if self.get_next_token() and self.get_next_token().value == "(":
                    self.consume_token()  # consume "("
                    args = self.parse_argument_list()
                    self.expect_token(")")
                    self.consume_token()  # consume ")"
                    expr = MethodCall(
                        expr,
                        prop_name,
                        args,
                        prop_token.file,
                        prop_token.line,
                        prop_token.column,
                    )
                else:
                    # Plain property access
                    expr = PropertyAccess(
                        expr,
                        prop_name,
                        prop_token.file,
                        prop_token.line,
                        prop_token.column,
                    )
            elif next_token.value == "[":
                bracket_token = self.consume_token()  # consume "["
                index_expr = self.parse_expression()
                self.expect_token("]")
                self.consume_token()  # consume "]"
                expr = ArrayAccess(
                    expr,
                    index_expr,
                    bracket_token.file,
                    bracket_token.line,
                    bracket_token.column,
                )
            elif next_token.value == "(" and isinstance(expr, Identifier):
                # This handles direct function calls like `myFunc(arg)`
                self.consume_token()  # consume "("
                args = self.parse_argument_list()
                self.expect_token(")")
                self.consume_token()  # consume ")"
                expr = FuncCall(expr.name, args, expr.file, expr.line, expr.column)
            else:
                break  # No more chained access/calls
        return expr

    def parse_def(self):
        self.expect_token("def")
        token = self.consume_token()
        self.expect_token("(")
        self.consume_token()

        parameters = []
        if self.get_next_token() and self.get_next_token().value != ")":
            while True:
                is_compact = False
                if self.get_next_token() and self.get_next_token().value == "compact":
                    self.consume_token()  # consume 'compact'
                    is_compact = True

                param_token = self.expect_type("identifier")
                param_name = param_token.value
                self.consume_token()
                annotation_type = None
                if self.get_next_token() and self.get_next_token().type == "identifier":
                    if is_compact:
                        raise NovaError(token, "'compact' does not support this")
                    type_token = self.get_next_token()
                    if (
                        type_token.value in BUILTIN_VAR_TYPES
                        or type_token.value in CUSTOM_TYPES
                    ):
                        annotation_type = type_token.value
                        self.consume_token()
                        annotation_type = self._consume_array_brackets(annotation_type)

                default_expr = None
                if self.get_next_token() and self.get_next_token().value == "=":
                    if is_compact:
                        raise NovaError(token, "'compact' does not support this")
                    self.consume_token()
                    default_expr = self.parse_expression()
                    if annotation_type:
                        raise NovaError(
                            token,
                            "Cannot have both explicit type annotation and a default value. Consider removing the type annotation to allow type inference from the default value.",
                        )

                    if default_expr.type == "Literal":
                        inferred_type = type(default_expr.value).__name__
                        annotation_type = BUILTIN_VAR_TYPES_INFER.get(
                            inferred_type, None
                        )
                    else:
                        annotation_type = None

                parameters.append(
                    Parameter(
                        param_name,
                        annotation_type,
                        default_expr,
                        is_compact,
                        param_token.file,
                        param_token.line,
                        param_token.column,
                        param_token.type,
                        param_token.value,
                    )
                )

                if self.get_next_token() and self.get_next_token().value == ",":
                    self.consume_token()
                else:
                    break

        self.expect_token(")")
        self.consume_token()
        if self.get_next_token().value == "=":
            t = self.consume_token()
            body = [ReturnStmt(self.parse_expression(), t.file, t.line, t.column)]
        else:
            body = self.parse_block_until(["end"])
            self.expect_token("end")
            self.consume_token()

        return LambdaDecl(parameters, body, token.file, token.line, token.column)

    def parse_primary(self):
        node = None
        token = self.get_next_token()
        if not token:
            last_token = (
                self.tokens[self.current - 1]
                if self.current > 0
                else Token("EOF", "EOF", self.file, 1, 1)
            )
            raise NovaError(last_token, "Unexpected end of input")

        if token.type == "number" or token.type == "string":
            self.consume_token()
            c = token.value
            node = Literal(c, token.file, token.line, token.column)
        elif token.value == "[":
            self.consume_token()
            elements = []
            if self.get_next_token() and self.get_next_token().value != "]":
                while True:
                    elements.append(self.parse_expression())
                    if self.get_next_token() and self.get_next_token().value == ",":
                        self.consume_token()
                    else:
                        break
            self.expect_token("]")
            self.consume_token()
            node = ArrayLiteral(elements, token.file, token.line, token.column)
        elif token.value == "{":
            self.consume_token()
            properties = []
            while self.get_next_token() and self.get_next_token().value != "}":
                if self.get_next_token().value == "}":
                    self.consume_token()
                    break
                key_token = self.get_next_token()
                if key_token.type not in ["identifier", "string"]:
                    raise NovaError(
                        key_token,
                        "Expected identifier or string as object key, got {}({})".format(
                            key_token.type, key_token.value
                        ),
                    )
                key = key_token.value
                self.consume_token()
                if key_token.type == "identifier" and self.get_next_token().value in [
                    ",",
                    "}",
                ]:
                    properties.append(
                        {
                            "key": key,
                            "value": Identifier(
                                key_token.value,
                                key_token.file,
                                key_token.line,
                                key_token.column,
                            ),
                        }
                    )
                else:
                    self.expect_token(":")
                    self.consume_token()
                    value = self.parse_expression()
                    properties.append({"key": key, "value": value})
                if self.get_next_token() and self.get_next_token().value == ",":
                    self.consume_token()
            self.expect_token("}")
            self.consume_token()
            node = ObjectLiteral(properties, token.file, token.line, token.column)
        elif token.type == "identifier":
            self.consume_token()
            node = Identifier(token.value, token.file, token.line, token.column)
        elif token.value == "enum":
            self.consume_token()
            self.expect_token("{")
            self.consume_token()
            values = []
            while self.get_next_token() and self.get_next_token().value != "}":
                v = self.expect_type("identifier").value
                if v in values:
                    raise NovaError(token, "Enum values must be unique.")
                values.append(v)
                self.consume_token()
            self.expect_token("}")
            self.consume_token()

            if len(values) < 1:
                raise NovaError(token, "Enum must have at least one value.")

            node = EnumDef(values, token.file, token.line, token.column)

        elif token.type == "keyword" and token.value == "new":
            self.consume_token()  # consume 'new'
            # Parse the fully qualified class name, e.g., "module.sub.ClassName"
            name_parts = []
            name_part_token = self.expect_type("identifier")
            name_parts.append(name_part_token.value)
            # Use the token from the first part of the name for error reporting
            new_instance_file = name_part_token.file
            new_instance_line = name_part_token.line
            new_instance_column = name_part_token.column
            self.consume_token()  # Consume the first identifier

            while self.get_next_token() and self.get_next_token().value == ".":
                self.consume_token()  # Consume the dot
                name_part_token = self.expect_type("identifier")
                name_parts.append(name_part_token.value)
                self.consume_token()  # Consume the next identifier

            full_class_name = ".".join(name_parts)

            self.expect_token("(")
            self.consume_token()

            args = []
            if self.get_next_token() and self.get_next_token().value != ")":
                while True:
                    args.append(self.parse_expression())
                    if self.get_next_token() and self.get_next_token().value == ",":
                        self.consume_token()
                    else:
                        break
            self.expect_token(")")
            self.consume_token()
            node = NewInstance(
                full_class_name,
                args,
                new_instance_file,
                new_instance_line,
                new_instance_column,
            )
        elif token.value == "@":
            at_token = self.consume_token()  # Consume '@'
            wrapper = self.parse_expression()
            # body = self.parse_def()
            body = self.parse_expression()
            # print(wrapper.__dict__, body.__dict__)
            return DecoratorExpr(
                wrapper, body, at_token.file, at_token.line, at_token.column
            )

        elif token.value == "$":
            dollar_token = self.consume_token()  # consume '$'
            identifier_token = self.expect_type("identifier")
            identifier_name = identifier_token.value
            self.consume_token()  # consume the identifier

            # Construct 'self'
            self_identifier = Identifier(
                "self", dollar_token.file, dollar_token.line, dollar_token.column
            )

            # If followed by '(', it's a method call directly on self
            if self.get_next_token() and self.get_next_token().value == "(":
                self.consume_token()  # consume '('
                args = self.parse_argument_list()
                self.expect_token(")")
                self.consume_token()  # consume ')'

                node = MethodCall(
                    self_identifier,
                    identifier_name,
                    args,
                    identifier_token.file,
                    identifier_token.line,
                    identifier_token.column,
                )
            else:
                # Plain property / attribute access
                node = PropertyAccess(
                    self_identifier,
                    identifier_name,
                    identifier_token.file,
                    identifier_token.line,
                    identifier_token.column,
                )
        elif token.value == "def":  # Lambda expression (def (...) ... end)
            return self.parse_def()

        elif token.value == "(":
            self.consume_token()
            node = self.parse_expression()
            self.expect_token(")")
            self.consume_token()
        else:
            raise NovaError(token, f"Unexpected token: {token.value}")

        return node


class Interpreter:
    def __init__(self, source, file_path):
        self.source = source
        c = pathlib.Path(file_path)
        self.file = c.expanduser().resolve()
        # self.file = os.path.abspath(file_path)
        self.tk = Tokenizer(source, file_path)
        self.globals = Environment()

        self.modules_loaded = {}
        self.current_env = None

        self.globals.define(
            "__SCRIPT_PATH__", os.path.dirname(os.path.abspath(self.file))
        )
        self.globals.define("__SCRIPT_NAME__", file_path)
        self.globals.define("__IS_MAIN__", True)
        self.current_class_stack = []
        self.callStack = []
        self.errorStack = []

        # ------------------------------------------------------------------
        # Extensibility registries
        # ------------------------------------------------------------------
        # To add a new statement type:
        #   1. Add the AST node in nodes.py
        #   2. Implement a method `_stmt_YourType(self, stmt, env)`
        #   3. Register it here: self._stmt_handlers["YourType"] = self._stmt_YourType
        # Same pattern for expressions with `_expr_handlers`.
        #
        # The big if/elif chains remain for now for backward compatibility, but
        # new features should prefer the registry path so the core dispatch stays
        # clean and easy to maintain.
        self._stmt_handlers = {}
        self._expr_handlers = {}
        self._register_default_handlers()

    def _register_default_handlers(self):
        """Populate the extensibility registries. Override or extend in subclasses."""
        # Statement handlers (examples – full coverage still lives in execute_stmt)
        # self._stmt_handlers["VarDecl"] = self._stmt_VarDecl
        # ...
        # Expression handlers
        # self._expr_handlers["Literal"] = self._expr_Literal
        pass

    def _create_callable(
        self, parameters, body, env, name=None, node=None, context_name=None
    ):
        """
        Shared factory for both named functions (FuncDecl) and lambdas (LambdaDecl).
        A lambda is simply an unnamed / uuid-named function.

        - Captures the current class context (if any) so nested lambdas can still
          touch private members of the enclosing class.
        - Uses the common bind_parameters helper.
        - Attaches __nova_ast__ so dynamic method binding continues to work.
        """
        captured_class = (
            self.current_class_stack[-1] if self.current_class_stack else None
        )
        display_name = name or "lambda"
        ctx = context_name or display_name

        def func_wrapper(*args, **kwargs):
            pushed = False
            if captured_class is not None:
                self.current_class_stack.append(captured_class)
                pushed = True
            try:
                func_env = Environment(env)
                bound = bind_parameters(
                    parameters,
                    args,
                    kwargs,
                    self,
                    env,
                    context_name=ctx,
                    token=node,
                )
                for pname, pval in bound.items():
                    func_env.define(pname, pval)
                result = self.execute_block(body, func_env)
                if isinstance(result, ReturnFlow):
                    return result.value
                return None
            finally:
                if pushed and self.current_class_stack:
                    self.current_class_stack.pop()

        wrapper = FuncWrapp(
            func_wrapper,
            (node.__repr__ if node else None),
            (node.__str__ if node else None),
            node,
            name=display_name,
        )
        wrapper.__name__ = name if name is not None else str(uuid.uuid4())
        if node is not None:
            wrapper.__nova_ast__ = node
        if captured_class is not None:
            wrapper.__DefiningClass = captured_class
        return wrapper

    def _check_private_access(self, obj, name, expr):
        """Raise NovaError if `name` is a private member of `obj` and access is not from inside its class."""
        if not isinstance(obj, dict) or "__DefiningClass" not in obj:
            return  # not a NovaScript instance
        cls = obj["__DefiningClass"]
        if name in cls._private_properties or name in cls._private_methods:
            # allowed only if the current class (top of stack) is exactly this class
            if not self.current_class_stack or self.current_class_stack[-1] is not cls:
                raise NovaError(
                    expr,
                    f"Private member '{name}' cannot be accessed outside its class",
                )

    # --- Evaluation / Execution ---
    def interpret(self):
        statements = self.tk.parse_block()
        try:
            self.execute_block(statements, self.globals)
        except Exception as err:
            # Ensure deferred statements still execute
            self.globals.execute_deferred(self)
            print("\n".join(reversed(self.errorStack)))
            if isinstance(err, NovaError):
                print(f"{err.message}", file=sys.stderr)
            else:
                print(
                    f"{err.__class__.__name__}:{err}"
                )  # trying to print the name and what caused it
            exit(1)

    def execute_block(self, statements, env):
        previous_env = self.current_env
        self.current_env = env
        try:
            for stmt in statements:
                result = self.execute_stmt(stmt, env)
                if isinstance(result, ControlFlow):
                    return result
            return None
        finally:
            # Execute deferred statements in reverse order
            env.execute_deferred(self)
            self.current_env = previous_env

    def get_current_context(self):
        return self.current_env

    def execute_stmt(self, stmt, env):
        dprint("stmt: ", stmt)

        # Prefer registered handler when present (makes new features easy to plug in)
        handler = self._stmt_handlers.get(stmt.type)
        if handler is not None:
            return handler(stmt, env)

        if stmt.type in ["VarDecl", "ConstDecl"]:
            value = self.evaluate_expr(stmt.initializer, env)
            if stmt.type_annotation:
                check_type(stmt.type_annotation, value, stmt)
            env.define(stmt.name, value, stmt.type == "ConstDecl", stmt.type_annotation)

        elif stmt.type == "ExportStmt":
            # Execute the inner statement first
            self.execute_stmt(stmt.expr, env)

            # Ensure exports dictionary exists (it should in modules, but create if missing)
            if not env.has("exports"):
                env.define("exports", {})
            exports_dict = env.get("exports").value

            inner = stmt.expr
            if inner.type in ("VarDecl", "ConstDecl", "FuncDecl", "ClassDefinition"):
                # For declarations, export the declared name
                name = inner.name
                if name in exports_dict:
                    raise NovaError(stmt, "cannot re-export: {}".format(name))
                value = env.get(name).value
                exports_dict[name] = value

            elif inner.type == "ExpressionStmt":
                expr = inner.expression
                if expr.type == "ObjectLiteral":
                    # export { a, b, c: d } → evaluate each property
                    for prop in expr.properties:
                        key = prop["key"]
                        val_expr = prop["value"]
                        if key in exports_dict:
                            raise NovaError(stmt, "cannot re-export: {}".format(key))
                        if val_expr.type == "Identifier":
                            val = env.get(val_expr.name).value
                        else:
                            val = self.evaluate_expr(val_expr, env)
                        exports_dict[key] = val
                elif expr.type == "Identifier":
                    # export a → export the variable 'a'
                    name = expr.name
                    if name in exports_dict:
                        raise NovaError(stmt, "cannot re-export: {}".format(name))
                    val = env.get(name).value
                    exports_dict[name] = val
                else:
                    raise NovaError(
                        stmt, f"Cannot export expression of type {expr.type}"
                    )

            else:
                raise NovaError(stmt, f"Cannot export statement of type {inner.type}")
        elif stmt.type == "DeferStmt":
            # To match TypeScript's behavior: statements within a single defer block
            # are executed in FIFO order. Since `execute_deferred` pops from the end
            # of the list, we must add them in reverse order here.
            reversed_stmts = list(reversed(stmt.body))
            for s in reversed_stmts:
                env.add_deferred(s)

        elif stmt.type == "ExpressionStmt":
            self.evaluate_expr(stmt.expression, env)

        elif stmt.type == "BreakStmt":
            return BreakFlow()

        elif stmt.type == "ContinueStmt":
            return ContinueFlow()

        elif stmt.type == "TryStmt":
            try:
                result = self.execute_block(stmt.try_block, env)
                if isinstance(result, ControlFlow) and not isinstance(
                    result, ReturnFlow
                ):
                    return result
            except Exception as e:  # Catch all other Python exceptions
                catch_env = Environment(env)
                # If e is a NovaError, use it directly. Otherwise, wrap it.
                catch_env.define(stmt.error_var, e, True)
                result = self.execute_block(stmt.catch_block, catch_env)
                if isinstance(result, ControlFlow):
                    return result
        elif stmt.type == "WithStmt":
            nenv = Environment(env)
            expression_val = self.evaluate_expr(stmt.expr, env)
            if getattr(expression_val, "__enter__", None):
                var = expression_val.__enter__()
            elif isinstance(expression_val, dict) and "__enter__" in expression_val:
                var = expression_val["__enter__"]()
            else:
                var = expression_val

            nenv.define(stmt.alias, var)
            try:
                result = self.execute_block(stmt.body, nenv)
                if isinstance(result, ControlFlow):
                    return result
            except Exception as e:
                handled = False
                if getattr(expression_val, "__exit__", None):
                    handled = expression_val.__exit__(type(e), e, e.__traceback__)
                elif isinstance(expression_val, dict) and "__exit__" in expression_val:
                    handled = expression_val["__exit__"](type(e), e, e.__traceback__)

                if not handled:
                    raise
            else:
                if getattr(expression_val, "__exit__", None):
                    expression_val.__exit__(None, None, None)
                elif isinstance(expression_val, dict) and "__exit__" in expression_val:
                    expression_val["__exit__"](None, None, None)

        elif stmt.type == "IfStmt":
            condition = self.evaluate_expr(stmt.condition, env)
            if condition:
                result = self.execute_block(stmt.then_block, Environment(env))
                if isinstance(result, ControlFlow):
                    return result
            elif stmt.else_if:
                matched = False
                for elseif_block in stmt.else_if:
                    elseif_condition = self.evaluate_expr(
                        elseif_block["condition"], env
                    )
                    if elseif_condition:
                        result = self.execute_block(
                            elseif_block["body"], Environment(env)
                        )
                        if isinstance(result, ControlFlow):
                            return result
                        matched = True
                        break
                if not matched and stmt.else_block:
                    result = self.execute_block(stmt.else_block, Environment(env))
                    if isinstance(result, ControlFlow):
                        return result
            elif stmt.else_block:
                result = self.execute_block(stmt.else_block, Environment(env))
                if isinstance(result, ControlFlow):
                    return result

        elif stmt.type == "ObjectDecl":
            temp_class = NovaClass(f"__anon_{stmt.name}", None, self, env)

            for member in stmt.body:
                if member.type == "FuncDecl":
                    if member.name == "init":
                        method_def = MethodDefinition(
                            member.name,
                            member.parameters,
                            member.body,
                            False,
                            True,
                            False,
                            member.file,
                            member.line,
                            member.column,
                        )
                        temp_class.constructor_def = method_def

                    else:
                        method_def = MethodDefinition(
                            member.name,
                            member.parameters,
                            member.body,
                            False,
                            False,
                            False,
                            member.file,
                            member.line,
                            member.column,
                        )
                        temp_class.instance_methods[member.name] = method_def
                elif member.type == "VarDecl":
                    # Treat 'var' as instance properties
                    prop_def = PropertyDefinition(
                        member.name,
                        member.type_annotation,
                        member.initializer,
                        False,  # is_static
                        False,  # is_private
                        False,  # is_const
                        member.file,
                        member.line,
                        member.column,
                    )
                    temp_class.instance_properties[member.name] = prop_def

                elif member.type == "PropertyDefinition":
                    # Use member directly or create a new one with forced flags
                    prop_def = PropertyDefinition(
                        member.name,
                        member.type_annotation,
                        member.initializer,
                        False,  # non‑static
                        False,  # non‑private
                        getattr(member, "is_const", False),
                        member.file,
                        member.line,
                        member.column,
                    )
                    temp_class.instance_properties[member.name] = prop_def

                elif member.type == "PropertyHandler":
                    # Add getter and setter as methods
                    temp_class.instance_methods[member.getter.name] = member.getter
                    temp_class.instance_methods[member.setter.name] = member.setter

                else:
                    raise NovaError(member, "not supported")

            # 4. Instantiate and assign to the variable name provided
            instance = temp_class.instantiate([])
            env.define(stmt.name, instance)
        elif stmt.type == "WhileStmt":
            while self.evaluate_expr(stmt.condition, env):
                result = self.execute_block(stmt.body, Environment(env))
                if isinstance(result, BreakFlow):
                    break
                elif isinstance(result, ReturnFlow):
                    return result
                elif isinstance(result, ContinueFlow):
                    continue
        elif stmt.type == "UntilStmt":
            while not self.evaluate_expr(stmt.condition, env):
                result = self.execute_block(stmt.body, Environment(env))
                if isinstance(result, BreakFlow):
                    break
                elif isinstance(result, ReturnFlow):
                    return result
                elif isinstance(result, ContinueFlow):
                    continue
        elif stmt.type == "ForEachStmt":
            list_val = self.evaluate_expr(stmt.list, env)
            if not isinstance(list_val, (list, dict)) and not hasattr(
                list_val, "__iter__"
            ):
                raise NovaError(
                    stmt,
                    f"Cannot iterate over non-array type for forEach loop. Got: {type(list_val).__name__}",
                )
            for item in list_val:
                loop_env = Environment(env)
                loop_env.define(stmt.variable, item)
                result = self.execute_block(stmt.body, loop_env)
                if isinstance(result, BreakFlow):
                    break
                elif isinstance(result, ReturnFlow):
                    return result
                elif isinstance(result, ContinueFlow):
                    continue
        elif stmt.type == "ForStmt":
            start = self.evaluate_expr(stmt.start, env)
            end = self.evaluate_expr(stmt.end, env)
            step = self.evaluate_expr(stmt.step, env) if stmt.step else 1

            if not all(isinstance(val, (int, float)) for val in [start, end, step]):
                raise NovaError(
                    stmt,
                    f"For loop bounds and step must be numbers. Got start: {type(start).__name__}, end: {type(end).__name__}, step: {type(step).__name__}",
                )

            # Python's range handles step correctly. For float steps, manual loop.
            # Assuming integer steps for now, as floats can lead to precision issues.
            # If NovaScript intends float steps, this will need adjustment.
            if isinstance(step, float):
                current_val = start
                while (step > 0 and current_val <= end) or (
                    step < 0 and current_val >= end
                ):
                    loop_env = Environment(env)
                    loop_env.define(stmt.variable, current_val)
                    result = self.execute_block(stmt.body, loop_env)
                    if isinstance(result, BreakFlow):
                        break
                    elif isinstance(result, ReturnFlow):
                        return result
                    elif isinstance(result, ContinueFlow):
                        current_val += step
                        continue
                    current_val += step
            else:  # Integer step
                for i in range(start, end + (1 if step > 0 else -1), step):
                    loop_env = Environment(env)
                    loop_env.define(stmt.variable, i)
                    result = self.execute_block(stmt.body, loop_env)
                    if isinstance(result, BreakFlow):
                        break
                    elif isinstance(result, ReturnFlow):
                        return result
                    elif isinstance(result, ContinueFlow):
                        continue
        elif stmt.type == "ScopeStmt":
            n_env = Environment(env)
            n_env.localsOnly = True
            result = self.execute_block(stmt.body, n_env)
            env.define(stmt.name, n_env)
            if isinstance(result, ControlFlow):
                return result

        elif stmt.type == "SwitchStmt":
            value = self.evaluate_expr(stmt.expression, env)
            matched = False
            default_case_body = None
            for case in stmt.cases:
                if case.case_expr is None:  # This is the default case
                    default_case_body = case.body
                else:
                    case_val = self.evaluate_expr(case.case_expr, env)
                    if value == case_val:
                        result = self.execute_block(case.body, Environment(env))
                        if isinstance(result, ControlFlow):
                            return result
                        matched = True
                        break  # Exit switch after first match

            if not matched and default_case_body and not stmt.strict:
                result = self.execute_block(default_case_body, Environment(env))
                if isinstance(result, ControlFlow):
                    return result

            if stmt.strict and not matched:
                raise NovaError(
                    stmt, f"Switch statement did not match any case for value: {value}"
                )

        elif stmt.type == "ReturnStmt":
            if not stmt.expression:
                return ReturnFlow(None)
            value = self.evaluate_expr(stmt.expression, env)
            return ReturnFlow(value)
        elif stmt.type == "LocalFuncDecl":
            _env = Environment(env)
            _env.localsOnly = True
            self.execute_stmt(stmt.fn, _env)
            env.define(
                stmt.fn.name, _env.get(stmt.fn.name).value, _env.get(stmt.fn.name).const
            )
        elif stmt.type == "FuncDecl":
            # Named function – same machinery as a lambda, just with a name
            # and stored in the environment.
            func_wrapper = self._create_callable(
                stmt.parameters,
                stmt.body,
                env,
                name=stmt.name,
                node=stmt,
                context_name=f"function '{stmt.name}'",
            )
            env.define(stmt.name, func_wrapper)

        elif stmt.type == "ClassDefinition":
            class_def = stmt
            super_class = None
            if class_def.superclass_name:
                # Resolve the fully qualified superclass name
                name_parts = class_def.superclass_name.split(".")
                current_resolved_object = env
                for i, part in enumerate(name_parts):
                    if isinstance(current_resolved_object, Environment):
                        # FIX: Get value from var
                        var_obj = current_resolved_object.get(part, class_def)
                        next_resolved_part = var_obj.value
                    elif isinstance(current_resolved_object, dict):
                        next_resolved_part = current_resolved_object.get(part)
                    elif hasattr(current_resolved_object, part):
                        next_resolved_part = getattr(current_resolved_object, part)
                    else:
                        raise NovaError(
                            class_def,
                            f"Cannot resolve part '{part}' in superclass path '{class_def.superclass_name}'.",
                        )

                    if next_resolved_part is None:
                        raise NovaError(
                            class_def,
                            f"Superclass '{class_def.superclass_name}' part '{part}' not found.",
                        )

                    current_resolved_object = next_resolved_part

                super_class = current_resolved_object

                # Allow NovaClass or Python type/class as superclass
                if not isinstance(super_class, (NovaClass, type)):
                    raise NovaError(
                        class_def,
                        f"Superclass '{class_def.superclass_name}' resolved to type {type(super_class).__name__}, which is not a class.",
                    )

            # Pass the resolved super_class to NovaClass
            nova_class = NovaClass(class_def.name, super_class, self, env)

            # Populate static members, instance properties/methods
            for member in class_def.body:
                if member.type == "PropertyDefinition":
                    if member.is_private:
                        nova_class._private_properties.add(member.name)
                    if getattr(member, "is_const", False):
                        nova_class._const_properties.add(member.name)
                    if member.is_static:
                        prop_value = None
                        if member.initializer:
                            prop_value = self.evaluate_expr(member.initializer, env)
                        nova_class.static_members[member.name] = prop_value
                        if getattr(member, "is_const", False):
                            nova_class._static_const.add(member.name)
                    else:
                        nova_class.instance_properties[member.name] = member
                elif member.type == "PropertyHandler":
                    nova_class.instance_properties[member.name] = member

                elif member.type == "MethodDefinition":
                    if member.is_private:
                        nova_class._private_methods.add(member.name)
                    if member.is_constructor:
                        nova_class.constructor_def = member
                    elif member.is_static:
                        # Wrap static methods using shared bind helper
                        def static_method_wrapper(*args, _member=member, **kwargs):
                            method_env = Environment(env)
                            method_env.define("self", nova_class.static_members)
                            bound = bind_parameters(
                                _member.parameters,
                                args,
                                kwargs,
                                self,
                                env,
                                context_name=f"static method '{_member.name}'",
                                token=_member,
                            )
                            for pname, pval in bound.items():
                                method_env.define(pname, pval)
                            result = self.execute_block(_member.body, method_env)
                            if isinstance(result, ReturnFlow):
                                return result.value
                            return None

                        nova_class.static_members[member.name] = static_method_wrapper
                    else:
                        nova_class.instance_methods[member.name] = member
                else:
                    raise NovaError(member, f"not supported: {member}")

            env.define(class_def.name, nova_class)
        elif stmt.type == "AssertStmt":
            e = self.evaluate_expr(stmt.expression, env)
            if not e:
                raise NovaError(stmt, f"{stmt.message}")
        elif stmt.type == "UsingStmt":
            # Helper to import members from a dict/Environment/module into env
            def import_members(value):
                if isinstance(value, NovaClass):
                    # Import public static members only
                    for key, val in value.get_public_static_members().items():
                        env.define(key, val)
                elif isinstance(value, dict) and "__DefiningClass" in value:
                    # NovaScript instance: import public instance members
                    cls = value["__DefiningClass"]
                    for key, val in cls.get_public_instance_members(value).items():
                        env.define(key, val)
                elif isinstance(value, Environment):
                    for key, var_obj in value.values.items():
                        # Environment has no private concept; import everything
                        env.define(
                            key, var_obj.value, var_obj.const, var_obj.type_annotation
                        )
                elif isinstance(value, dict):
                    # Plain dict – import all keys (no privacy)
                    for key, val in value.items():
                        env.define(key, val)
                elif hasattr(value, "__dict__"):
                    # Python object – skip names starting with '_' (by convention)
                    for key, val in value.__dict__.items():
                        if not key.startswith("_"):
                            env.define(key, val)
                else:
                    raise NovaError(
                        stmt,
                        f"Cannot 'use' value of type {type(value).__name__}. "
                        "Expected a namespace, dict, class, instance, or Python object.",
                    )

            if isinstance(stmt.name, list):
                # List of string paths: resolve each and import members
                for path_str in stmt.name:
                    parts = path_str.split(".")
                    current = env
                    for part in parts:
                        if isinstance(current, Environment):
                            var_obj = current.get(part, stmt)
                            current = var_obj.value
                        elif isinstance(current, dict):
                            current = current.get(part)
                        elif hasattr(current, part):
                            current = getattr(current, part)
                        else:
                            raise NovaError(
                                stmt,
                                f"Cannot resolve part '{part}' in path '{path_str}'.",
                            )
                        if current is None:
                            raise NovaError(
                                stmt,
                                f"Name '{path_str}' part '{part}' not found.",
                            )
                    import_members(current)
            else:
                # Single expression: evaluate it, then import members from the result
                value = self.evaluate_expr(stmt.name, env)
                import_members(value)
        else:
            raise NovaError(stmt, f"Unknown statement type: {stmt.type}")

    # --- NEW HELPER: Resolves the base object and final key/index for assignment ---
    def resolve_assignment_target(self, target_expr, env):
        if isinstance(target_expr, Identifier):
            # For simple identifiers, the base is the environment and the key is the identifier name
            return {"base": env, "final_key": target_expr.name}
        elif isinstance(target_expr, ArrayAccess):
            # Recursively evaluate the object part to get the actual array/object
            base_object = self.evaluate_expr(target_expr.object, env)
            index = self.evaluate_expr(target_expr.index, env)

            if base_object is None:
                raise NovaError(target_expr, "Cannot assign to index of None value.")
            if not isinstance(base_object, (list, dict)) and not hasattr(
                base_object, "__setitem__"
            ):  # Python lists/dicts for arrays/objects
                # print(dir(base_object))
                raise NovaError(
                    target_expr,
                    f"Cannot assign to index of non-list/dict: {type(base_object).__name__}",
                )
            if not isinstance(index, (int, str)):
                raise NovaError(
                    target_expr,
                    f"List/dict index must be a number or string for assignment. Got: {type(index).__name__}",
                )
            return {"base": base_object, "final_key": index}
        elif isinstance(target_expr, PropertyAccess):
            # Recursively evaluate the object part to get the actual object
            base_object = self.evaluate_expr(target_expr.object, env)
            key = target_expr.property  # Property name is a string

            if base_object is None:
                raise NovaError(
                    target_expr, f"Cannot assign property '{key}' of None value."
                )

            # Special case: if the base object is an Environment, we assign to it directly
            if isinstance(base_object, Environment):
                return {"base": base_object, "final_key": key}
            # If the base object is a NovaScript instance (a Python dict), assign to its key.
            elif isinstance(base_object, dict):
                return {"base": base_object, "final_key": key}
            # For other Python objects, assume it's a regular attribute access.
            else:
                return {"base": base_object, "final_key": key}
        else:
            raise NovaError(
                target_expr, "Invalid assignment target type: " + target_expr.type
            )

    def get_target_type(self, target, env):
        """Resolve the expected type for an assignment target (Identifier or nested PropertyAccess)."""
        if isinstance(target, Identifier):
            var = env.get(target.name, target)
            return var.type_annotation

        if isinstance(target, PropertyAccess):
            # Recurse to get the type of the parent object
            parent_type = self.get_target_type(target.object, env)
            if not parent_type or parent_type not in CUSTOM_TYPES:
                return None
            custom_def = CUSTOM_TYPES[parent_type]
            prop_def = next(
                (p for p in custom_def.properties if p.name == target.property), None
            )
            return prop_def.type if prop_def else None

        # ArrayAccess or anything else → no static type check for now
        return None

    def evaluate_expr(self, expr, env):
        dprint("expr: ", expr)

        # Prefer registered handler when present (extension point for new expr types)
        handler = self._expr_handlers.get(expr.type)
        if handler is not None:
            return handler(expr, env)

        if expr.type == "Literal":
            return expr.value
        elif expr.type == "Identifier":
            return env.get(expr.name, expr).value

        elif expr.type == "AssignmentExpr":
            target = expr.target
            op = expr.operator

            # Resolve where we are assigning to
            resolved = self.resolve_assignment_target(target, env)
            base = resolved["base"]
            final_key = resolved["final_key"]

            assigned_value = self.evaluate_expr(expr.value, env)

            # === Compound assignment handling ===
            if op != "=":
                if isinstance(base, Environment):
                    current_value = base.get(final_key, target).value
                elif isinstance(base, dict):
                    current_value = base.get(final_key)
                else:
                    current_value = getattr(base, final_key, None)
                if isinstance(current_value, Proxy):
                    if not current_value.get:
                        raise NovaError(
                            target, f"Property '{final_key}' has no getter."
                        )
                    current_value = current_value.get()

                if op == "+=":
                    final_value_to_assign = current_value + assigned_value
                elif op == "-=":
                    final_value_to_assign = current_value - assigned_value
                elif op == "*=":
                    if isinstance(current_value, (int, float)) and not isinstance(
                        assigned_value, (int, float)
                    ):
                        raise NovaError(
                            expr,
                            f"If you wanted to repeat '{assigned_value}' {current_value} times, "
                            f"you'd do '\"{assigned_value}\" * {current_value}'",
                        )
                    final_value_to_assign = current_value * assigned_value
                elif op == "/=":
                    if assigned_value == 0:
                        raise NovaError(
                            expr, "Division by zero in compound assignment."
                        )
                    final_value_to_assign = current_value / assigned_value
                elif op == "%=":
                    final_value_to_assign = current_value % assigned_value
                else:
                    raise NovaError(expr, f"Unknown compound assignment operator: {op}")
            else:
                final_value_to_assign = assigned_value

            # === Perform the actual assignment + type checking ===
            if isinstance(target, ArrayLiteral):  # destructuring
                source_array = final_value_to_assign
                if not isinstance(source_array, list):
                    raise NovaError(
                        target,
                        f"Cannot destructure non-list value. Expected list, got {type(source_array).__name__}.",
                    )
                for i, target_element in enumerate(target.elements):
                    source_value = source_array[i] if i < len(source_array) else None
                    temp_assignment = AssignmentExpr(
                        target_element,
                        Literal(source_value, expr.file, expr.line, expr.column),
                        "=",
                        expr.file,
                        expr.line,
                        expr.column,
                    )
                    self.evaluate_expr(temp_assignment, env)

            else:  # normal / property / nested assignment
                if isinstance(base, Environment):
                    var = base.get(final_key, target)
                    if var.type_annotation:
                        check_type(var.type_annotation, final_value_to_assign, target)
                    base.assign(final_key, final_value_to_assign, target)

                elif isinstance(base, dict):
                    # Enforce const properties on Nova instances
                    const_set = base.get("__const__")
                    if const_set and final_key in const_set:
                        raise NovaError(
                            target,
                            f"Cannot re-assign constant property '{final_key}'",
                        )
                    # Resolve expected type for (possibly nested) property using root variable's type
                    if final_key in base and isinstance(base[final_key], Proxy):
                        proxy = base[final_key]
                        if not proxy.set:
                            raise NovaError(
                                target, f"Property '{final_key}' has no setter."
                            )
                        cls = base.get("__DefiningClass")
                        if cls:
                            try:
                                self.current_class_stack.append(cls)
                                proxy.set(final_value_to_assign)
                            finally:
                                self.current_class_stack.pop()
                        else:
                            proxy.set(final_value_to_assign)
                    elif "__DefiningClass" in base and isinstance(
                        assigned_value, Callable
                    ):
                        # === Dynamic method binding ===
                        # Makes it easy to do: instance.foo = def() print(self.name) end
                        # We prefer the original AST (LambdaDecl) when available so we
                        # can reconstruct a proper MethodDefinition that receives `self`.
                        # Extension point: if you add new callable AST nodes, just attach
                        # .__nova_ast__ on the wrapper (see LambdaDecl evaluation).
                        ast_node = getattr(assigned_value, "__nova_ast__", None)
                        if (
                            ast_node is not None
                            and hasattr(ast_node, "parameters")
                            and hasattr(ast_node, "body")
                        ):
                            meth = MethodDefinition(
                                final_key,
                                ast_node.parameters,
                                ast_node.body,
                                False,  # is_static
                                False,  # is_constructor
                                False,  # is_private
                                expr.file,
                                expr.line,
                                expr.column,
                            )
                            bound = base["__DefiningClass"]._create_method(meth, base)
                            base[final_key] = bound
                        elif hasattr(expr.value, "parameters") and hasattr(
                            expr.value, "body"
                        ):
                            # Fallback: RHS was still the raw AST (should not normally happen)
                            meth = MethodDefinition(
                                final_key,
                                expr.value.parameters,
                                expr.value.body,
                                False,
                                False,
                                False,
                                expr.file,
                                expr.line,
                                expr.column,
                            )
                            bound = base["__DefiningClass"]._create_method(meth, base)
                            base[final_key] = bound
                        else:
                            # Plain Python callable or already-bound function – store as-is
                            base[final_key] = final_value_to_assign

                    else:
                        expected_type = self.get_target_type(target, env)
                        if expected_type:
                            check_type(expected_type, final_value_to_assign, target)
                        base[final_key] = final_value_to_assign

                else:
                    # Python object
                    try:
                        setattr(base, final_key, final_value_to_assign)
                    except (AttributeError, TypeError):
                        if isinstance(base, (dict, list)) or hasattr(
                            base, "__setitem__"
                        ):
                            base[final_key] = final_value_to_assign
                        else:
                            raise NovaError(
                                target,
                                f"Cannot assign to property '{final_key}' of object of type {type(base).__name__}.",
                            )

            return final_value_to_assign

        elif expr.type == "BinaryExpr":
            dprint("BinOp", expr)
            if expr.operator == "&&":
                f = self.evaluate_expr(expr.left, env)
                if not f:
                    return f
                return self.evaluate_expr(expr.right, env)
            elif expr.operator == "||":
                f = self.evaluate_expr(expr.left, env)
                if f:
                    return f
                return self.evaluate_expr(expr.right, env)
            left = self.evaluate_expr(expr.left, env)
            right = self.evaluate_expr(expr.right, env)

            # Handle operator overloading for custom objects (dicts)
            if isinstance(left, dict):
                op = opToName(expr.operator)
                if op and op in left and callable(left[op]):
                    try:
                        return left[op](right)
                    except Exception as e:
                        raise NovaError(
                            expr, f"Error calling overloaded operator '{op}': {e}"
                        )
                # If no operator overloading found, fall through to normal operations

            # Normal binary operations
            if expr.operator == "+":
                return left + right
            elif expr.operator == "|":
                return left | right
            elif expr.operator == "&":
                return left & right
            elif expr.operator == "%":
                if isinstance(left, str):
                    if isinstance(right, list):
                        left = left.format(*right)
                    elif isinstance(right, dict):
                        left = left.format(**right)
                    else:
                        raise NovaError(
                            expr,
                            "'%' formatting is only available when right side is an array or object",
                        )
                    return left
                return left % right
            elif expr.operator == "^":
                return left ^ right
            elif expr.operator == "-":
                return left - right
            elif expr.operator == "*":
                if isinstance(left, (int, float)) and not isinstance(
                    right, (int, float)
                ):
                    raise NovaError(
                        expr,
                        f"If you wanted to repeat '{right}' {left} times, you'd do '\"{right}\" * {left}'",
                    )
                return left * right
            elif expr.operator == "**":
                return left**right
            elif expr.operator == "/":
                if right == 0:
                    raise NovaError(expr, "Division by zero is not allowed.")
                return left / right
            elif expr.operator == "==":
                return left == right
            elif expr.operator == "!=":
                return left != right
            elif expr.operator == "<":
                return left < right
            elif expr.operator == ">":
                return left > right
            elif expr.operator == ">>":
                return left >> right
            elif expr.operator == "<<":
                return left << right
            elif expr.operator == "<=":
                return left <= right
            elif expr.operator == "between":
                if not isinstance(right, (list, tuple)) or len(right) != 2:
                    raise NovaError(expr, "'between' requires a range [low, high]")
                return left >= right[0] and left <= right[1]
            elif expr.operator == ">=":
                return left >= right
            elif expr.operator == "->":
                if not callable(right):
                    raise NovaError(expr, f"right side of pipe expr must be a function")
                return right(left)
            elif expr.operator == "=>":
                if not callable(right):
                    raise NovaError(expr, f"right side of pipe expr must be a function")
                clone = copy.deepcopy(left)
                if isinstance(clone, dict):
                    for k, v in clone.items():
                        clone[k] = right(v, k)
                elif isinstance(clone, list):
                    for i, v in enumerate(clone):
                        clone[i] = right(v, i)
                else:
                    raise NovaError(expr, "=> only works on arrays or objects")
                return clone
            else:
                raise NovaError(expr, f"Unknown binary operator: {expr.operator}")

        elif expr.type == "UnaryExpr":
            right = self.evaluate_expr(expr.right, env)
            if expr.operator == "-":
                if not isinstance(right, (int, float)):
                    raise NovaError(
                        expr,
                        f"Unary '-' operator can only be applied to numbers. Got: {type(right).__name__}",
                    )
                return -right
            elif expr.operator == "!":
                return not right
            elif expr.operator == "#":
                if isinstance(right, (list, str, dict)):
                    return len(right)
                elif hasattr(right, "__len__"):
                    return len(right)
                else:
                    raise NovaError(
                        expr, f"Unary '#' cannot be applied to {type(right).__name__}"
                    )

            else:
                raise NovaError(expr, f"Unknown unary operator: {expr.operator}")

        elif expr.type == "FuncCall":
            self.callStack.append(expr.name)
            try:
                func = env.get(expr.name, expr).value
                if not callable(func):
                    raise NovaError(expr, f"{expr.name} is not a function")
                args = []
                kwargs = {}
                for arg in expr.arguments:
                    if arg.type == "ExplodeExpr":
                        v = self.evaluate_expr(arg.args, env)
                        if isinstance(v, dict):
                            for name in v.keys():
                                kwargs[name] = v[name]
                        elif isinstance(v, Iterable) and not isinstance(
                            v, (str, bytes)
                        ):
                            args.extend(v)
                        else:
                            args.append(v)
                    elif arg.type == "NamedArg":
                        kwargs[arg.name] = self.evaluate_expr(arg.value, env)
                    else:
                        v = self.evaluate_expr(arg, env)
                        args.append(v)
                return func(*args, **kwargs)
            except Exception as E:
                self.errorStack.append(
                    " " * len(self.callStack)
                    + f"- {expr.file}:{expr.line}:{expr.column}: "
                    + "Error while executing: "
                    + expr.name
                )
                raise E
            finally:
                self.callStack.pop()

        elif expr.type == "MethodCall":
            obj = self.evaluate_expr(expr.object, env)
            self._check_private_access(obj, expr.method, expr)
            if obj is None:
                raise NovaError(expr, f"Cannot call method '{expr.method}' on None.")

            args = []
            kwargs = {}
            for arg in expr.arguments:
                if arg.type == "ExplodeExpr":
                    v = self.evaluate_expr(arg.args, env)
                    if isinstance(v, dict):
                        for name in v.keys():
                            kwargs[name] = v[name]
                    elif isinstance(v, Iterable) and not isinstance(v, (str, bytes)):
                        args.extend(v)
                    else:
                        args.append(v)
                elif arg.type == "NamedArg":
                    kwargs[arg.name] = self.evaluate_expr(arg.value, env)
                else:
                    v = self.evaluate_expr(arg, env)
                    args.append(v)

            # Handle NovaClass static methods
            if isinstance(obj, NovaClass):
                static_method = obj.static_members.get(expr.method)
                if callable(static_method):
                    return static_method(*args, **kwargs)
                raise NovaError(
                    expr,
                    f"Static method '{expr.method}' not found or is not a function on class '{obj.name}'.",
                )

            fn = None
            # If obj is a NovaScript instance (represented as a Python dictionary)
            if isinstance(obj, dict) and expr.method in obj:
                fn = obj[expr.method]
            # If obj is an Environment (e.g., 'self' within a NovaScript method)
            elif isinstance(obj, Environment):
                # FIX: Get value from var
                fn = obj.get(expr.method, expr).value
            # For regular Python objects exposed to NovaScript
            else:
                fn = getattr(obj, expr.method, None)

            if not callable(fn):
                raise NovaError(
                    expr, f"{expr.method} is not a function or method on this object"
                )

            # Push class context if the method has a defining clas attribute (NovaScript bound method)
            if isinstance(obj, dict) and "__DefiningClass" in obj:
                # Push the defining class before calling the method
                self.current_class_stack.append(obj["__DefiningClass"])
                try:
                    result = fn(*args, **kwargs)
                finally:
                    self.current_class_stack.pop()
                return result
            else:
                try:
                    if hasattr(fn, "__DefiningClass"):
                        self.current_class_stack.append(fn.__DefiningClass)
                    result = fn(*args, **kwargs)
                finally:
                    if hasattr(fn, "__DefiningClass"):
                        self.current_class_stack.pop()
                return result

        elif expr.type == "ArrayAccess":
            arr = self.evaluate_expr(expr.object, env)
            index = self.evaluate_expr(expr.index, env)
            if not isinstance(arr, (list, dict, str, tuple)) and not hasattr(
                arr, "__getitem__"
            ):
                raise NovaError(
                    expr,
                    f"Cannot access index of non-list/dict/indexible: {type(arr).__name__}",
                )
            if not isinstance(index, (int, str)):
                raise NovaError(
                    expr,
                    f"List/dict index must be a number or string. Got: {type(index).__name__}",
                )
            try:
                return arr[index]
            except (IndexError, KeyError):
                raise NovaError(
                    expr, f"Index/key '{index}' out of bounds or not found for object."
                )

        elif expr.type == "PropertyAccess":
            obj = self.evaluate_expr(expr.object, env)
            self._check_private_access(obj, expr.property, expr)

            if obj is None:
                raise NovaError(
                    expr, f"Cannot access property '{expr.property}' of None."
                )

            # Handle NovaClass static properties
            if isinstance(obj, NovaClass):
                if expr.property in obj.static_members:
                    return obj.static_members[expr.property]
                raise NovaError(
                    expr,
                    f"Static property '{expr.property}' not found on class '{obj.name}'.",
                )
            if isinstance(obj, dict) and expr.property in obj:
                val = obj[expr.property]
                if isinstance(val, Proxy):
                    if not val.get:
                        raise NovaError(
                            expr, f"Property '{expr.property}' has no getter."
                        )
                    cls = obj.get("__DefiningClass")
                    if cls:
                        try:
                            self.current_class_stack.append(cls)
                            return val.get()
                        finally:
                            self.current_class_stack.pop()
                    else:
                        return val.get()
                return val
            # If obj is a NovaScript instance (represented as a Python dictionary)
            # if isinstance(obj, dict) and expr.property in obj:
            #    return obj[expr.property]
            # If obj is an Environment (e.g., 'self' within a NovaScript method)
            elif isinstance(obj, Environment):
                # FIX: Get value from var
                return obj.get(expr.property, expr).value
            # For regular Python objects (e.g., exposed modules, native types)
            elif hasattr(obj, expr.property):
                return getattr(obj, expr.property)

            raise NovaError(expr, f"Property '{expr.property}' not found on object.")

        elif expr.type == "ArrayLiteral":
            return [self.evaluate_expr(element, env) for element in expr.elements]

        elif expr.type == "ObjectLiteral":
            obj = {}
            for prop in expr.properties:
                obj[prop["key"]] = self.evaluate_expr(prop["value"], env)
            return obj

        elif expr.type == "NewInstance":
            # Resolve the fully qualified class name string
            name_parts = expr.class_name.split(".")
            current_resolved_object = env  # Start resolution from current environment

            for i, part in enumerate(name_parts):
                if isinstance(current_resolved_object, Environment):
                    # FIX: Get value from var
                    var_obj = current_resolved_object.get(part, expr)
                    next_resolved_part = var_obj.value
                elif isinstance(current_resolved_object, dict):
                    next_resolved_part = current_resolved_object.get(part)
                elif hasattr(current_resolved_object, part):
                    next_resolved_part = getattr(current_resolved_object, part)
                else:
                    raise NovaError(
                        expr,
                        f"Cannot resolve part '{part}' in class path '{expr.class_name}'. Previous part was type {type(current_resolved_object).__name__}.",
                    )

                if next_resolved_part is None:
                    raise NovaError(
                        expr, f"Class '{expr.class_name}' part '{part}' not found."
                    )

                current_resolved_object = next_resolved_part

            target_class = current_resolved_object
            args = [self.evaluate_expr(arg, env) for arg in expr.arguments]
            c = None
            if isinstance(target_class, NovaClass):
                c = target_class.instantiate(args)
            elif isinstance(target_class, type) and hasattr(
                target_class, "__init__"
            ):  # It's a Python class
                # For Python classes, we instantiate them directly.
                try:
                    c = target_class(*args)
                except Exception as e:
                    raise NovaError(
                        expr,
                        f"Error instantiating Python class '{expr.class_name}' (resolved to {target_class}): {e}",
                    )
            else:
                raise NovaError(
                    expr,
                    f"'{expr.class_name}' (resolved to {target_class}) is not a constructible class.",
                )
            if CUSTOM_TYPES.get(expr.class_name):
                check_type(
                    expr.class_name,
                    c,
                    expr,
                )
            return c
        elif expr.type == "LambdaDecl":
            # A lambda is just an unnamed / uuid-named function.
            # Same creation path as FuncDecl; class context is captured
            # automatically by _create_callable.
            return self._create_callable(
                expr.parameters,
                expr.body,
                env,
                name=None,  # → random uuid
                node=expr,
                context_name="lambda",
            )
        elif expr.type == "DecoratorExpr":
            # print(expr.__dict__)
            value = self.evaluate_expr(expr.expr, env)
            if not callable(value):
                raise NovaError(expr.expr, "Expected to return a function")
            # print(f"{value = }")
            body = self.evaluate_expr(expr.body, env)

            if not callable(body):
                raise NovaError(expr.body, "Expected to be a function")
            # print(f"{body = }")
            val = value(body)
            # print(f"{val = }")
            return val
        elif expr.type == "EnumDef":
            v = Environment()  # no sense copying env here
            v.values = {}
            for i in range(len(expr.values)):
                v.define(expr.values[i], i)

            v.lock()
            return v
        else:
            raise NovaError(expr, f"Unknown expression type: {expr}")
