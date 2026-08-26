from typing import Any

from .nodes import NovaError
from .typechecker import BUILTIN_VAR_TYPES
from .novaenv import Environment
from .runtime import Interpreter
from .conf import LIBS_PATH
import importlib
import pathlib
impLib = importlib.import_module

import sys
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

    def shift(x: list[Any]):
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
        def toBytes(data, encoding:str|None=None):
            if isinstance(data, str):
                if not isinstance(encoding, str):
                    raise ValueError("expected encoding to be string")
                return bytes(data, encoding)
            return bytes(data)

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
        nova_path = pathlib.Path(path if path.endswith(".nova") else path + ".nova")

        possible_locations = [
            nova_path,
            pathlib.Path(interpreter.file).parent / nova_path,
            pathlib.Path.cwd() / nova_path,
            LIBS_PATH / nova_path

        ]

        file_path = None
        for candidate in possible_locations:
            if candidate.is_file():
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
                + "\n  ".join([str(x) for x in possible_locations])
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


            # interpreter turns nova's objects into dicts when passing back to python, so this is safe
    class Fs:
        @staticmethod
        def read(path:str, opts:dict[str,Any]|None=None):
            opts = opts or {"mode": "r", "encoding": "utf8"}
            with open(path, **opts) as f:
                return f.read()

        @staticmethod
        def open(path:str, opts:dict[str,Any]|None=None):
            opts = opts or {"mode": "r", "encoding": "utf8"}
            return open(path, **opts)

        @staticmethod
        def write(path:str, contents, opts:dict[str,Any]|None=None):
            opts = opts or {"mode": "w", "encoding": "utf8"}
            with open(path, **opts) as f:
                f.write(contents)
        @staticmethod
        def exists(path:str):
            return pathlib.Path(path).exists()

        @staticmethod
        def isdir(path:str):
            return pathlib.Path(path).is_dir()

        @staticmethod
        def listdir(path:str="."):
            return [p.name for p in pathlib.Path(path).iterdir()]

        @staticmethod
        def join(*parts):
            return str(pathlib.Path(*parts))
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

    builtin_values: dict[str, object] = {
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
