import os
import sys
import json
import math
import re
import datetime
import time
from urllib.parse import unquote, quote

# --- Exceptions ---
class ReturnException(Exception):
    """A special exception used to implement returning values from functions."""
    def __init__(self, value):
        super().__init__("Return")
        self.value = value

class BreakException(Exception):
    """Special exception to implement break."""
    def __init__(self):
        super().__init__("Break")

class ContinueException(Exception):
    """Special exception to implement continue."""
    def __init__(self):
        super().__init__("Continue")

class NovaError(Exception):
    """Custom error class for NovaScript, including location information."""
    def __init__(self, token, user_message=None):
        # Provide fallback values if token is None or lacks properties
        file = getattr(token, 'file', "unknown_file") if token else "unknown_file"
        line = getattr(token, 'line', 0) if token else 0
        column = getattr(token, 'column', 0) if token else 0
        token_value = getattr(token, 'value', 'N/A') if token else 'N/A'

        message = f"{file}:{line}:{column} {user_message or 'unknown error at token: ' + str(token_value)}"
        super().__init__(message)
        self.line = line
        self.column = column
        self.file = file

# --- AST Node Base Classes ---
class Token:
    def __init__(self, type, value, file, line, column):
        self.type = type
        self.value = value
        self.file = file
        self.line = line
        self.column = column

class Statement(Token):
    def __init__(self, type, file, line, column):
        super().__init__(type, None, file, line, column)

class Expression(Token):
    def __init__(self, type, file, line, column):
        super().__init__(type, None, file, line, column)

# --- Specific AST Node Classes ---
class Literal(Expression):
    def __init__(self, value, file, line, column):
        super().__init__("Literal", file, line, column)
        self.value = value

class Identifier(Expression):
    def __init__(self, name, file, line, column):
        super().__init__("Identifier", file, line, column)
        self.name = name

class BinaryExpr(Expression):
    def __init__(self, operator, left, right, file, line, column):
        super().__init__("BinaryExpr", file, line, column)
        self.operator = operator
        self.left = left
        self.right = right

class UnaryExpr(Expression):
    def __init__(self, operator, right, file, line, column):
        super().__init__("UnaryExpr", file, line, column)
        self.operator = operator
        self.right = right

class FuncCall(Expression):
    def __init__(self, name, arguments, file, line, column):
        super().__init__("FuncCall", file, line, column)
        self.name = name
        self.arguments = arguments

class MethodCall(Expression):
    def __init__(self, object, method, arguments, file, line, column):
        super().__init__("MethodCall", file, line, column)
        self.object = object
        self.method = method
        self.arguments = arguments

class PropertyAccess(Expression):
    def __init__(self, object, property, file, line, column):
        super().__init__("PropertyAccess", file, line, column)
        self.object = object
        self.property = property

class ArrayAccess(Expression):
    def __init__(self, object, index, file, line, column):
        super().__init__("ArrayAccess", file, line, column)
        self.object = object
        self.index = index

class ArrayLiteral(Expression):
    def __init__(self, elements, file, line, column):
        super().__init__("ArrayLiteral", file, line, column)
        self.elements = elements

class ObjectLiteral(Expression):
    def __init__(self, properties, file, line, column):
        super().__init__("ObjectLiteral", file, line, column)
        self.properties = properties

class AssignmentExpr(Expression):
    def __init__(self, target, value, operator, file, line, column):
        super().__init__("AssignmentExpr", file, line, column)
        self.target = target
        self.value = value
        self.operator = operator

class NewInstance(Expression):
    def __init__(self, class_name, arguments, file, line, column):
        super().__init__("NewInstance", file, line, column)
        self.class_name = class_name
        self.arguments = arguments

class Parameter(Token):
    def __init__(self, name, annotation_type, default, file, line, column, type, value):
        super().__init__(type, value, file, line, column)
        self.name = name
        self.annotation_type = annotation_type
        self.default = default

class Case:
    def __init__(self, case_expr, body):
        self.case_expr = case_expr
        self.body = body

class DeferStmt(Statement):
    def __init__(self, body, file, line, column):
        super().__init__("DeferStmt", file, line, column)
        self.body = body

class VarDeclStmt(Statement):
    def __init__(self, name, type_annotation, initializer, file, line, column):
        super().__init__("VarDecl", file, line, column)
        self.name = name
        self.type_annotation = type_annotation
        self.initializer = initializer

class CustomTypeProperty:
    def __init__(self, name, type):
        self.name = name
        self.type = type

class CustomType:
    def __init__(self, name, properties, file, line, column):
        self.name = name
        self.properties = properties
        self.file = file
        self.line = line
        self.column = column

class CustomTypeDeclStmt(Statement):
    def __init__(self, name, definition, file, line, column):
        super().__init__("CustomTypeDecl", file, line, column)
        self.name = name
        self.definition = definition

class ClassDefinition(Statement):
    def __init__(self, name, superclass_name, body, file, line, column):
        super().__init__("ClassDefinition", file, line, column)
        self.name = name
        self.superclass_name = superclass_name
        self.body = body

class MethodDefinition(Statement):
    def __init__(self, name, parameters, body, is_static, is_constructor, file, line, column):
        super().__init__("MethodDefinition", file, line, column)
        self.name = name
        self.parameters = parameters
        self.body = body
        self.is_static = is_static
        self.is_constructor = is_constructor

class PropertyDefinition(Statement):
    def __init__(self, name, type_annotation, initializer, is_static, file, line, column):
        super().__init__("PropertyDefinition", file, line, column)
        self.name = name
        self.type_annotation = type_annotation
        self.initializer = initializer
        self.is_static = is_static

class ExpressionStmt(Statement):
    def __init__(self, expression, file, line, column):
        super().__init__("ExpressionStmt", file, line, column)
        self.expression = expression

class BreakStmt(Statement):
    def __init__(self, file, line, column):
        super().__init__("BreakStmt", file, line, column)

class ContinueStmt(Statement):
    def __init__(self, file, line, column):
        super().__init__("ContinueStmt", file, line, column)

class TryStmt(Statement):
    def __init__(self, try_block, error_var, catch_block, file, line, column):
        super().__init__("TryStmt", file, line, column)
        self.try_block = try_block
        self.error_var = error_var
        self.catch_block = catch_block

class IfStmt(Statement):
    def __init__(self, condition, then_block, else_block, else_if, file, line, column):
        super().__init__("IfStmt", file, line, column)
        self.condition = condition
        self.then_block = then_block
        self.else_block = else_block
        self.else_if = else_if

class WhileStmt(Statement):
    def __init__(self, condition, body, file, line, column):
        super().__init__("WhileStmt", file, line, column)
        self.condition = condition
        self.body = body

class ForEachStmt(Statement):
    def __init__(self, variable, list, body, file, line, column):
        super().__init__("ForEachStmt", file, line, column)
        self.variable = variable
        self.list = list
        self.body = body

class ForStmt(Statement):
    def __init__(self, variable, start, end, step, body, file, line, column):
        super().__init__("ForStmt", file, line, column)
        self.variable = variable
        self.start = start
        self.end = end
        self.step = step
        self.body = body

class ImportStmt(Statement):
    def __init__(self, filename, alias, file, line, column):
        super().__init__("ImportStmt", file, line, column)
        self.filename = filename
        self.alias = alias

class NamespaceStmt(Statement):
    def __init__(self, name, body, file, line, column):
        super().__init__("NamespaceStmt", file, line, column)
        self.name = name
        self.body = body

class SwitchStmt(Statement):
    def __init__(self, expression, cases, file, line, column):
        super().__init__("SwitchStmt", file, line, column)
        self.expression = expression
        self.cases = cases

class ReturnStmt(Statement):
    def __init__(self, expression, file, line, column):
        super().__init__("ReturnStmt", file, line, column)
        self.expression = expression

class FuncDecl(Statement):
    def __init__(self, name, parameters, body, file, line, column):
        super().__init__("FuncDecl", file, line, column)
        self.name = name
        self.parameters = parameters
        self.body = body

class LambdaDecl(Expression): # LambdaDecl is an expression that evaluates to a function
    def __init__(self, parameters, body, file, line, column):
        super().__init__("LambdaDecl", file, line, column)
        self.parameters = parameters
        self.body = body

class UsingStmt(Statement):
    def __init__(self, name, file, line, column):
        super().__init__("UsingStmt", file, line, column)
        self.name = name

# --- Environment for variable scoping ---
class Environment:
    def __init__(self, parent=None):
        self.values = {}
        self.parent = parent
        self.deferred = []

    def add_deferred(self, stmt):
        self.deferred.append(stmt)

    def execute_deferred(self, interpreter):
        # Execute in reverse order of deferral
        while self.deferred:
            stmt = self.deferred.pop()
            interpreter.execute_stmt(stmt, self)

    def define(self, name, value):
        self.values[name] = value

    def has(self, name):
        if name in self.values:
            return True
        elif self.parent:
            return self.parent.has(name)
        else:
            return False

    def assign(self, name, value, tok):
        if name in self.values:
            self.values[name] = value
        elif self.parent:
            self.parent.assign(name, value, tok)
        else:
            raise NovaError(tok, f"Undefined variable {name}")

    def get(self, name, tok=None):
        if name in self.values:
            return self.values[name]
        elif self.parent:
            return self.parent.get(name, tok)
        else:
            if tok:
                raise NovaError(tok, f"Undefined variable {name}")
            raise Exception(f"Undefined variable {name}") # Fallback for internal errors without token

# --- Runtime representation of a NovaScript class ---
class NovaClass:
    def __init__(self, name, super_class, interpreter, env):
        self.name = name
        self.super_class = super_class
        self.interpreter = interpreter
        self.env = env
        self.static_members = {}
        self.instance_properties = {} # Stores PropertyDefinition AST nodes
        self.instance_methods = {}    # Stores MethodDefinition AST nodes
        self.constructor_def = None

    def instantiate(self, args, instance_token, instance_obj=None):
        instance = instance_obj if instance_obj is not None else {}

        # First, handle superclass construction if applicable and this is the top-level call
        if self.super_class and instance_obj is None: # Only call superclass constructor if this is the initial instantiation
            self.super_class.instantiate(args, instance_token, instance)

        # Initialize instance properties
        for prop_name, prop_def in self.instance_properties.items():
            prop_value = None
            if prop_def.initializer:
                # Evaluate initializer in the context of the class definition environment
                prop_value = self.interpreter.evaluate_expr(prop_def.initializer, self.env)
            instance[prop_name] = prop_value # Default value if no initializer is None in Python

        # Bind instance methods to the instance
        for method_name, method_def in self.instance_methods.items():
            # Create a Python function that wraps the NovaScript method body
            def nova_method(*method_args, _method_def=method_def, _method_name=method_name): # Capture method_def and method_name
                method_env = Environment(self.env) # Method's scope
                method_env.define("self", instance) # 'self' refers to the instance

                # Handle 'super' call within instance methods
                if self.super_class:
                    def super_call(*super_method_args, _method_name=_method_name): # Capture method_name for super
                        super_method = self.super_class.instance_methods.get(_method_name)
                        if not super_method:
                            raise NovaError(instance_token, f"Method '{_method_name}' not found in superclass '{self.super_class.name}'.")

                        def temp_super_func(*temp_args):
                            temp_super_env = Environment(self.super_class.env)
                            temp_super_env.define("self", instance) # 'self' still refers to the current instance
                            for i, param in enumerate(super_method.parameters):
                                arg_val = temp_args[i] if i < len(temp_args) else None
                                if arg_val is None and param.default is not None:
                                    arg_val = self.interpreter.evaluate_expr(param.default, self.super_class.env)
                                elif arg_val is None and param.default is None:
                                    raise NovaError(param, f"Missing argument for parameter '{param.name}' in super method '{_method_name}'.")
                                if param.annotation_type:
                                    check_type(param.annotation_type, arg_val, param, self.interpreter)
                                temp_super_env.define(param.name, arg_val)
                            try:
                                self.interpreter.execute_block(super_method.body, temp_super_env)
                            except ReturnException as e:
                                return e.value
                            return None # NovaScript functions return undefined if no explicit return
                        return temp_super_func(*super_method_args)
                    method_env.define("super", super_call)

                # Handle parameters for current method
                for i, param in enumerate(_method_def.parameters):
                    arg_val = method_args[i] if i < len(method_args) else None
                    if arg_val is None and param.default is not None:
                        arg_val = self.interpreter.evaluate_expr(param.default, self.env) # Evaluate default in class's env
                    elif arg_val is None and param.default is None:
                        raise NovaError(param, f"Missing argument for parameter '{param.name}' in method '{_method_name}'.")
                    if param.annotation_type:
                        check_type(param.annotation_type, arg_val, param, self.interpreter)
                    method_env.define(param.name, arg_val)

                try:
                    self.interpreter.execute_block(_method_def.body, method_env)
                except ReturnException as e:
                    return e.value
                return None # NovaScript functions return undefined if no explicit return

            instance[method_name] = nova_method

        # Handle constructor call (only if this is the top-level instantiation)
        if instance_obj is None:
            if self.constructor_def:
                constructor_env = Environment(self.env)
                constructor_env.define("self", instance)
                # Define 'super' for constructor's environment
                if self.super_class:
                    def super_constructor_call(*super_args):
                        if not self.super_class: # Should not happen given the check above
                            raise NovaError(instance_token, "Cannot call super() in a class without a superclass.")
                        self.super_class.instantiate(super_args, instance_token, instance) # Pass current instance
                    constructor_env.define("super", super_constructor_call)

                # Handle constructor parameters
                for i, param in enumerate(self.constructor_def.parameters):
                    arg_val = args[i] if i < len(args) else None
                    if arg_val is None and param.default is not None:
                        arg_val = self.interpreter.evaluate_expr(param.default, self.env)
                    elif arg_val is None and param.default is None:
                        raise NovaError(param, f"Missing argument for parameter '{param.name}' in constructor of '{self.name}'.")
                    if param.annotation_type:
                        check_type(param.annotation_type, arg_val, param, self.interpreter)
                    constructor_env.define(param.name, arg_val)

                try:
                    self.interpreter.execute_block(self.constructor_def.body, constructor_env)
                except ReturnException:
                    # Constructors don't typically return values, but if they do, ignore it.
                    pass
        return instance

# --- Helper function for type checking ---
def check_type(expected, value, token, interpreter):
    actual_expected = expected
    if expected == "bool":
        actual_expected = "boolean" # Map 'bool' from parser to 'boolean' for Python type checking

    if actual_expected == "number":
        if not isinstance(value, (int, float)):
            raise NovaError(token, f"Type mismatch: expected number, got {type(value).__name__}")
    elif actual_expected == "string":
        if not isinstance(value, str):
            raise NovaError(token, f"Type mismatch: expected string, got {type(value).__name__}")
    elif actual_expected == "boolean":
        if not isinstance(value, bool):
            raise NovaError(token, f"Type mismatch: expected boolean, got {type(value).__name__}")
    elif actual_expected == "function":
        if not callable(value):
            raise NovaError(token, f"Type mismatch: expected function, got {type(value).__name__}")
    elif actual_expected == "void":
        if value is not None:
            raise NovaError(token, f"Type mismatch: expected void, got {type(value).__name__}")
    else:
        # Check for custom types
        custom_type_definition = interpreter.custom_types.get(expected) # Use original 'expected' for lookup
        if custom_type_definition:
            if not isinstance(value, dict) or value is None:
                raise NovaError(token, f"Type mismatch: expected custom type '{expected}', got {type(value).__name__}")
            for prop_def in custom_type_definition.properties:
                if prop_def.name not in value:
                    raise NovaError(token, f"Type mismatch: custom type '{expected}' is missing property '{prop_def.name}'")
                # Recursively check property type
                check_type(prop_def.type, value[prop_def.name], token, interpreter)
        else:
            raise NovaError(token, f"Unknown type: {expected}")
    return value

# --- Global Initialization ---
def init_globals(globals_env):
    globals_env.define('print', print)
    runtime_version = {
        "major": 0,
        "minor": 7,
        "patch": 5
    }

    # Create a dummy token for internal/bootstrap errors in init_globals
    internal_token = Token("internal", "init", "internal_init.py", 1, 1)

    globals_env.define("isArray", lambda obj: isinstance(obj, list))

    class Logger:
        @staticmethod
        def info(*args):
            now = datetime.datetime.now()
            print(f"[info {globals_env.get('Runtime', internal_token).versionString}| at: {now.hour}:{now.minute}:{now.second}]:", *args)
        @staticmethod
        def warn(*args):
            now = datetime.datetime.now()
            print(f"[warn {globals_env.get('Runtime', internal_token).versionString}| at: {now.hour}:{now.minute}:{now.second}]:", *args, file=sys.stderr)
        @staticmethod
        def error(*args):
            now = datetime.datetime.now()
            print(f"[error {globals_env.get('Runtime', internal_token).versionString}| at: {now.hour}:{now.minute}:{now.second}]:", *args, file=sys.stderr)
    globals_env.define("Logger", Logger)

    class Is:
        @staticmethod
        def string(s): return isinstance(s, str)
        @staticmethod
        def number(n): return isinstance(n, (int, float))
        @staticmethod
        def boolean(b): return isinstance(b, bool)
    globals_env.define("is", Is)

    #args = sys.argv[2:] # Skip script name and NovaScript file name

    globals_env.define("json", json)

    class Parse:
        @staticmethod
        def int(s): return int(s)
        @staticmethod
        def float(s): return float(s)
        @staticmethod
        def str(s): return str(s)
        @staticmethod
        def bool(s): return bool(s)
    globals_env.define("parse", Parse)

    globals_env.define("math", math)
    globals_env.define("null", None)
    globals_env.define("undefined", None)
    globals_env.define("void", None)
    globals_env.define("NaN", float('nan'))

    class Runtime:
        @staticmethod
        def dump_keys(obj): return list(obj.keys())
        @staticmethod
        def dump_values(obj): return list(obj.values())
        dump = {
            "keys": dump_keys,
            "values": dump_values
        }
        version = runtime_version
        versionString = f"v{runtime_version['major']}.{runtime_version['minor']}.{runtime_version['patch']}"
        currentDirectory = os.getcwd
        @staticmethod
        def regex(pattern, options=""): return re.compile(pattern, 0 if 'i' not in options else re.IGNORECASE)
        args = sys.argv[2:]
        @staticmethod
        def exit(code=0): sys.exit(code)
        @staticmethod
        def versionAtLeast(maj, min, pat):
            if runtime_version['major'] > maj: return True
            if runtime_version['major'] < maj: return False
            if runtime_version['minor'] > min: return True
            if runtime_version['minor'] < min: return False
            return runtime_version['patch'] >= pat
        @staticmethod
        def env(key=None):
            if key: return os.environ.get(key)
            return dict(os.environ) # Return a copy of the environment variables
        @staticmethod
        def throw(reason, *rest):
            if rest:
                reason = reason.format(*rest) # Pythonic way to format
            runtime_throw_token = Token("runtime", "throw", "runtime_internal.py", 0, 0)
            raise NovaError(runtime_throw_token, reason)

        class Fs:
            @staticmethod
            def read(path):
                with open(path, 'r', encoding='utf8') as f:
                    return f.read()
            @staticmethod
            def write(path, contents):
                with open(path, 'w', encoding='utf8') as f:
                    f.write(contents)
            @staticmethod
            def exists(path):
                return os.path.exists(path)
        fs = Fs

        class Uri:
            @staticmethod
            def decode(s): return unquote(s)
            @staticmethod
            def encode(s): return quote(s)
        URI = Uri

        class Time:
            @staticmethod
            def now(): return int(time.time() * 1000) # Milliseconds since epoch
            @staticmethod
            def str(): return str(datetime.datetime.now())
            @staticmethod
            def hrtime(): return str(time.perf_counter_ns()) # High-resolution time in nanoseconds
        time = Time
    globals_env.define('Runtime', Runtime)


class Interpreter:
    keywords = [
        "var", "if", "else", "elseif", "end", "break", "continue", "func",
        "return", "import", "as", "namespace", "while", "forEach", "for",
        "do", "in", "test", "failed", "defer",
        "switch", "case", "default", "using",
        "def",
        "define",
        "class", "inherits", "static", "new", "super"
    ]

    def __init__(self, file_path):
        self.file = file_path
        with open(file_path, "r", encoding="utf8") as f:
            self.source = f.read()

        self.tokens = self.tokenize(self.source, self.file)
        self.current = 0
        self.globals = Environment()
        self.functions = {} # Not used directly in TS, but keeping for consistency if needed
        self.imported_files = set()
        self.current_env = None
        self.custom_types = {} # Dictionary for custom types

        init_globals(self.globals)
        self.globals.define("__SCRIPT_PATH__", os.path.dirname(os.path.abspath(self.file)))

    def expose_js_class(self, name, py_class):
        """Exposes a Python class to the NovaScript environment."""
        if not (isinstance(py_class, type) and hasattr(py_class, '__init__')):
            raise TypeError(f"Cannot expose '{name}': Provided value is not a valid Python class.")
        self.globals.define(name, py_class)

    # --- Tokenization ---
    def tokenize(self, source, file):
        tokens = []
        i = 0
        line = 1
        col = 1
        length = len(source)

        while i < length:
            char = source[i]
            start_col = col
            next_char = source[i + 1] if i + 1 < length else ''

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
            if char == "/" and next_char == "/": # Single-line comment
                while i < length and source[i] != "\n":
                    i += 1
                continue
            if char == "/" and next_char == "*": # Multi-line comment
                i += 2
                col += 2
                while i < length and not (source[i] == "*" and i + 1 < length and source[i + 1] == "/"):
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
                    raise NovaError(Token("error", "Unterminated comment", file, line, col), "Unterminated multi-line comment")
                continue

            # --- Operators (prioritize longer matches) ---
            matched_operator = False
            two_char_ops = ["+=", "-=", "*=", "/=", "%=", "==", "!=", "<=", ">=", "&&", "||"]
            current_two_char = char + next_char

            if current_two_char in two_char_ops:
                tokens.append(Token("operator", current_two_char, file, line, start_col))
                i += 2
                col += 2
                matched_operator = True
            else:
                # Single-character operators that might be part of two-char ops, or stand alone
                potential_single_ops = "=+-*/%<>!."
                if char in potential_single_ops:
                    tokens.append(Token("operator", char, file, line, start_col))
                    i += 1
                    col += 1
                    matched_operator = True
                else:
                    # Other single-character punctuation/operators
                    other_single_ops = "()[]{},:#"
                    if other_single_ops.find(char) != -1: # Use find for string check
                        tokens.append(Token("operator", char, file, line, start_col))
                        i += 1
                        col += 1
                        matched_operator = True

            if matched_operator:
                continue

            # Numbers (supporting decimals)
            if char.isdigit():
                num = ""
                while i < length and (source[i].isdigit() or source[i] == '.'):
                    num += source[i]
                    i += 1
                    col += 1
                tokens.append(Token("number", float(num) if '.' in num else int(num), file, line, start_col))
                continue

            # Strings: delimited by double quotes.
            if char == '"':
                i += 1
                str_val = ""
                while i < length and source[i] != '"':
                    if source[i] == "\\" and i + 1 < length:
                        i += 1
                        if source[i] == "n": str_val += "\n"
                        elif source[i] == "t": str_val += "\t"
                        elif source[i] == "r": str_val += "\r"
                        elif source[i] == "\\": str_val += "\\"
                        elif source[i] == '"': str_val += '"'
                        else: str_val += source[i] # Unrecognized escape sequence, just add char
                    else:
                        str_val += source[i]
                    i += 1
                    col += 1
                    if source[i-1] == "\n": # If escaped newline or just newline in string
                        col = 1
                        line += 1
                if i < length and source[i] == '"': # Consume closing quote
                    i += 1
                    col += 1
                else:
                    raise NovaError(Token("error", "Unterminated string", file, line, col), "Unterminated string literal")
                tokens.append(Token("string", str_val, file, line, start_col))
                continue

            # Identifiers, keywords, booleans.
            if char.isalpha() or char == '_':
                id_str = ""
                while i < length and (source[i].isalnum() or source[i] == '_'):
                    id_str += source[i]
                    i += 1
                    col += 1
                if id_str == "true" or id_str == "false":
                    tokens.append(Token("boolean", id_str == "true", file, line, start_col))
                elif id_str in self.keywords:
                    tokens.append(Token("keyword", id_str, file, line, start_col))
                else:
                    tokens.append(Token("identifier", id_str, file, line, start_col))
                continue

            raise NovaError(Token("error", char, file, line, col), f"Unexpected character: {char}")
        return tokens

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
            raise NovaError(token, f"Expected token type {type_str}, got {token.type if token else 'EOF'}")
        return token

    def expect_token(self, value):
        token = self.get_next_token()
        if not token:
            last_token = self.tokens[self.current - 1] if self.current > 0 else Token("EOF", "EOF", self.file, 1, 1)
            raise NovaError(last_token, f"Expected token '{value}', got EOF")
        if token.value != value:
            raise NovaError(token, f"Expected token '{value}', got {token.value}")
        return token

    # --- Parsing Helpers ---
    def parse_block_until(self, terminators=None):
        if terminators == None:
            terminators = []
        statements = []
        while self.current < len(self.tokens):
            token = self.get_next_token()
            if not token:
                last_token = self.tokens[self.current - 1] if self.current > 0 else Token("EOF", "EOF", self.file, 1, 1)
                raise NovaError(last_token, "Unexpected end of input")
            if (token.type == "keyword" and token.value in terminators) or \
               (token.type == "operator" and token.value in terminators):
                break
            stmt = self.parse_statement()
            if stmt: # CustomTypeDeclStmt returns None
                statements.append(stmt)
        return statements

    def parse_block(self):
        return self.parse_block_until()

    def consume_expected(self, type_str):
        self.expect_type(type_str)
        return self.consume_token()

    def parse_class_definition(self):
        class_token = self.consume_token() # consume 'class'
        name_token = self.expect_type("identifier")
        name = name_token.value
        self.consume_token() # consume class name

        superclass_name = None
        if self.get_next_token() and self.get_next_token().value == "inherits":
            self.consume_token() # consume 'inherits'
            super_name_token = self.expect_type("identifier")
            superclass_name = super_name_token.value
            self.consume_token() # consume superclass name

        has_initializer = False
        body = []
        while self.get_next_token() and self.get_next_token().value != "end":
            member_token = self.get_next_token()
            is_static = False
            if member_token.type == "keyword" and member_token.value == "static":
                self.consume_token() # consume 'static'
                is_static = True

            if self.get_next_token() and self.get_next_token().type == "keyword" and self.get_next_token().value == "var":
                # Property Definition
                self.consume_token() # consume 'var'
                prop_name_token = self.expect_type("identifier")
                prop_name = prop_name_token.value
                self.consume_token()

                type_annotation = None
                if self.get_next_token() and self.get_next_token().type == "operator" and self.get_next_token().value == ":":
                    self.consume_token() # consume ':'
                    type_token = self.expect_type("identifier")
                    type_annotation = type_token.value
                    self.consume_token()

                initializer = None
                if self.get_next_token() and self.get_next_token().type == "operator" and self.get_next_token().value == "=":
                    self.consume_token() # consume '='
                    initializer = self.parse_expression()
                body.append(PropertyDefinition(
                    prop_name, type_annotation, initializer, is_static,
                    prop_name_token.file, prop_name_token.line, prop_name_token.column
                ))
            elif self.get_next_token() and self.get_next_token().type == "keyword" and self.get_next_token().value == "func":
                # Method Definition
                self.consume_token() # consume 'func'
                method_name_token = self.expect_type("identifier")
                method_name = method_name_token.value
                self.consume_token()

                is_constructor = False
                if method_name == "init":
                    is_constructor = True
                    has_initializer = True

                self.expect_token("(")
                self.consume_token()

                parameters = []
                if self.get_next_token() and self.get_next_token().value != ")":
                    while True:
                        param_token = self.expect_type("identifier")
                        param_name = param_token.value
                        self.consume_token()

                        annotation_type = None
                        if self.get_next_token() and self.get_next_token().type == "identifier":
                            type_token = self.get_next_token()
                            if type_token.value in ["string", "number", "bool", "boolean"] or type_token.value in self.custom_types:
                                annotation_type = type_token.value
                                self.consume_token()

                        default_expr = None
                        if self.get_next_token() and self.get_next_token().value == "=":
                            self.consume_token()
                            default_expr = self.parse_expression()

                        parameters.append(Parameter(
                            param_name, annotation_type, default_expr,
                            param_token.file, param_token.line, param_token.column,
                            param_token.type, param_token.value
                        ))

                        if self.get_next_token() and self.get_next_token().value == ",":
                            self.consume_token()
                        else:
                            break
                self.expect_token(")")
                self.consume_token()

                body_statements = self.parse_block_until(["end"])
                self.expect_token("end")
                self.consume_token()

                body.append(MethodDefinition(
                    method_name, parameters, body_statements, is_static, is_constructor,
                    method_name_token.file, method_name_token.line, method_name_token.column
                ))
            else:
                raise NovaError(member_token, f"Unexpected token in class body: {member_token.value}")

        if not has_initializer:
            raise NovaError(class_token, "this class has no `init` initializer function")

        self.expect_token("end")
        self.consume_token() # consume 'end'

        return ClassDefinition(
            name, superclass_name, body,
            class_token.file, class_token.line, class_token.column
        )

    # --- Parsing Statements and Expressions ---
    def parse_statement(self):
        token = self.get_next_token()
        if not token:
            last_token = self.tokens[self.current - 1] if self.current > 0 else Token("EOF", "EOF", self.file, 1, 1)
            raise NovaError(last_token, "Unexpected end of input")

        if token.type == "keyword":
            if token.value == "defer":
                self.consume_token()
                defer_body = self.parse_block_until(["end"])
                self.expect_token("end")
                self.consume_token()
                return DeferStmt(defer_body, token.file, token.line, token.column)

            elif token.value == "define":
                self.consume_token() # consume 'type'
                custom_type_name_token = self.expect_type("identifier")
                custom_type_name = custom_type_name_token.value
                self.consume_token() # consume type name

                self.expect_token("=")
                self.consume_token() # consume '='

                self.expect_token("{")
                self.consume_token() # consume '{'

                properties = []
                while self.get_next_token() and self.get_next_token().value != "}":
                    prop_name_token = self.expect_type("identifier")
                    prop_name = prop_name_token.value
                    self.consume_token() # consume property name

                    self.expect_token(":")
                    self.consume_token() # consume ':'

                    prop_type_token = self.expect_type("identifier")
                    prop_type = prop_type_token.value
                    self.consume_token() # consume property type

                    properties.append(CustomTypeProperty(prop_name, prop_type))

                    if self.get_next_token() and self.get_next_token().value == ",":
                        self.consume_token() # consume ','
                    else:
                        break

                self.expect_token("}")
                self.consume_token() # consume '}'
                custom_type_stmt = CustomTypeDeclStmt(
                    custom_type_name, properties,
                    token.file, token.line, token.column
                )
                self.custom_types[custom_type_stmt.name] = CustomType(
                    custom_type_stmt.name, custom_type_stmt.definition,
                    custom_type_stmt.file, custom_type_stmt.line, custom_type_stmt.column
                )
                return None # the statement is not handled at runtime, don't return it

            elif token.value == "var":
                self.consume_token()
                var_name_token = self.expect_type("identifier")
                var_name = var_name_token.value
                self.consume_token()

                var_annotation_type = None
                if (self.get_next_token() and
                    self.get_next_token().type == "identifier" and
                    (self.get_next_token().value in ["string", "number", "boolean", "void", "bool"] or
                    self.get_next_token().value in self.custom_types)): # Parentheses for clarity
                    var_annotation_type = self.get_next_token().value
                    self.consume_token()

                self.expect_token("=")
                self.consume_token()
                var_initializer = self.parse_expression()
                return VarDeclStmt(var_name, var_annotation_type, var_initializer, token.file, token.line, token.column)

            elif token.value == "switch":
                self.consume_token()
                switch_expr = self.parse_expression()
                cases = []
                while self.get_next_token() and self.get_next_token().type == "keyword" and self.get_next_token().value == "case":
                    self.consume_token()
                    case_expr = self.parse_expression()
                    self.expect_token("do")
                    self.consume_token()
                    case_body = self.parse_block_until(["end"])
                    self.expect_token("end")
                    self.consume_token()
                    cases.append(Case(case_expr, case_body))
                if self.get_next_token() and self.get_next_token().type == "keyword" and self.get_next_token().value == "default":
                    self.consume_token()
                    self.expect_token("do")
                    self.consume_token()
                    default_body = self.parse_block_until(["end"])
                    self.expect_token("end")
                    self.consume_token()
                    cases.append(Case(None, default_body)) # None for default case_expr
                self.expect_token("end")
                self.consume_token()
                return SwitchStmt(switch_expr, cases, token.file, token.line, token.column)

            elif token.value == "using":
                self.consume_token()
                # Parse the fully qualified name, e.g., "module.sub.name"
                name_parts = []
                name_part_token = self.expect_type("identifier")
                name_parts.append(name_part_token.value)
                self.consume_token() # Consume the first identifier

                while self.get_next_token() and self.get_next_token().value == ".":
                    self.consume_token() # Consume the dot
                    name_part_token = self.expect_type("identifier")
                    name_parts.append(name_part_token.value)
                    self.consume_token() # Consume the next identifier

                full_name = ".".join(name_parts)
                return UsingStmt(full_name, token.file, token.line, token.column)

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
                return TryStmt(try_block, error_var, catch_block, token.file, token.line, token.column)

            elif token.value == "forEach":
                self.consume_token()
                for_each_var_token = self.expect_type("identifier")
                for_each_variable = for_each_var_token.value
                self.consume_token()
                self.expect_token("in")
                self.consume_token()
                for_each_list_expr = self.parse_expression()
                self.expect_token("do")
                self.consume_token()
                for_each_body = self.parse_block_until(["end"])
                self.expect_token("end")
                self.consume_token()
                return ForEachStmt(for_each_variable, for_each_list_expr, for_each_body, token.file, token.line, token.column)

            elif token.value == "for":
                self.consume_token()
                for_var_token = self.expect_type("identifier")
                for_variable = for_var_token.value
                self.consume_token()
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
                return ForStmt(for_variable, for_start_expr, for_end_expr, for_step_expr, for_body, token.file, token.line, token.column)

            elif token.value == "while":
                self.consume_token()
                while_condition = self.parse_expression()
                while_body = self.parse_block_until(["end"])
                self.expect_token("end")
                self.consume_token()
                return WhileStmt(while_condition, while_body, token.file, token.line, token.column)

            elif token.value == "break":
                self.consume_token()
                return BreakStmt(token.file, token.line, token.column)

            elif token.value == "continue":
                self.consume_token()
                return ContinueStmt(token.file, token.line, token.column)

            elif token.value == "if":
                self.consume_token()
                if_condition = self.parse_expression()
                then_block = self.parse_block_until(["else", "elseif", "end"])

                else_if_blocks = []
                else_block = None

                while self.get_next_token() and self.get_next_token().type == "keyword" and self.get_next_token().value == "elseif":
                    self.consume_token()
                    elseif_condition = self.parse_expression()
                    elseif_body = self.parse_block_until(["else", "elseif", "end"])
                    else_if_blocks.append({"condition": elseif_condition, "body": elseif_body})

                if self.get_next_token() and self.get_next_token().type == "keyword" and self.get_next_token().value == "else":
                    self.consume_token()
                    else_block = self.parse_block_until(["end"])

                self.expect_token("end")
                self.consume_token()

                return IfStmt(
                    if_condition, then_block, else_block,
                    else_if_blocks if else_if_blocks else None,
                    token.file, token.line, token.column
                )

            elif token.value == "namespace":
                self.consume_token()
                namespace_name_token = self.expect_type("identifier")
                namespace_name = namespace_name_token.value
                self.consume_token()
                namespace_body = self.parse_block_until(["end"])
                self.expect_token("end")
                self.consume_token()
                return NamespaceStmt(namespace_name, namespace_body, token.file, token.line, token.column)

            elif token.value == "func":
                self.consume_token()
                func_name_token = self.expect_type("identifier")
                func_name = func_name_token.value
                self.consume_token()
                self.expect_token("(")
                self.consume_token()

                func_parameters = []
                if self.get_next_token() and self.get_next_token().value != ")":
                    while True:
                        param_token = self.expect_type("identifier")
                        param_name = param_token.value
                        self.consume_token()

                        annotation_type = None
                        if self.get_next_token() and self.get_next_token().type == "identifier":
                            type_token = self.get_next_token()
                            if type_token.value in ["string", "number", "bool", "boolean"] or type_token.value in self.custom_types:
                                annotation_type = type_token.value
                                self.consume_token()

                        default_expr = None
                        if self.get_next_token() and self.get_next_token().value == "=":
                            self.consume_token()
                            default_expr = self.parse_expression()
                            if annotation_type:
                                raise NovaError(token, "Cannot have both explicit type annotation and a default value. Consider removing the type annotation to allow type inference from the default value.")

                            if default_expr.type == "Literal":
                                inferred_type = type(default_expr.value).__name__
                                if inferred_type == "str": annotation_type = "string"
                                elif inferred_type in ["int", "float"]: annotation_type = "number"
                                elif inferred_type == "bool": annotation_type = "bool"
                                else: annotation_type = None
                            else:
                                annotation_type = None

                        func_parameters.append(Parameter(
                            param_name, annotation_type, default_expr,
                            param_token.file, param_token.line, param_token.column,
                            param_token.type, param_token.value
                        ))

                        if self.get_next_token() and self.get_next_token().value == ",":
                            self.consume_token()
                        else:
                            break

                self.expect_token(")")
                self.consume_token()
                func_body = self.parse_block_until(["end"])
                self.expect_token("end")
                self.consume_token()

                return FuncDecl(
                    func_name, func_parameters, func_body,
                    token.file, token.line, token.column
                )

            elif token.value == "def": # Lambda statement (def (...) ... end)
                # This is a statement that *declares* a lambda and assigns it.
                # The TS code treats `def` as a keyword for both statement and expression.
                # Here, we'll parse it as a lambda expression and then wrap it in an ExpressionStmt.
                # The full `def` statement (e.g., `x = def() end`) is handled in parse_primary.
                # This `def` keyword here is for standalone lambda declarations, which is not typical in NovaScript.
                # Re-evaluating this based on the TS code:
                # The `def` keyword in `parseStatement` is for a standalone lambda declaration.
                # It seems like it's meant to define a lambda and then implicitly assign it or use it.
                # Given the TS code, it's parsed as a LambdaDecl statement.
                # For simplicity, if it's a standalone `def` statement, it should likely be assigned to a variable.
                # The TS code's `LambdaDecl` is an `Expression`.
                # So if `def` is used as a statement, it's likely an error or a lambda that's immediately executed.
                # Let's assume it's meant to be an expression that is then part of an ExpressionStmt.
                # The TS `parseStatement` calls `parseExpression` as a fallback.
                # So, the `def` keyword should only be handled in `parsePrimary` for expressions.
                # Removing the `def` case from `parse_statement` and relying on `parse_expression` fallback.
                pass # Fall through to expression statement

            elif token.value == "return":
                self.consume_token()
                return_expression = None
                # Check if the next token is not a keyword (indicating end of statement or start of expression)
                if self.get_next_token() and self.get_next_token().value not in self.keywords:
                    return_expression = self.parse_expression()
                return ReturnStmt(return_expression, token.file, token.line, token.column)

            elif token.value == "import":
                self.consume_token()
                file_token = self.expect_type("string")
                filename = file_token.value
                self.consume_token()
                alias = None
                if self.get_next_token() and self.get_next_token().value == "as":
                    self.consume_token()
                    alias_token = self.expect_type("identifier")
                    alias = alias_token.value
                    self.consume_token()
                return ImportStmt(filename, alias, token.file, token.line, token.column)

            elif token.value == "class":
                return self.parse_class_definition()

        # --- Expression statement (fallback) ---
        expr = self.parse_expression()
        return ExpressionStmt(expr, token.file, token.line, token.column)

    # --- Expression Parsing (Recursive Descent) ---
    def parse_expression(self):
        return self.parse_assignment()

    def parse_assignment(self):
        expr = self.parse_logical_or() # This is the left-hand side of the assignment
        next_token = self.get_next_token()

        # Check if the next token is an assignment operator (simple or compound)
        if (next_token and
            next_token.type == "operator" and
            next_token.value in ["=", "+=", "-=", "*=", "/=", "%="]):
            assignment_op_token = self.consume_token() # Consume the assignment operator token
            value_expr = self.parse_assignment() # Recursively parse the right-hand side

            # Ensure the target is something assignable
            if not isinstance(expr, (Identifier, PropertyAccess, ArrayAccess, ArrayLiteral)):
                raise NovaError(expr, f"Invalid assignment target: Cannot assign to {expr.type}")

            # Create the AssignmentExpr AST node
            return AssignmentExpr(
                expr, value_expr, assignment_op_token.value,
                assignment_op_token.file, assignment_op_token.line, assignment_op_token.column
            )
        return expr # If no assignment operator, it's just a logical OR expression

    def parse_logical_or(self):
        expr = self.parse_logical_and()
        while (self.get_next_token() and
               self.get_next_token().type == "operator" and
               self.get_next_token().value == "||"):
            operator_token = self.consume_token()
            operator = operator_token.value
            right = self.parse_logical_and()
            # Corrected: operator_token.token.line should be operator_token.line
            expr = BinaryExpr(operator, expr, right, operator_token.file, operator_token.line, operator_token.column)
        return expr

    def parse_logical_and(self):
        expr = self.parse_equality()
        while (self.get_next_token() and
               self.get_next_token().type == "operator" and
               self.get_next_token().value == "&&"):
            operator_token = self.consume_token()
            operator = operator_token.value
            right = self.parse_equality()
            expr = BinaryExpr(operator, expr, right, operator_token.file, operator_token.line, operator_token.column)
        return expr

    def parse_equality(self):
        expr = self.parse_comparison()
        while (self.get_next_token() and
               self.get_next_token().type == "operator" and
               self.get_next_token().value in ["==", "!="]):
            operator_token = self.consume_token()
            operator = operator_token.value
            right = self.parse_comparison()
            expr = BinaryExpr(operator, expr, right, operator_token.file, operator_token.line, operator_token.column)
        return expr

    def parse_comparison(self):
        expr = self.parse_term()
        while (self.get_next_token() and
               self.get_next_token().type == "operator" and
               self.get_next_token().value in ["<", "<=", ">", ">="]):
            operator_token = self.consume_token()
            operator = operator_token.value
            right = self.parse_term()
            expr = BinaryExpr(operator, expr, right, operator_token.file, operator_token.line, operator_token.column)
        return expr

    def parse_term(self):
        expr = self.parse_factor()
        while (self.get_next_token() and
               self.get_next_token().type == "operator" and
               self.get_next_token().value in ["+", "-"]):
            operator_token = self.consume_token()
            operator = operator_token.value
            right = self.parse_factor()
            expr = BinaryExpr(operator, expr, right, operator_token.file, operator_token.line, operator_token.column)
        return expr

    def parse_factor(self):
        expr = self.parse_unary()
        while (self.get_next_token() and
               self.get_next_token().type == "operator" and
               self.get_next_token().value in ["*", "/", "%"]):
            operator_token = self.consume_token()
            operator = operator_token.value
            right = self.parse_unary()
            expr = BinaryExpr(operator, expr, right, operator_token.file, operator_token.line, operator_token.column)
        return expr

    def parse_unary(self):
        if (self.get_next_token() and
            self.get_next_token().type == "operator" and
            self.get_next_token().value in ["-", "!"]):
            operator_token = self.consume_token()
            operator = operator_token.value
            right = self.parse_unary()
            return UnaryExpr(operator, right, operator_token.file, operator_token.line, operator_token.column)
        return self.parse_call_member_expression()

    def parse_call_member_expression(self):
        expr = self.parse_primary()

        while True:
            next_token = self.get_next_token()
            if not next_token:
                break

            if next_token.value == ".":
                dot_token = self.consume_token() # consume "."
                prop_token = self.expect_type("identifier")
                prop_name = prop_token.value
                self.consume_token() # consume identifier

                # Check for method call
                if self.get_next_token() and self.get_next_token().value == "(":
                    self.consume_token() # consume "("
                    args = []
                    if self.get_next_token() and self.get_next_token().value != ")":
                        while True:
                            args.append(self.parse_expression())
                            if self.get_next_token() and self.get_next_token().value == ",": self.consume_token()
                            else: break
                    self.expect_token(")")
                    self.consume_token() # consume ")"
                    expr = MethodCall(
                        expr, prop_name, args,
                        prop_token.file, prop_token.line, prop_token.column
                    )
                else:
                    # Plain property access
                    expr = PropertyAccess(
                        expr, prop_name,
                        prop_token.file, prop_token.line, prop_token.column
                    )
            elif next_token.value == "[":
                bracket_token = self.consume_token() # consume "["
                index_expr = self.parse_expression()
                self.expect_token("]")
                self.consume_token() # consume "]"
                expr = ArrayAccess(
                    expr, index_expr,
                    bracket_token.file, bracket_token.line, bracket_token.column
                )
            elif next_token.value == "(" and isinstance(expr, Identifier):
                # This handles direct function calls like `myFunc(arg)`
                self.consume_token() # consume "("
                args = []
                if self.get_next_token() and self.get_next_token().value != ")":
                    while True:
                        args.append(self.parse_expression())
                        if self.get_next_token() and self.get_next_token().value == ",":
                            self.consume_token()
                        else:
                            break
                self.expect_token(")")
                self.consume_token() # consume ")"
                expr = FuncCall(
                    expr.name, args,
                    expr.file, expr.line, expr.column
                )
            else:
                break # No more chained access/calls
        return expr

    def parse_primary(self):
        node = None
        token = self.get_next_token()
        if not token:
            last_token = self.tokens[self.current - 1] if self.current > 0 else Token("EOF", "EOF", self.file, 1, 1)
            raise NovaError(last_token, "Unexpected end of input")

        if token.type == "boolean":
            self.consume_token()
            node = Literal(token.value, token.file, token.line, token.column)
        elif token.type == "number" or token.type == "string":
            self.consume_token()
            node = Literal(token.value, token.file, token.line, token.column)
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
            if self.get_next_token() and self.get_next_token().value != "}":
                while True:
                    key_token = self.get_next_token()
                    if key_token.type not in ["identifier", "string"]:
                        raise NovaError(key_token, "Expected identifier or string as object key")
                    key = key_token.value
                    self.consume_token()
                    self.expect_token(":")
                    self.consume_token()
                    value = self.parse_expression()
                    properties.append({"key": key, "value": value})
                    if self.get_next_token() and self.get_next_token().value == ",":
                        self.consume_token()
                    else:
                        break
            self.expect_token("}")
            self.consume_token()
            node = ObjectLiteral(properties, token.file, token.line, token.column)
        elif token.type == "identifier":
            self.consume_token()
            node = Identifier(token.value, token.file, token.line, token.column)
        elif token.type == "keyword" and token.value == "new":
            self.consume_token() # consume 'new'
            class_name_token = self.expect_type("identifier")
            class_name = class_name_token.value
            self.consume_token() # consume class name

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
                class_name, args,
                class_name_token.file, class_name_token.line, class_name_token.column
            )
        elif token.value == "def": # Lambda expression (def (...) ... end)
            self.consume_token()
            self.expect_token("(")
            self.consume_token()

            parameters = []
            if self.get_next_token() and self.get_next_token().value != ")":
                while True:
                    param_token = self.expect_type("identifier")
                    param_name = param_token.value
                    self.consume_token()

                    annotation_type = None
                    if self.get_next_token() and self.get_next_token().type == "identifier":
                        type_token = self.get_next_token()
                        if type_token.value in ["string", "number", "bool", "boolean"] or type_token.value in self.custom_types:
                            annotation_type = type_token.value
                            self.consume_token()

                    default_expr = None
                    if self.get_next_token() and self.get_next_token().value == "=":
                        self.consume_token()
                        default_expr = self.parse_expression()
                        if annotation_type:
                            raise NovaError(token, "Cannot have both explicit type annotation and a default value. Consider removing the type annotation to allow type inference from the default value.")

                        if default_expr.type == "Literal":
                            inferred_type = type(default_expr.value).__name__
                            if inferred_type == "str": annotation_type = "string"
                            elif inferred_type in ["int", "float"]: annotation_type = "number"
                            elif inferred_type == "bool": annotation_type = "bool"
                            else: annotation_type = None
                        else:
                            annotation_type = None

                    parameters.append(Parameter(
                        param_name, annotation_type, default_expr,
                        param_token.file, param_token.line, param_token.column,
                        param_token.type, param_token.value
                    ))

                    if self.get_next_token() and self.get_next_token().value == ",":
                        self.consume_token()
                    else:
                        break

            self.expect_token(")")
            self.consume_token()
            body = self.parse_block_until(["end"])
            self.expect_token("end")
            self.consume_token()

            return LambdaDecl(
                parameters, body,
                token.file, token.line, token.column
            )
        elif token.value == "(":
            self.consume_token()
            node = self.parse_expression()
            self.expect_token(")")
            self.consume_token()
        else:
            raise NovaError(token, f"Unexpected token: {token.value}")

        return node

    # --- Evaluation / Execution ---
    def interpret(self):
        statements = self.parse_block()
        try:
            self.execute_block(statements, self.globals)
        except Exception as err:
            # Ensure deferred statements still execute
            self.globals.execute_deferred(self)
            raise err

    def execute_block(self, statements, env):
        previous_env = self.current_env
        self.current_env = env

        try:
            for stmt in statements:
                self.execute_stmt(stmt, env)
        finally:
            # Execute deferred statements in reverse order
            env.execute_deferred(self)
            self.current_env = previous_env

    def get_current_context(self):
        return self.current_env

    def execute_stmt(self, stmt, env):
        if stmt.type == "VarDecl":
            value = self.evaluate_expr(stmt.initializer, env)
            if stmt.type_annotation:
                check_type(stmt.type_annotation, value, stmt, self)
            env.define(stmt.name, value)

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
            raise BreakException()

        elif stmt.type == "ContinueStmt":
            raise ContinueException()

        elif stmt.type == "TryStmt":
            try:
                self.execute_block(stmt.try_block, env)
            except Exception as e:
                catch_env = Environment(env)
                # If e is a NovaError, pass it. Otherwise, wrap it.
                error_to_define = e if isinstance(e, NovaError) else NovaError(stmt, str(e))
                catch_env.define(stmt.error_var, error_to_define)
                self.execute_block(stmt.catch_block, catch_env)

        elif stmt.type == "IfStmt":
            condition = self.evaluate_expr(stmt.condition, env)
            if condition:
                self.execute_block(stmt.then_block, Environment(env))
            elif stmt.else_if:
                matched = False
                # Create a new environment for the elseif chain
                # The TS code creates a new env for elseif chain, then new env for block.
                # This seems to imply the elseif chain itself shares an env, but its blocks get new ones.
                # Let's simplify: each block gets its own new env.
                for elseif_block in stmt.else_if:
                    elseif_condition = self.evaluate_expr(elseif_block["condition"], env)
                    if elseif_condition:
                        self.execute_block(elseif_block["body"], Environment(env))
                        matched = True
                        break
                if not matched and stmt.else_block:
                    self.execute_block(stmt.else_block, Environment(env))
            elif stmt.else_block:
                self.execute_block(stmt.else_block, Environment(env))

        elif stmt.type == "WhileStmt":
            while self.evaluate_expr(stmt.condition, env):
                try:
                    self.execute_block(stmt.body, Environment(env))
                except BreakException:
                    break
                except ContinueException:
                    continue

        elif stmt.type == "ForEachStmt":
            list_val = self.evaluate_expr(stmt.list, env)
            if not isinstance(list_val, list):
                raise NovaError(stmt, f"Cannot iterate over non-array type for forEach loop. Got: {type(list_val).__name__}")
            for item in list_val:
                loop_env = Environment(env)
                loop_env.define(stmt.variable, item)
                try:
                    self.execute_block(stmt.body, loop_env)
                except BreakException:
                    break
                except ContinueException:
                    continue

        elif stmt.type == "ForStmt":
            start = self.evaluate_expr(stmt.start, env)
            end = self.evaluate_expr(stmt.end, env)
            step = self.evaluate_expr(stmt.step, env) if stmt.step else 1

            if not all(isinstance(val, (int, float)) for val in [start, end, step]):
                raise NovaError(stmt, f"For loop bounds and step must be numbers. Got start: {type(start).__name__}, end: {type(end).__name__}, step: {type(step).__name__}")

            # Python's range handles step correctly. For float steps, manual loop.
            # Assuming integer steps for now, as floats can lead to precision issues.
            # If NovaScript intends float steps, this will need adjustment.
            if isinstance(step, float):
                current_val = start
                while (step > 0 and current_val <= end) or (step < 0 and current_val >= end):
                    loop_env = Environment(env)
                    loop_env.define(stmt.variable, current_val)
                    try:
                        self.execute_block(stmt.body, loop_env)
                    except BreakException:
                        break
                    except ContinueException:
                        current_val += step
                        continue
                    current_val += step
            else: # Integer step
                for i in range(start, end + (1 if step > 0 else -1), step):
                    loop_env = Environment(env)
                    loop_env.define(stmt.variable, i)
                    try:
                        self.execute_block(stmt.body, loop_env)
                    except BreakException:
                        break
                    except ContinueException:
                        continue

        elif stmt.type == "ImportStmt":
            file_path = stmt.filename
            if file_path.startswith("os:"):
                file_path = file_path[3:]
                if not env.has("os-import-handler"):
                    raise NovaError(stmt, "os-import-handler is not defined, your runtime should define it, interpreter.globals.define('os-import-handler', handler)")
                handler = env.get("os-import-handler", stmt)
                result = handler(file_path)
                name = stmt.alias or os.path.basename(file_path)
                env.define(name, result)
                return

            file_path += ".nova"
            file_dir = os.path.dirname(self.file)
            full_path = os.path.abspath(os.path.join(file_dir, file_path))

            if full_path in self.imported_files:
                return # Already imported

            self.imported_files.add(full_path)

            if not os.path.exists(full_path):
                raise NovaError(stmt, f"Import error: File not found at '{full_path}'")

            imported_interpreter = Interpreter(full_path)
            # Share the same custom_types map
            imported_interpreter.custom_types = self.custom_types

            imported_env = Environment(self.globals)
            imported_interpreter.globals = imported_env # Overwrite globals with the new imported_env
            imported_interpreter.functions = self.functions # Share functions, though not directly used in TS
            imported_interpreter.imported_files = self.imported_files # Share imported files set

            imported_interpreter.interpret()

            namespace = {}
            for key, value in imported_env.values.items():
                namespace[key] = value

            module_name = os.path.splitext(os.path.basename(file_path))[0]
            name = stmt.alias or module_name
            env.define(name, namespace)

        elif stmt.type == "NamespaceStmt":
            n_env = Environment(env)
            self.execute_block(stmt.body, n_env)
            env.define(stmt.name, n_env)

        elif stmt.type == "SwitchStmt":
            value = self.evaluate_expr(stmt.expression, env)
            matched = False
            default_case_body = None

            for case in stmt.cases:
                if case.case_expr is None: # This is the default case
                    default_case_body = case.body
                else:
                    case_val = self.evaluate_expr(case.case_expr, env)
                    if value == case_val:
                        self.execute_block(case.body, Environment(env))
                        matched = True
                        break # Exit switch after first match

            if not matched and default_case_body:
                self.execute_block(default_case_body, Environment(env))

        elif stmt.type == "ReturnStmt":
            value = self.evaluate_expr(stmt.expression, env) if stmt.expression else None
            raise ReturnException(value)

        elif stmt.type == "FuncDecl":
            def func_wrapper(*args):
                func_env = Environment(env)

                for i, param in enumerate(stmt.parameters):
                    arg_val = args[i] if i < len(args) else None

                    # apply default if missing
                    if arg_val is None and param.default is not None:
                        arg_val = self.evaluate_expr(param.default, env)
                    elif arg_val is None and param.default is None:
                        raise NovaError(param, f"Missing argument for parameter '{param.name}' in function '{stmt.name}'.")

                    # soft type check
                    if param.annotation_type:
                        check_type(param.annotation_type, arg_val, param, self)

                    func_env.define(param.name, arg_val)

                try:
                    self.execute_block(stmt.body, func_env)
                except ReturnException as e:
                    return e.value
                return None # Functions without explicit return return None (undefined)

            env.define(stmt.name, func_wrapper)

        elif stmt.type == "ClassDefinition":
            class_def = stmt
            nova_class = NovaClass(class_def.name, None, self, env) # Superclass will be resolved later

            # Populate static members, instance properties/methods
            for member in class_def.body:
                if member.type == "PropertyDefinition":
                    if member.is_static:
                        prop_value = None
                        if member.initializer:
                            prop_value = self.evaluate_expr(member.initializer, env)
                        nova_class.static_members[member.name] = prop_value
                    else:
                        nova_class.instance_properties[member.name] = member
                elif member.type == "MethodDefinition":
                    if member.is_constructor:
                        nova_class.constructor_def = member
                    elif member.is_static:
                        # Wrap static methods
                        def static_method_wrapper(*args, _member=member): # Capture member
                            method_env = Environment(env) # Static methods run in the class's definition environment
                            method_env.define("self", nova_class.static_members) # 'self' refers to the static members map
                            # Handle parameters and execute body
                            for i, param in enumerate(_member.parameters):
                                arg_val = args[i] if i < len(args) else None
                                if arg_val is None and param.default is not None:
                                    arg_val = self.evaluate_expr(param.default, env)
                                elif arg_val is None and param.default is None:
                                    raise NovaError(param, f"Missing argument for parameter '{param.name}' in static method '{_member.name}'.")
                                if param.annotation_type:
                                    check_type(param.annotation_type, arg_val, param, self)
                                method_env.define(param.name, arg_val)
                            try:
                                self.execute_block(_member.body, method_env)
                            except ReturnException as e:
                                return e.value
                            return None
                        nova_class.static_members[member.name] = static_method_wrapper
                    else:
                        nova_class.instance_methods[member.name] = member

            # Resolve superclass if exists
            if class_def.superclass_name:
                super_class = env.get(class_def.superclass_name, class_def)
                if not isinstance(super_class, NovaClass):
                    raise NovaError(class_def, f"Superclass '{class_def.superclass_name}' not found or is not a class.")
                nova_class.super_class = super_class

            env.define(class_def.name, nova_class)

        elif stmt.type == "UsingStmt":
            # Resolve the fully qualified namespace name
            name_parts = stmt.name.split('.')
            current_resolved_object = env # Start resolution from current environment

            for i, part in enumerate(name_parts):
                if isinstance(current_resolved_object, Environment):
                    # NovaScript Environment: use its get method
                    next_resolved_part = current_resolved_object.get(part, stmt)
                elif isinstance(current_resolved_object, dict):
                    # Python dictionary (e.g., NovaScript object literal, or imported dict-like module): use dict access
                    next_resolved_part = current_resolved_object.get(part)
                elif hasattr(current_resolved_object, part):
                    # Python object (e.g., imported Python module): use getattr
                    next_resolved_part = getattr(current_resolved_object, part)
                else:
                    raise NovaError(stmt, f"Cannot resolve part '{part}' in '{'.'.join(name_parts[:i])}'. Not a namespace, dict, or object with attribute.")

                if next_resolved_part is None:
                    raise NovaError(stmt, f"Namespace '{stmt.name}' part '{part}' not found.")

                current_resolved_object = next_resolved_part

            # After resolving the full path, current_resolved_object holds the target namespace/object
            target_namespace = current_resolved_object

            if isinstance(target_namespace, Environment):
                # Import from NovaScript Environment
                for key, value in target_namespace.values.items():
                    env.define(key, value)
            elif isinstance(target_namespace, dict):
                # Import from Python dictionary
                for key, value in target_namespace.items():
                    env.define(key, value)
            elif hasattr(target_namespace, '__dict__'):
                # Import from Python module or class instance (its __dict__)
                # Filter out built-in/private attributes if desired, or just import all.
                # For simplicity, let's import all public attributes.
                for key, value in target_namespace.__dict__.items():
                    if not key.startswith('_'): # Avoid importing Python's internal/private attributes
                        env.define(key, value)
            else:
                raise NovaError(stmt, f"Cannot 'use' value of type {type(target_namespace).__name__}. Expected a namespace, dict, or Python object with attributes.")

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
            if not isinstance(base_object, (list, dict)): # Python lists/dicts for arrays/objects
                raise NovaError(target_expr, f"Cannot assign to index of non-list/dict: {type(base_object).__name__}")
            if not isinstance(index, (int, str)):
                raise NovaError(target_expr, f"List/dict index must be a number or string for assignment. Got: {type(index).__name__}")
            return {"base": base_object, "final_key": index}
        elif isinstance(target_expr, PropertyAccess):
            # Recursively evaluate the object part to get the actual object
            base_object = self.evaluate_expr(target_expr.object, env)
            key = target_expr.property # Property name is a string

            if base_object is None:
                raise NovaError(target_expr, f"Cannot assign property '{key}' of None value.")

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
            raise NovaError(target_expr, "Invalid assignment target type: " + target_expr.type)

    def evaluate_expr(self, expr, env):
        if expr.type == "Literal":
            return expr.value
        elif expr.type == "Identifier":
            return env.get(expr.name, expr)
        elif expr.type == "AssignmentExpr":
            target = expr.target
            assigned_value = self.evaluate_expr(expr.value, env)
            op = expr.operator

            final_value_to_assign = assigned_value

            # Handle compound assignments (if operator is not just "=")
            if op != "=":
                if isinstance(target, ArrayLiteral):
                    raise NovaError(target, f"Compound assignment operators like '{op}' cannot be used with array destructuring.")

                current_value = self.evaluate_expr(target, env)

                if op == "+=": final_value_to_assign = current_value + assigned_value
                elif op == "-=": final_value_to_assign = current_value - assigned_value
                elif op == "*=": final_value_to_assign = current_value * assigned_value
                elif op == "/=":
                    if assigned_value == 0:
                        raise NovaError(expr, "Division by zero in compound assignment.")
                    final_value_to_assign = current_value / assigned_value
                elif op == "%=": final_value_to_assign = current_value % assigned_value
                else:
                    raise NovaError(expr, f"Internal error: Unknown compound assignment operator: {op}")

            # Now, perform the actual assignment with final_value_to_assign
            if isinstance(target, ArrayLiteral): # Array destructuring
                source_array = final_value_to_assign
                if not isinstance(source_array, list):
                    raise NovaError(target, f"Cannot destructure non-list value. Expected list, got {type(source_array).__name__}.")
                for i, target_element in enumerate(target.elements):
                    source_value = source_array[i] if i < len(source_array) else None # Handle fewer elements in source
                    # Recursively assign each element using a simple assignment
                    temp_assignment = AssignmentExpr(
                        target_element,
                        Literal(source_value, expr.file, expr.line, expr.column),
                        "=", # Always simple assignment for destructuring elements
                        expr.file, expr.line, expr.column
                    )
                    self.evaluate_expr(temp_assignment, env)
            else: # All other assignment targets (Identifier, ArrayAccess, PropertyAccess)
                resolved = self.resolve_assignment_target(target, env)
                base = resolved["base"]
                final_key = resolved["final_key"]
                if isinstance(base, Environment):
                    base.assign(final_key, final_value_to_assign, target)
                elif final_key is not None:
                    # For NovaScript instances (Python dictionaries), assign directly
                    if isinstance(base, dict):
                        base[final_key] = final_value_to_assign
                    else: # For other Python objects, use setattr if it's an attribute
                        try:
                            setattr(base, final_key, final_value_to_assign)
                        except AttributeError:
                             # Fallback for dicts that are not NovaScript instances but are being assigned to
                             # This handles object literals or other dicts where direct key assignment is expected
                            if isinstance(base, dict):
                                base[final_key] = final_value_to_assign
                            else:
                                raise NovaError(target, f"Cannot assign to property '{final_key}' of object of type {type(base).__name__}.")
                else:
                    raise NovaError(target, "Failed to resolve assignment target.")
            return final_value_to_assign # Return the value that was assigned

        elif expr.type == "BinaryExpr":
            left = self.evaluate_expr(expr.left, env)
            right = self.evaluate_expr(expr.right, env)
            if expr.operator == "+": return left + right
            elif expr.operator == "%": return left % right
            elif expr.operator == "-": return left - right
            elif expr.operator == "*": return left * right
            elif expr.operator == "/":
                if right == 0:
                    raise NovaError(expr, "Division by zero is not allowed.")
                return left / right
            elif expr.operator == "==": return left == right
            elif expr.operator == "!=": return left != right
            elif expr.operator == "<": return left < right
            elif expr.operator == "<=": return left <= right
            elif expr.operator == ">": return left > right
            elif expr.operator == ">=": return left >= right
            elif expr.operator == "&&": return left and right
            elif expr.operator == "||": return left or right
            else:
                raise NovaError(expr, f"Unknown binary operator: {expr.operator}")

        elif expr.type == "UnaryExpr":
            right = self.evaluate_expr(expr.right, env)
            if expr.operator == "-":
                if not isinstance(right, (int, float)):
                    raise NovaError(expr, f"Unary '-' operator can only be applied to numbers. Got: {type(right).__name__}")
                return -right
            elif expr.operator == "!": return not right
            else:
                raise NovaError(expr, f"Unknown unary operator: {expr.operator}")

        elif expr.type == "FuncCall":
            func = env.get(expr.name, expr)
            if not callable(func):
                raise NovaError(expr, f"{expr.name} is not a function")
            args = [self.evaluate_expr(arg, env) for arg in expr.arguments]
            return func(*args)

        elif expr.type == "MethodCall":
            obj = self.evaluate_expr(expr.object, env)

            if obj is None:
                raise NovaError(expr, f"Cannot call method '{expr.method}' on None.")

            arg_vals = [self.evaluate_expr(arg, env) for arg in expr.arguments]

            # Handle NovaClass static methods
            if isinstance(obj, NovaClass):
                static_method = obj.static_members.get(expr.method)
                if callable(static_method):
                    return static_method(*arg_vals)
                raise NovaError(expr, f"Static method '{expr.method}' not found or is not a function on class '{obj.name}'.")

            fn = None
            # If obj is a NovaScript instance (represented as a Python dictionary)
            if isinstance(obj, dict) and expr.method in obj:
                fn = obj[expr.method]
            # If obj is an Environment (e.g., 'self' within a NovaScript method)
            elif isinstance(obj, Environment):
                fn = obj.get(expr.method, expr)
            # For regular Python objects exposed to NovaScript
            else:
                fn = getattr(obj, expr.method, None)

            if not callable(fn):
                raise NovaError(expr, f"{expr.method} is not a function or method on this object")

            # Call the method.
            # If it's a Python method, it's already bound. If it's a NovaScript method, it's a closure.
            return fn(*arg_vals)

        elif expr.type == "ArrayAccess":
            arr = self.evaluate_expr(expr.object, env)
            index = self.evaluate_expr(expr.index, env)
            if not isinstance(arr, (list, dict)):
                raise NovaError(expr, f"Cannot access index of non-list/dict: {type(arr).__name__}")
            if not isinstance(index, (int, str)):
                raise NovaError(expr, f"List/dict index must be a number or string. Got: {type(index).__name__}")
            try:
                return arr[index]
            except (IndexError, KeyError):
                raise NovaError(expr, f"Index/key '{index}' out of bounds or not found for object.")

        elif expr.type == "PropertyAccess":
            obj = self.evaluate_expr(expr.object, env)

            if obj is None:
                raise NovaError(expr, f"Cannot access property '{expr.property}' of None.")

            # Handle NovaClass static properties
            if isinstance(obj, NovaClass):
                if expr.property in obj.static_members:
                    return obj.static_members[expr.property]
                raise NovaError(expr, f"Static property '{expr.property}' not found on class '{obj.name}'.")

            # If obj is a NovaScript instance (represented as a Python dictionary)
            if isinstance(obj, dict) and expr.property in obj:
                return obj[expr.property]
            # If obj is an Environment (e.g., 'self' within a NovaScript method)
            elif isinstance(obj, Environment):
                return obj.get(expr.property, expr)
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
            class_name = expr.class_name
            target_class = env.get(class_name, expr)

            args = [self.evaluate_expr(arg, env) for arg in expr.arguments]

            if isinstance(target_class, NovaClass):
                return target_class.instantiate(args, expr)
            elif isinstance(target_class, type) and hasattr(target_class, '__init__'): # It's a Python class
                try:
                    return target_class(*args)
                except Exception as e:
                    raise NovaError(expr, f"Error instantiating Python class '{class_name}': {e}")
            else:
                raise NovaError(expr, f"'{class_name}' is not a constructible class.")

        elif expr.type == "LambdaDecl":
            def lambda_func_wrapper(*args):
                func_env = Environment(env) # Closure over the environment where lambda was defined

                for i, param in enumerate(expr.parameters):
                    arg_val = args[i] if i < len(args) else None

                    # apply default if missing
                    if arg_val is None and param.default is not None:
                        arg_val = self.evaluate_expr(param.default, env) # Default evaluated in outer env
                    elif arg_val is None and param.default is None:
                        raise NovaError(param, f"Missing argument for parameter '{param.name}'.")

                    # soft type check
                    if param.annotation_type:
                        check_type(param.annotation_type, arg_val, param, self)

                    func_env.define(param.name, arg_val)

                try:
                    self.execute_block(expr.body, func_env)
                except ReturnException as e:
                    return e.value
                return None # Functions without explicit return return None

            return lambda_func_wrapper
        else:
            raise NovaError(expr, f"Unknown expression type: {expr.type}")

# Example usage (if this were a script):
# if __name__ == "__main__":
#     if len(sys.argv) < 2:
#         print("Usage: python nova_interpreter.py <nova_script_file.nova>")
#         sys.exit(1)
#
#     script_file = sys.argv[1]
#     try:
#         interpreter = Interpreter(script_file)
#         interpreter.interpret()
#     except NovaError as e:
#         print(f"NovaScript Error: {e}")
#         sys.exit(1)
#     except Exception as e:
#         print(f"Runtime Error: {e}")
#         sys.exit(1)