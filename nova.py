import os
import sys
import math
import json
import re
import time
import datetime
from dataclasses import dataclass, field
from typing import Any, List, Dict, Optional, Union, Callable

# --- Token and AST Node Definitions ---

@dataclass
class Token:
    type: str
    value: Any
    file: str
    line: int
    column: int

# Base interfaces for AST nodes
@dataclass
class Statement(Token):
    # type and value are inherited from Token
    pass

@dataclass
class Expression(Token):
    # type and value are inherited from Token
    pass

@dataclass
class Parameter(Token):
    name: str
    type: Optional[str]
    default: Optional[Expression] = None

@dataclass
class Case:
    case_expr: Any
    body: List[Statement]

# Specific Statement Types
@dataclass
class VarDecl(Statement):
    name: str
    type_annotation: Optional[str]
    initializer: Expression
    modifier: Optional[str] = None

@dataclass
class DeferStmt(Statement):
    body: List[Statement]

@dataclass
class ExpressionStmt(Statement):
    expression: Expression

@dataclass
class BreakStmt(Statement):
    pass

@dataclass
class ContinueStmt(Statement):
    pass

@dataclass
class TryStmt(Statement):
    try_block: List[Statement]
    error_var: str
    catch_block: List[Statement]

@dataclass
class IfStmt(Statement):
    condition: Expression
    then_block: List[Statement]
    else_block: Optional[List[Statement]] = None
    else_if: Optional[List[Dict[str, Any]]] = None # List of {condition, body}

@dataclass
class WhileStmt(Statement):
    condition: Expression
    body: List[Statement]

@dataclass
class ForEachStmt(Statement):
    variable: str
    list: Expression
    body: List[Statement]

@dataclass
class ForStmt(Statement):
    variable: str
    start: Expression
    end: Expression
    body: List[Statement]
    step: Optional[Expression] = None

@dataclass
class ImportStmt(Statement):
    filename: str
    alias: Optional[str] = None

@dataclass
class NamespaceStmt(Statement):
    name: str
    body: List[Statement]

@dataclass
class SwitchStmt(Statement):
    expression: Expression
    cases: List[Case]

@dataclass
class ReturnStmt(Statement):
    expression: Optional[Expression] = None

@dataclass
class FuncDecl(Statement):
    name: str
    parameters: List[Parameter]
    body: List[Statement]

@dataclass
class LambdaDecl(Statement): # Lambda is also a statement type in parsing, but evaluated as an expression
    parameters: List[Parameter]
    body: List[Statement]

@dataclass
class UsingStmt(Statement):
    name: str

@dataclass
class LabelStmt(Statement):
    name: str

# Specific Expression Types
@dataclass
class Literal(Expression):
    value: Any

@dataclass
class Identifier(Expression):
    name: str

@dataclass
class AssignmentExpr(Expression):
    target: Expression
    value: Expression

@dataclass
class BinaryExpr(Expression):
    operator: str
    left: Expression
    right: Expression

@dataclass
class UnaryExpr(Expression):
    operator: str
    right: Expression

@dataclass
class FuncCall(Expression):
    name: str
    arguments: List[Expression]

@dataclass
class MethodCall(Expression):
    object: Expression
    method: str
    arguments: List[Expression]

@dataclass
class PropertyAccess(Expression):
    object: Expression
    property: str

@dataclass
class ArrayAccess(Expression):
    name: str
    index: Expression

@dataclass
class ArrayLiteral(Expression):
    elements: List[Expression]

@dataclass
class ObjectLiteral(Expression):
    properties: List[Dict[str, Any]] # List of {'key': str, 'value': Expression}

# --- Custom Exceptions ---

class ReturnException(Exception):
    """A special exception used to implement returning values from functions."""
    def __init__(self, value: Any):
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
    """Custom error class for NovaScript, including file, line, and column information."""
    def __init__(self, token: Optional[Token], user_message: Optional[str] = None):
        file = token.file if token and token.file else "unknown_file"
        line = token.line if token else 0
        column = token.column if token else 0
        token_value = token.value if token and token.value is not None else 'N/A'

        message = f"{file}:{line}:{column} {user_message or 'unknown error at token: ' + str(token_value)}"
        super().__init__(message)
        self.line = line
        self.column = column
        self.file = file

# --- Environment for Scoping ---

class Environment:
    """A simple environment for variable scoping."""
    def __init__(self, parent: Optional['Environment'] = None):
        self.values: Dict[str, Any] = {}
        self.parent = parent
        self.deferred: List[Statement] = []

    def add_deferred(self, stmt: Statement):
        self.deferred.append(stmt)

    def execute_deferred(self, interpreter: 'Interpreter'):
        # Execute deferred statements in reverse order of addition
        while self.deferred:
            stmt = self.deferred.pop()
            interpreter.execute_stmt(stmt, self)

    def define(self, name: str, value: Any):
        self.values[name] = value

    def has(self, name: str) -> bool:
        if name in self.values:
            return True
        elif self.parent:
            return self.parent.has(name)
        else:
            return False

    def assign(self, name: str, value: Any, tok: Token):
        if name in self.values:
            self.values[name] = value
        elif self.parent:
            self.parent.assign(name, value, tok)
        else:
            raise NovaError(tok, f"Undefined variable {name}")

    def get(self, name: str, tok: Optional[Token] = None) -> Any:
        if name in self.values:
            return self.values[name]
        elif self.parent:
            return self.parent.get(name, tok)
        else:
            if tok:
                raise NovaError(tok, f"Undefined variable {name}")
            raise Exception(f"Undefined variable {name}") # Fallback for internal errors without a token

# --- Global Initialization ---

def init_globals(globals_env: Environment):
    """Initializes the global environment with built-in functions and objects."""
    globals_env.define('print', print)

    runtime_version = {
        "major": 0,
        "minor": 7,
        "patch": 0
    }

    # Create a dummy token for internal/bootstrap errors in init_globals
    internal_token = Token(type="internal", value="init", file="internal_init.py", line=1, column=1)

    globals_env.define("isArray", lambda x: isinstance(x, list))
    # 'new' functionality is typically handled by direct class instantiation in Python,
    # but for a dynamic language, we might pass Python classes.
    # For simplicity, we'll assume 'className' is an actual Python class.
    globals_env.define("new", lambda class_name, *args: class_name(*args))

    # Logger
    class Logger:
        @staticmethod
        def _log_prefix(level: str, runtime_info: Any):
            now = datetime.datetime.now()
            return f"[{level} {runtime_info.get('versionString', 'N/A')}| at: {now.hour}:{now.minute}:{now.second}]:"

        def info(self, *args):
            print(self._log_prefix("info", globals_env.get("Runtime", internal_token)), *args)

        def warn(self, *args):
            print(self._log_prefix("warn", globals_env.get("Runtime", internal_token)), *args, file=sys.stderr)

        def error(self, *args):
            print(self._log_prefix("error", globals_env.get("Runtime", internal_token)), *args, file=sys.stderr)

    globals_env.define("Logger", Logger())

    # Type checking utilities
    globals_env.define("is", {
        "string": lambda s: isinstance(s, str),
        "number": lambda n: isinstance(n, (int, float)),
        "boolean": lambda b: isinstance(b, bool),
    })

    # Command-line arguments
    args = sys.argv[2:] # Equivalent to process.argv.slice(2)

    globals_env.define("json", json)
    globals_env.define("parse", {
        "int": int,
        "float": float,
        "str": str,
        "bool": bool
    })

    globals_env.define("math", math)
    globals_env.define("null", None)
    globals_env.define("undefined", None) # Python's None serves for both null and undefined
    globals_env.define("NaN", float('nan'))

    # Runtime object
    class Runtime:
        @staticmethod
        def dump_keys(obj):
            return list(obj.keys())

        @staticmethod
        def dump_values(obj):
            return list(obj.values())

        @staticmethod
        def current_directory():
            return os.getcwd()

        @staticmethod
        def regex(pattern: str, options: str = ""):
            flags = 0
            if 'i' in options: flags |= re.IGNORECASE
            if 'm' in options: flags |= re.MULTILINE
            # Add other flags as needed
            return re.compile(pattern, flags)

        @staticmethod
        def exit(code: int = 0):
            sys.exit(code)

        @staticmethod
        def version_at_least(maj: int, min: int, pat: int) -> bool:
            if runtime_version["major"] > maj: return True
            if runtime_version["major"] < maj: return False
            if runtime_version["minor"] > min: return True
            if runtime_version["minor"] < min: return False
            return runtime_version["patch"] >= pat

        @staticmethod
        def env(key: Optional[str] = None):
            if key: return os.environ.get(key)
            return dict(os.environ) # Return a copy of the environment variables

        @staticmethod
        def throw(reason: Any, *rest: Any):
            if rest:
                reason = str(reason).format(*rest) # Simple string formatting
            # For Runtime.throw, we don't have a direct token. Use a generic one.
            runtime_throw_token = Token(type="runtime", value="throw", file="runtime_internal.py", line=0, column=0)
            raise NovaError(runtime_throw_token, reason)

        class FS:
            @staticmethod
            def read(file_path: str):
                with open(file_path, 'r', encoding='utf8') as f:
                    return f.read()

            @staticmethod
            def write(file_path: str, contents: str):
                with open(file_path, 'w', encoding='utf8') as f:
                    f.write(contents)

            @staticmethod
            def exists(file_path: str):
                return os.path.exists(file_path)

        class URI:
            @staticmethod
            def decode(s: str):
                return s # Python's default string handling is often already URI-decoded for common use cases
                # For proper URI decoding, one might use urllib.parse.unquote
                # from urllib.parse import unquote
                # return unquote(s)

            @staticmethod
            def encode(s: str):
                return s # Python's default string handling is often already URI-encoded for common use cases
                # For proper URI encoding, one might use urllib.parse.quote
                # from urllib.parse import quote
                # return quote(s)

        class Time:
            @staticmethod
            def now():
                return int(time.time() * 1000) # Milliseconds since epoch

            @staticmethod
            def str():
                return datetime.datetime.now().isoformat() # ISO format string

            @staticmethod
            def hrtime():
                # Python's time.perf_counter_ns() is similar to Node.js hrtime.bigint()
                return str(time.perf_counter_ns())

    globals_env.define('Runtime', {
        "dump": {
            "keys": Runtime.dump_keys,
            "values": Runtime.dump_values
        },
        "version": runtime_version,
        "versionString": f"v{runtime_version['major']}.{runtime_version['minor']}.{runtime_version['patch']}",
        "currentDirectory": Runtime.current_directory,
        "regex": Runtime.regex,
        "args": args,
        "exit": Runtime.exit,
        "versionAtLeast": Runtime.version_at_least,
        "env": Runtime.env,
        "throw": Runtime.throw,
        "fs": Runtime.FS(),
        "URI": Runtime.URI(),
        "time": Runtime.Time()
    })

# --- Type Checking Helper ---

def check_type(expected_type: str, value: Any, token: Token):
    """Checks if a value matches the expected type."""
    if expected_type == "number":
        if not isinstance(value, (int, float)):
            raise NovaError(token, f"Type mismatch: expected number, got {type(value).__name__}")
    elif expected_type == "string":
        if not isinstance(value, str):
            raise NovaError(token, f"Type mismatch: expected string, got {type(value).__name__}")
    elif expected_type == "boolean":
        if not isinstance(value, bool):
            raise NovaError(token, f"Type mismatch: expected boolean, got {type(value).__name__}")
    elif expected_type == "void":
        if value is not None:
            raise NovaError(token, f"Type mismatch: expected void, got {type(value).__name__}")
    else:
        raise NovaError(token, f"Unknown type: {expected_type}")
    return value

# --- Interpreter Class ---

class Interpreter:
    def __init__(self, file_path: str):
        with open(file_path, "r", encoding="utf8") as f:
            self.source = f.read()
        self.file = os.path.abspath(file_path) # Store absolute path
        self.imported_files = set()

        self.keywords = [
            "var", "if", "else", "elseif", "end", "break", "continue", "func",
            "return", "import", "as", "namespace", "while", "forEach", "for",
            "do", "in", "try", "errored", "defer",
            "switch", "case", "default", "using", "def"
        ]

        self.tokens = self.tokenize(self.source, self.file)
        self.current = 0
        self.globals = Environment()
        # Functions are stored directly in the environment, not a separate dict
        init_globals(self.globals)
        self.globals.define("__SCRIPT_PATH__", os.path.dirname(self.file))
        self.current_env: Optional[Environment] = None

    # --- Tokenization ---
    def tokenize(self, source: str, file: str) -> List[Token]:
        tokens: List[Token] = []
        i = 0
        line = 1
        col = 1
        length = len(source)

        while i < length:
            char = source[i]

            # Skip whitespace
            if char == "\n":
                line += 1
                col = 1
                i += 1
                continue
            if char.isspace():
                i += 1
                col += 1
                continue

            # Comments
            if char == "/" and i + 1 < length and source[i + 1] == "/":
                while i < length and source[i] != "\n":
                    i += 1
                continue # Loop will handle newline
            if char == "/" and i + 1 < length and source[i + 1] == "*":
                i += 2
                while i < length:
                    if source[i] == "*" and i + 1 < length and source[i + 1] == "/":
                        i += 2
                        break
                    if source[i] == "\n":
                        line += 1
                        col = 1
                    else:
                        col += 1
                    i += 1
                continue

            # Logical operators (&& and ||)
            if char == "&" and i + 1 < length and source[i + 1] == "&":
                tokens.append(Token(type="operator", value="&&", line=line, column=col, file=file))
                i += 2
                col += 2
                continue
            if char == "|" and i + 1 < length and source[i + 1] == "|":
                tokens.append(Token(type="operator", value="||", line=line, column=col, file=file))
                i += 2
                col += 2
                continue

            # Numbers
            if char.isdigit():
                num_str = ""
                start_col = col
                while i < length and (source[i].isdigit() or source[i] == '.'):
                    num_str += source[i]
                    i += 1
                    col += 1
                tokens.append(Token(type="number", value=float(num_str), line=line, column=start_col, file=file))
                continue

            # Strings
            if char == '"':
                i += 1
                start_col = col
                string_val = ""
                while i < length and source[i] != '"':
                    if source[i] == "\\" and i + 1 < length:
                        i += 1
                        if source[i] == "n": string_val += "\n"
                        elif source[i] == "t": string_val += "\t"
                        elif source[i] == "r": string_val += "\r"
                        elif source[i] == "\\": string_val += "\\"
                        elif source[i] == '"': string_val += '"'
                        else: string_val += source[i]
                    else:
                        string_val += source[i]
                    i += 1
                    col += 1
                if i >= length or source[i] != '"':
                    raise NovaError(Token("error", "Unterminated string", file, line, start_col), "Unterminated string literal")
                i += 1
                col += 1
                tokens.append(Token(type="string", value=string_val, line=line, column=start_col, file=file))
                continue

            # Identifiers, keywords, booleans
            if char.isalpha() or char == '_':
                id_str = ""
                start_col = col
                while i < length and (source[i].isalnum() or source[i] == '_'):
                    id_str += source[i]
                    i += 1
                    col += 1
                if id_str == "true":
                    tokens.append(Token(type="boolean", value=True, line=line, column=start_col, file=file))
                elif id_str == "false":
                    tokens.append(Token(type="boolean", value=False, line=line, column=start_col, file=file))
                elif id_str in self.keywords:
                    tokens.append(Token(type="keyword", value=id_str, line=line, column=start_col, file=file))
                else:
                    tokens.append(Token(type="identifier", value=id_str, line=line, column=start_col, file=file))
                continue

            # Multi-character operators: ==, !=, >=, <=
            if char in "=!<>" and i + 1 < length and source[i + 1] == "=":
                op = char + "="
                tokens.append(Token(type="operator", value=op, line=line, column=col, file=file))
                i += 2
                col += 2
                continue
            
            # Single-character operators/punctuation
            if char in "#+-*%/(),{}[]:":
                tokens.append(Token(type="operator", value=char, line=line, column=col, file=file))
                i += 1
                col += 1
                continue

            # Dot operator
            if char == ".":
                tokens.append(Token(type="operator", value=".", line=line, column=col, file=file))
                i += 1
                col += 1
                continue

            raise NovaError(Token(type="error", value=char, file=file, line=line, column=col), f"Unexpected character: {char}")
        return tokens

    # --- Token Parser Helpers ---
    def get_next_token(self) -> Optional[Token]:
        if self.current < len(self.tokens):
            return self.tokens[self.current]
        return None

    def consume_token(self) -> Token:
        if not self.get_next_token():
            # Create a dummy token for EOF context if needed, or use the last valid token
            last_token = self.tokens[-1] if self.tokens else Token("EOF", "EOF", self.file, 1, 1)
            raise NovaError(last_token, "Unexpected end of input (EOF)")
        token = self.tokens[self.current]
        self.current += 1
        return token

    def expect_type(self, expected_type: str) -> Token:
        token = self.get_next_token()
        if not token or token.type != expected_type:
            raise NovaError(token, f"Expected token type {expected_type}, got {token.type if token else 'EOF'}")
        return token

    def expect_token(self, expected_value: str) -> Token:
        token = self.get_next_token()
        if not token:
            last_token = self.tokens[-1] if self.tokens else Token("EOF", "EOF", self.file, 1, 1)
            raise NovaError(last_token, f"Expected token '{expected_value}', got EOF")
        if token.value != expected_value:
            raise NovaError(token, f"Expected token '{expected_value}', got '{token.value}'")
        return token

    # --- Parsing Helpers ---
    def parse_block_until(self, terminators: List[str] = []) -> List[Statement]:
        statements: List[Statement] = []
        while self.current < len(self.tokens):
            token = self.get_next_token()
            if not token:
                last_token = self.tokens[-1] if self.tokens else Token("EOF", "EOF", self.file, 1, 1)
                raise NovaError(last_token, "Unexpected end of input")
            
            if (token.type == "keyword" and token.value in terminators) or \
               (token.type == "operator" and token.value in terminators):
                break
            statements.append(self.parse_statement())
        return statements

    def parse_block(self) -> List[Statement]:
        return self.parse_block_until([])

    def consume_expected(self, expected_type: str) -> Token:
        self.expect_type(expected_type)
        return self.consume_token()

    # --- Parsing Statements and Expressions ---
    def parse_statement(self) -> Statement:
        token = self.get_next_token()
        if not token:
            last_token = self.tokens[-1] if self.tokens else Token("EOF", "EOF", self.file, 1, 1)
            raise NovaError(last_token, "Unexpected end of input")

        # Label statement
        if token.type == "keyword" and token.value == "label":
            self.consume_token()
            name_token = self.expect_type("identifier")
            name = name_token.value
            self.consume_token()
            return LabelStmt(type="LabelStmt", value=name, file=token.file, line=token.line, column=token.column, name=name)

        # Defer statement
        if token.type == "keyword" and token.value == "defer":
            self.consume_token()
            body = self.parse_block_until(["end"])
            self.expect_token("end")
            self.consume_token()
            return DeferStmt(type="DeferStmt", value="defer", file=token.file, line=token.line, column=token.column, body=body)

        # Variable declaration
        if token.type == "keyword" and token.value == "var":
            self.consume_token()
            name_token = self.expect_type("identifier")
            name = name_token.value
            self.consume_token()

            type_annotation: Optional[str] = None
            next_t = self.get_next_token()
            if next_t and next_t.type == "identifier" and next_t.value in ["string", "number", "boolean", "void"]:
                type_annotation = next_t.value
                self.consume_token()

            modifier: Optional[str] = None
            next_t = self.get_next_token()
            if next_t and next_t.type == "operator" and next_t.value == "#":
                self.consume_token()
                mod_token = self.expect_type("identifier")
                modifier = mod_token.value
                self.consume_token()

            self.expect_token("=")
            self.consume_token()
            initializer = self.parse_expression()
            return VarDecl(type="VarDecl", value="var", file=token.file, line=token.line, column=token.column,
                           name=name, type_annotation=type_annotation, initializer=initializer, modifier=modifier)

        # Switch statement
        if token.type == "keyword" and token.value == "switch":
            self.consume_token()
            expression = self.parse_expression()
            cases: List[Case] = []
            while self.get_next_token() and self.get_next_token().type == "keyword" and self.get_next_token().value == "case":
                self.consume_token()
                case_expr = self.parse_expression()
                self.expect_token("do")
                self.consume_token()
                body = self.parse_block_until(["end"])
                self.expect_token("end")
                self.consume_token()
                cases.append(Case(case_expr=case_expr, body=body))
            
            if self.get_next_token() and self.get_next_token().type == "keyword" and self.get_next_token().value == "default":
                self.consume_token()
                self.expect_token("do")
                self.consume_token()
                body = self.parse_block_until(["end"])
                self.expect_token("end")
                self.consume_token()
                cases.append(Case(case_expr=None, body=body)) # Default case_expr is None

            self.expect_token("end")
            self.consume_token()
            return SwitchStmt(type="SwitchStmt", value="switch", file=token.file, line=token.line, column=token.column,
                              expression=expression, cases=cases)

        # Using statement
        if token.type == "keyword" and token.value == "using":
            self.consume_token()
            name_token = self.expect_type("identifier")
            name = name_token.value
            self.consume_token()
            return UsingStmt(type="UsingStmt", value="using", file=token.file, line=token.line, column=token.column, name=name)

        # Try/Catch statement
        if token.type == "keyword" and token.value == "try":
            self.consume_token()
            try_block = self.parse_block_until(["errored"])
            self.expect_token("errored")
            self.consume_token()
            error_var_token = self.expect_type("identifier")
            error_var = error_var_token.value
            self.consume_token()
            catch_block = self.parse_block_until(["end"])
            self.expect_token("end")
            self.consume_token()
            return TryStmt(type="TryStmt", value="try", file=token.file, line=token.line, column=token.column,
                           try_block=try_block, error_var=error_var, catch_block=catch_block)

        # ForEach loop
        if token.type == "keyword" and token.value == "forEach":
            self.consume_token()
            var_token = self.expect_type("identifier")
            variable = var_token.value
            self.consume_token()
            self.expect_token("in")
            self.consume_token()
            list_expr = self.parse_expression()
            self.expect_token("do")
            self.consume_token()
            body = self.parse_block_until(["end"])
            self.expect_token("end")
            self.consume_token()
            return ForEachStmt(type="ForEachStmt", value="forEach", file=token.file, line=token.line, column=token.column,
                               variable=variable, list=list_expr, body=body)

        # For loop
        if token.type == "keyword" and token.value == "for":
            self.consume_token()
            var_token = self.expect_type("identifier")
            variable = var_token.value
            self.consume_token()
            self.expect_token("=")
            self.consume_token()
            start_expr = self.parse_expression()
            self.expect_token(",")
            self.consume_token()
            end_expr = self.parse_expression()
            step_expr: Optional[Expression] = None
            if self.get_next_token() and self.get_next_token().value == ",":
                self.consume_token()
                step_expr = self.parse_expression()
            self.expect_token("do")
            self.consume_token()
            body = self.parse_block_until(["end"])
            self.expect_token("end")
            self.consume_token()
            return ForStmt(type="ForStmt", value="for", file=token.file, line=token.line, column=token.column,
                           variable=variable, start=start_expr, end=end_expr, step=step_expr, body=body)

        # While loop
        if token.type == "keyword" and token.value == "while":
            self.consume_token()
            condition = self.parse_expression()
            body = self.parse_block_until(["end"])
            self.expect_token("end")
            self.consume_token()
            return WhileStmt(type="WhileStmt", value="while", file=token.file, line=token.line, column=token.column,
                             condition=condition, body=body)

        # Break statement
        if token.type == "keyword" and token.value == "break":
            self.consume_token()
            return BreakStmt(type="BreakStmt", value="break", file=token.file, line=token.line, column=token.column)

        # Continue statement
        if token.type == "keyword" and token.value == "continue":
            self.consume_token()
            return ContinueStmt(type="ContinueStmt", value="continue", file=token.file, line=token.line, column=token.column)

        # If statement
        if token.type == "keyword" and token.value == "if":
            self.consume_token()
            condition = self.parse_expression()
            then_block = self.parse_block_until(["else", "elseif", "end"])

            else_if_blocks: List[Dict[str, Any]] = []
            else_block: Optional[List[Statement]] = None

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

            return IfStmt(type="IfStmt", value="if", file=token.file, line=token.line, column=token.column,
                          condition=condition, then_block=then_block, else_block=else_block,
                          else_if=else_if_blocks if else_if_blocks else None)

        # Namespace statement
        if token.type == "keyword" and token.value == "namespace":
            self.consume_token()
            name_token = self.expect_type("identifier")
            name = name_token.value
            self.consume_token()
            body = self.parse_block_until(["end"])
            self.expect_token("end")
            self.consume_token()
            return NamespaceStmt(type="NamespaceStmt", value="namespace", file=token.file, line=token.line, column=token.column,
                                 name=name, body=body)

        # Function declaration
        if token.type == "keyword" and token.value == "func":
            self.consume_token()
            name_token = self.expect_type("identifier")
            name = name_token.value
            self.consume_token()
            self.expect_token("(")
            self.consume_token()

            parameters: List[Parameter] = []
            if self.get_next_token() and self.get_next_token().value != ")":
                while True:
                    param_token = self.expect_type("identifier")
                    param_name = param_token.value
                    self.consume_token()

                    param_type: Optional[str] = None
                    default_expr: Optional[Expression] = None

                    # Optional type annotation
                    next_t = self.get_next_token()
                    if next_t and next_t.type == "identifier" and next_t.value in ["string", "number", "bool"]:
                        param_type = next_t.value
                        self.consume_token()

                    # Optional default value
                    if self.get_next_token() and self.get_next_token().value == "=":
                        self.consume_token()
                        default_expr = self.parse_expression()
                        if param_type:
                            raise NovaError(token, "cannot have both type and default value, as that prevents type infering")
                        # Infer type from default value if not explicitly given
                        if isinstance(default_expr, Literal):
                            if isinstance(default_expr.value, (int, float)): param_type = "number"
                            elif isinstance(default_expr.value, str): param_type = "string"
                            elif isinstance(default_expr.value, bool): param_type = "bool"
                        # else: param_type = default_expr.type # This needs careful handling of AST node types

                    parameters.append(Parameter(name=param_name, type=param_type, default=default_expr,
                                                file=param_token.file, line=param_token.line, column=param_token.column,
                                                 value=param_token.value))

                    if self.get_next_token() and self.get_next_token().value == ",":
                        self.consume_token()
                    else:
                        break
            self.expect_token(")")
            self.consume_token()
            body = self.parse_block_until(["end"])
            self.expect_token("end")
            self.consume_token()

            return FuncDecl(type="FuncDecl", value="func", file=token.file, line=token.line, column=token.column,
                            name=name, parameters=parameters, body=body)

        # Lambda declaration (def) - treated as a statement for top-level def
        if token.type == "keyword" and token.value == "def":
            self.consume_token()
            self.expect_token("(")
            self.consume_token()

            parameters: List[Parameter] = []
            if self.get_next_token() and self.get_next_token().value != ")":
                while True:
                    param_token = self.expect_type("identifier")
                    param_name = param_token.value
                    self.consume_token()

                    param_type: Optional[str] = None
                    default_expr: Optional[Expression] = None

                    next_t = self.get_next_token()
                    if next_t and next_t.type == "identifier" and next_t.value in ["string", "number", "bool"]:
                        param_type = next_t.value
                        self.consume_token()

                    if self.get_next_token() and self.get_next_token().value == "=":
                        self.consume_token()
                        default_expr = self.parse_expression()
                        if param_type:
                            raise NovaError(token, "cannot have both type and default value, as that prevents type infering")
                        if isinstance(default_expr, Literal):
                            if isinstance(default_expr.value, (int, float)): param_type = "number"
                            elif isinstance(default_expr.value, str): param_type = "string"
                            elif isinstance(default_expr.value, bool): param_type = "bool"

                    parameters.append(Parameter(name=param_name, type=param_type, default=default_expr,
                                                file=param_token.file, line=param_token.line, column=param_token.column,
                                                 value=param_token.value))

                    if self.get_next_token() and self.get_next_token().value == ",":
                        self.consume_token()
                    else:
                        break
            self.expect_token(")")
            self.consume_token()
            body = self.parse_block_until(["end"])
            self.expect_token("end")
            self.consume_token()

            return LambdaDecl(type="LambdaDecl", value="def", file=token.file, line=token.line, column=token.column,
                              parameters=parameters, body=body)

        # Return statement
        if token.type == "keyword" and token.value == "return":
            self.consume_token()
            expression: Optional[Expression] = None
            # Check if the next token is not a keyword (indicating end of statement or block)
            next_t = self.get_next_token()
            if next_t and next_t.type not in ["keyword", "operator"] and next_t.value not in [")", "]", "}"]:
                expression = self.parse_expression()
            return ReturnStmt(type="ReturnStmt", value="return", file=token.file, line=token.line, column=token.column,
                              expression=expression)

        # Import statement
        if token.type == "keyword" and token.value == "import":
            self.consume_token()
            file_token = self.expect_type("string")
            filename = file_token.value
            self.consume_token()
            alias: Optional[str] = None
            if self.get_next_token() and self.get_next_token().value == "as":
                self.consume_token()
                alias_token = self.expect_type("identifier")
                alias = alias_token.value
                self.consume_token()
            return ImportStmt(type="ImportStmt", value="import", file=token.file, line=token.line, column=token.column,
                              filename=filename, alias=alias)

        # Expression statement (fallback)
        expr = self.parse_expression()
        return ExpressionStmt(type="ExpressionStmt", value="expression", file=token.file, line=token.line, column=token.column,
                              expression=expr)

    def parse_expression(self) -> Expression:
        return self.parse_assignment()

    def parse_assignment(self) -> Expression:
        expr = self.parse_logical_or()
        next_t = self.get_next_token()
        if next_t and next_t.type == "operator" and next_t.value == "=":
            assignment_op_token = self.consume_token()
            value_expr = self.parse_assignment()
            return AssignmentExpr(type="AssignmentExpr", file=assignment_op_token.file,
                                  line=assignment_op_token.line, column=assignment_op_token.column,
                                  target=expr, value=value_expr)
        return expr

    def parse_logical_or(self) -> Expression:
        expr = self.parse_logical_and()
        while self.get_next_token() and self.get_next_token().type == "operator" and self.get_next_token().value == "||":
            operator_token = self.consume_token()
            operator = operator_token.value
            right = self.parse_logical_and()
            expr = BinaryExpr(type="BinaryExpr", value=operator, file=operator_token.file,
                              line=operator_token.line, column=operator_token.column,
                              operator=operator, left=expr, right=right)
        return expr

    def parse_logical_and(self) -> Expression:
        expr = self.parse_equality()
        while self.get_next_token() and self.get_next_token().type == "operator" and self.get_next_token().value == "&&":
            operator_token = self.consume_token()
            operator = operator_token.value
            right = self.parse_equality()
            expr = BinaryExpr(type="BinaryExpr", value=operator, file=operator_token.file,
                              line=operator_token.line, column=operator_token.column,
                              operator=operator, left=expr, right=right)
        return expr

    def parse_equality(self) -> Expression:
        expr = self.parse_comparison()
        while self.get_next_token() and self.get_next_token().type == "operator" and self.get_next_token().value in ["==", "!="]:
            operator_token = self.consume_token()
            operator = operator_token.value
            right = self.parse_comparison()
            expr = BinaryExpr(type="BinaryExpr", value=operator, file=operator_token.file,
                              line=operator_token.line, column=operator_token.column,
                              operator=operator, left=expr, right=right)
        return expr

    def parse_comparison(self) -> Expression:
        expr = self.parse_term()
        while self.get_next_token() and self.get_next_token().type == "operator" and self.get_next_token().value in ["<", "<=", ">", ">="]:
            operator_token = self.consume_token()
            operator = operator_token.value
            right = self.parse_term()
            expr = BinaryExpr(type="BinaryExpr", value=operator, file=operator_token.file,
                              line=operator_token.line, column=operator_token.column,
                              operator=operator, left=expr, right=right)
        return expr

    def parse_term(self) -> Expression:
        expr = self.parse_factor()
        while self.get_next_token() and self.get_next_token().type == "operator" and self.get_next_token().value in ["+", "-"]:
            operator_token = self.consume_token()
            operator = operator_token.value
            right = self.parse_factor()
            expr = BinaryExpr(type="BinaryExpr", value=operator, file=operator_token.file,
                              line=operator_token.line, column=operator_token.column,
                              operator=operator, left=expr, right=right)
        return expr

    def parse_factor(self) -> Expression:
        expr = self.parse_unary()
        while self.get_next_token() and self.get_next_token().type == "operator" and self.get_next_token().value in ["*", "/", "%"]:
            operator_token = self.consume_token()
            operator = operator_token.value
            right = self.parse_unary()
            expr = BinaryExpr(type="BinaryExpr", value=operator, file=operator_token.file,
                              line=operator_token.line, column=operator_token.column,
                              operator=operator, left=expr, right=right)
        return expr

    def parse_unary(self) -> Expression:
        next_t = self.get_next_token()
        if next_t and next_t.type == "operator" and next_t.value in ["-", "!"]:
            operator_token = self.consume_token()
            operator = operator_token.value
            right = self.parse_unary()
            return UnaryExpr(type="UnaryExpr", value=operator, file=operator_token.file,
                             line=operator_token.line, column=operator_token.column,
                             operator=operator, right=right)
        return self.parse_primary()

    def parse_primary(self) -> Expression:
        node: Expression
        token = self.get_next_token()
        if not token:
            last_token = self.tokens[-1] if self.tokens else Token("EOF", "EOF", self.file, 1, 1)
            raise NovaError(last_token, "Unexpected end of input")

        if token.type == "boolean" or token.type == "number" or token.type == "string":
            self.consume_token()
            node = Literal(type="Literal", value=token.value, file=token.file, line=token.line, column=token.column)
        elif token.value == "[": # Array literal
            self.consume_token()
            elements: List[Expression] = []
            if self.get_next_token() and self.get_next_token().value != "]":
                while True:
                    elements.append(self.parse_expression())
                    if self.get_next_token() and self.get_next_token().value == ",":
                        self.consume_token()
                    else:
                        break
            self.expect_token("]")
            self.consume_token()
            node = ArrayLiteral(type="ArrayLiteral", value="[]", file=token.file, line=token.line, column=token.column, elements=elements)
        elif token.value == "{": # Object literal
            self.consume_token()
            properties: List[Dict[str, Any]] = []
            if self.get_next_token() and self.get_next_token().value != "}":
                while True:
                    key_token = self.get_next_token()
                    if not key_token or (key_token.type != "identifier" and key_token.type != "string"):
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
            node = ObjectLiteral(type="ObjectLiteral", value="{}", file=token.file, line=token.line, column=token.column, properties=properties)
        elif token.type == "identifier":
            self.consume_token()
            if self.get_next_token() and self.get_next_token().value == "[": # Array access
                self.consume_token()
                index = self.parse_expression()
                self.expect_token("]")
                self.consume_token()
                node = ArrayAccess(type="ArrayAccess", value=token.value, file=token.file, line=token.line, column=token.column,
                                   name=token.value, index=index)
            elif self.get_next_token() and self.get_next_token().value == "(": # Function call
                self.consume_token()
                args: List[Expression] = []
                if self.get_next_token() and self.get_next_token().value != ")":
                    while True:
                        args.append(self.parse_expression())
                        if self.get_next_token() and self.get_next_token().value == ",":
                            self.consume_token()
                        else:
                            break
                self.expect_token(")")
                self.consume_token()
                node = FuncCall(type="FuncCall", value=token.value, file=token.file, line=token.line, column=token.column,
                                name=token.value, arguments=args)
            else:
                node = Identifier(type="Identifier", value=token.value, file=token.file, line=token.line, column=token.column,
                                  name=token.value)
        elif token.value == "def": # Lambda expression (def (...))
            self.consume_token() # consume 'def'
            self.expect_token("(")
            self.consume_token()

            parameters: List[Parameter] = []
            if self.get_next_token() and self.get_next_token().value != ")":
                while True:
                    param_token = self.expect_type("identifier")
                    param_name = param_token.value
                    self.consume_token()

                    param_type: Optional[str] = None
                    default_expr: Optional[Expression] = None

                    next_t = self.get_next_token()
                    if next_t and next_t.type == "identifier" and next_t.value in ["string", "number", "bool"]:
                        param_type = next_t.value
                        self.consume_token()

                    if self.get_next_token() and self.get_next_token().value == "=":
                        self.consume_token()
                        default_expr = self.parse_expression()
                        if param_type:
                            raise NovaError(token, "cannot have both type and default value, as that prevents type infering")
                        if isinstance(default_expr, Literal):
                            if isinstance(default_expr.value, (int, float)): param_type = "number"
                            elif isinstance(default_expr.value, str): param_type = "string"
                            elif isinstance(default_expr.value, bool): param_type = "bool"

                    parameters.append(Parameter(name=param_name, type=param_type, default=default_expr,
                                                file=param_token.file, line=param_token.line, column=param_token.column,
                                                 value=param_token.value))

                    if self.get_next_token() and self.get_next_token().value == ",":
                        self.consume_token()
                    else:
                        break
            self.expect_token(")")
            self.consume_token()
            body = self.parse_block_until(["end"])
            self.expect_token("end")
            self.consume_token()

            node = LambdaDecl(type="LambdaDecl", value="def", file=token.file, line=token.line, column=token.column,
                              parameters=parameters, body=body)
        elif token.value == "(": # Grouping expression
            self.consume_token()
            node = self.parse_expression()
            self.expect_token(")")
            self.consume_token()
        else:
            raise NovaError(token, f"Unexpected token: {token.value}")

        # Property access or method call chaining
        while self.get_next_token() and self.get_next_token().value == ".":
            dot_token = self.consume_token()
            prop_token = self.expect_type("identifier")
            prop_name = prop_token.value
            self.consume_token()

            if self.get_next_token() and self.get_next_token().value == "(": # Method call
                self.consume_token()
                args: List[Expression] = []
                while self.get_next_token() and self.get_next_token().value != ")":
                    args.append(self.parse_expression())
                    if self.get_next_token() and self.get_next_token().value == ",":
                        self.consume_token()
                    else:
                        break
                self.expect_token(")")
                self.consume_token()
                node = MethodCall(type="MethodCall", value=prop_name, file=prop_token.file, line=prop_token.line, column=prop_token.column,
                                  object=node, method=prop_name, arguments=args)
            else: # Property access
                node = PropertyAccess(type="PropertyAccess", value=prop_name, file=prop_token.file, line=prop_token.line, column=prop_token.column,
                                      object=node, property=prop_name)
        return node

    # --- Evaluation / Execution ---
    def interpret(self):
        statements = self.parse_block()
        try:
            self.execute_block(statements, self.globals)
        except Exception as err:
            # Ensure deferred statements still execute on error
            self.globals.execute_deferred(self)
            raise err

    def execute_block(self, statements: List[Statement], env: Environment):
        previous_env = self.current_env
        self.current_env = env

        try:
            for stmt in statements:
                self.execute_stmt(stmt, env)
        finally:
            # Execute deferred statements in reverse order
            env.execute_deferred(self)
            self.current_env = previous_env

    def get_current_context(self) -> Optional[Environment]:
        return self.current_env

    def execute_stmt(self, stmt: Statement, env: Environment):
        if isinstance(stmt, VarDecl):
            value = self.evaluate_expr(stmt.initializer, env)
            if stmt.type_annotation:
                check_type(stmt.type_annotation, value, stmt)
            env.define(stmt.name, value)
        elif isinstance(stmt, DeferStmt):
            # Add statements to be executed when the current block exits
            # The original JS code reverses the list and adds, which means they are popped in reverse order of definition.
            # Python's list.extend(reversed(list)) or simple append and then pop works.
            # The current implementation of add_deferred and execute_deferred handles this correctly.
            for s in stmt.body:
                env.add_deferred(s)
        elif isinstance(stmt, ExpressionStmt):
            self.evaluate_expr(stmt.expression, env)
        elif isinstance(stmt, BreakStmt):
            raise BreakException()
        elif isinstance(stmt, ContinueStmt):
            raise ContinueException()
        elif isinstance(stmt, TryStmt):
            try:
                self.execute_block(stmt.try_block, env)
            except Exception as e:
                catch_env = Environment(env)
                # If e is an Error or NovaError, pass it. Otherwise, wrap it.
                error_to_define = e if isinstance(e, (Exception, NovaError)) else NovaError(stmt, str(e))
                catch_env.define(stmt.error_var, error_to_define)
                self.execute_block(stmt.catch_block, catch_env)
        elif isinstance(stmt, IfStmt):
            condition = self.evaluate_expr(stmt.condition, env)
            if condition:
                self.execute_block(stmt.then_block, Environment(env))
            elif stmt.else_if:
                matched = False
                # Create a new environment for the elseif chain
                # The original JS creates new Environment(env) for each block, which is correct for scoping.
                # However, the elseif chain itself should probably share an environment if variables
                # declared in one elseif block should be visible in subsequent ones (which is not typical).
                # Sticking to the original JS behavior: new env for each block.
                for elseif_block_data in stmt.else_if:
                    elseif_condition = self.evaluate_expr(elseif_block_data["condition"], env)
                    if elseif_condition:
                        self.execute_block(elseif_block_data["body"], Environment(env))
                        matched = True
                        break
                if not matched and stmt.else_block:
                    self.execute_block(stmt.else_block, Environment(env))
            elif stmt.else_block:
                self.execute_block(stmt.else_block, Environment(env))
        elif isinstance(stmt, WhileStmt):
            while self.evaluate_expr(stmt.condition, env):
                try:
                    self.execute_block(stmt.body, Environment(env))
                except BreakException:
                    break
                except ContinueException:
                    continue
                except Exception as e:
                    raise e
        elif isinstance(stmt, ForEachStmt):
            lst = self.evaluate_expr(stmt.list, env)
            if not isinstance(lst, list):
                raise NovaError(stmt, f"Cannot iterate over non-list type for forEach loop. Got: {type(lst).__name__}")
            for item in lst:
                loop_env = Environment(env)
                loop_env.define(stmt.variable, item)
                try:
                    self.execute_block(stmt.body, loop_env)
                except BreakException:
                    break
                except ContinueException:
                    continue
                except Exception as e:
                    raise e
        elif isinstance(stmt, ForStmt):
            start = self.evaluate_expr(stmt.start, env)
            end = self.evaluate_expr(stmt.end, env)
            step = self.evaluate_expr(stmt.step, env) if stmt.step else 1

            if not all(isinstance(val, (int, float)) for val in [start, end, step]):
                raise NovaError(stmt, f"For loop bounds and step must be numbers. Got start: {type(start).__name__}, end: {type(end).__name__}, step: {type(step).__name__}")

            # Handle step direction
            if step == 0:
                raise NovaError(stmt, "For loop step cannot be zero.")
            
            if step > 0:
                range_iter = range(int(start), int(end) + 1, int(step))
            else: # step < 0
                range_iter = range(int(start), int(end) - 1, int(step))

            for i in range_iter:
                loop_env = Environment(env)
                loop_env.define(stmt.variable, i)
                try:
                    self.execute_block(stmt.body, loop_env)
                except BreakException:
                    break
                except ContinueException:
                    continue
                except Exception as e:
                    raise e
        elif isinstance(stmt, ImportStmt):
            file_path = stmt.filename
            if file_path.startswith("os:"):
                js_module_name = file_path[3:]
                if not env.has("os-import-handler"):
                    raise NovaError(stmt, "os-import-handler is not defined, your runtime should define it, interpreter.globals.define('os-import-handler', handler)")
                
                handler = env.get("os-import-handler", stmt)
                # In Python, this would typically mean importing a native Python module
                # or dynamically loading something. For now, we'll just call the handler.
                try:
                    result = handler(js_module_name)
                except Exception as e:
                    raise NovaError(stmt, f"Error importing JS module '{js_module_name}': {e}")
                
                name = stmt.alias or js_module_name
                env.define(name, result)
                
            else: # NovaScript file import
                if not file_path.endswith(".nova"):
                    file_path += ".nova"
                
                file_dir = os.path.dirname(self.file)
                full_path = os.path.abspath(os.path.join(file_dir, file_path))

                if full_path in self.imported_files:
                    return # Already imported

                self.imported_files.add(full_path)

                if not os.path.exists(full_path):
                    raise NovaError(stmt, f"Import error: File not found at '{full_path}'")

                imported_interpreter = Interpreter(full_path)
                # Imported environment inherits from the current global environment
                imported_env = Environment(self.globals)
                imported_interpreter.globals = imported_env # Overwrite globals for the imported interpreter to use the new env
                imported_interpreter.imported_files = self.imported_files # Share imported_files set

                imported_interpreter.interpret()

                # Collect defined values in the imported environment to form a namespace
                namespace: Dict[str, Any] = {}
                for key, value in imported_env.values.items():
                    namespace[key] = value
                
                # If the imported environment has a parent (e.g., globals), also include its values
                # This part of the original JS code is a bit ambiguous.
                # The TS code iterates importedEnv.values. If importedEnv.parent is self.globals,
                # then globals are not copied into the namespace. Only top-level definitions in the imported file.
                # This seems correct for a module system.

                module_name = os.path.splitext(os.path.basename(file_path))[0]
                name = stmt.alias or module_name
                env.define(name, namespace)

        elif isinstance(stmt, NamespaceStmt):
            n_env = Environment(env)
            self.execute_block(stmt.body, n_env)
            env.define(stmt.name, n_env) # Define the nested environment itself as the namespace
        elif isinstance(stmt, SwitchStmt):
            value = self.evaluate_expr(stmt.expression, env)
            matched = False
            default_case_body: Optional[List[Statement]] = None

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
        elif isinstance(stmt, ReturnStmt):
            value = self.evaluate_expr(stmt.expression, env) if stmt.expression else None
            raise ReturnException(value)
        elif isinstance(stmt, FuncDecl):
            # Create a Python function that wraps the NovaScript function logic
            def novafunc(*args):
                func_env = Environment(env) # Closure over the environment where func was defined

                if len(args) > len(stmt.parameters):
                    raise NovaError(stmt, f"Too many arguments passed to function '{stmt.name}'. Expected {len(stmt.parameters)}, got {len(args)}.")

                for i, param in enumerate(stmt.parameters):
                    arg_val = args[i] if i < len(args) else None

                    # Apply default if missing or None (Python None is like JS undefined/null)
                    if arg_val is None and param.default is not None:
                        arg_val = self.evaluate_expr(param.default, env) # Evaluate default in the parent scope
                    elif arg_val is None and param.default is None:
                        # If an argument is missing and no default is provided, and it's not optional
                        # This check is more robust if we track optionality, but based on TS, if default is None, it's mandatory.
                        if i >= len(args): # Only complain if argument was actually missing
                            raise NovaError(param, f"Missing argument for parameter '{param.name}' in function '{stmt.name}'.")

                    # Soft type check
                    if param.type:
                        actual_type_name = type(arg_val).__name__
                        if param.type == "number" and not isinstance(arg_val, (int, float)):
                            raise NovaError(param, f"Type mismatch in function '{stmt.name}': parameter '{param.name}' expected number, got {actual_type_name}")
                        elif param.type == "string" and not isinstance(arg_val, str):
                            raise NovaError(param, f"Type mismatch in function '{stmt.name}': parameter '{param.name}' expected string, got {actual_type_name}")
                        elif param.type == "bool" and not isinstance(arg_val, bool):
                            raise NovaError(param, f"Type mismatch in function '{stmt.name}': parameter '{param.name}' expected bool, got {actual_type_name}")
                    
                    func_env.define(param.name, arg_val)

                try:
                    self.execute_block(stmt.body, func_env)
                except ReturnException as e:
                    return e.value
                except Exception as e:
                    raise e # Re-raise other exceptions
                return None # Functions without explicit return return None (Python equivalent of undefined)
            
            env.define(stmt.name, novafunc)
        elif isinstance(stmt, LambdaDecl):
            # This case should ideally not be hit if LambdaDecl is only for expressions.
            # However, the TS code defines it as a Statement type as well.
            # If it's a top-level 'def', it essentially defines an anonymous function.
            # For now, we'll treat it as defining an anonymous function that isn't assigned.
            # This might be an unreachable path if 'def' is always part of an assignment.
            # If it is reached, it means a lambda is declared but not used, so it's a no-op.
            pass
        elif isinstance(stmt, UsingStmt):
            namespace = env.get(stmt.name, stmt)
            if isinstance(namespace, Environment): # If it's a NovaScript namespace (Environment object)
                for key, value in namespace.values.items():
                    env.define(key, value)
            elif isinstance(namespace, dict): # If it's a Python dictionary (e.g., from an imported module)
                for key, value in namespace.items():
                    env.define(key, value)
            else:
                raise NovaError(stmt, f"Cannot 'use' non-namespace value '{stmt.name}'.")
        else:
            raise NovaError(stmt, f"Unknown statement type: {stmt.type}")

    def expand_array_target(self, expr: ArrayAccess, env: Environment) -> Dict[str, Any]:
        array_name = expr.name
        index_expr = expr.index

        arr = env.get(array_name, expr)
        index = self.evaluate_expr(index_expr, env)

        if not isinstance(arr, (list, dict)): # Python lists are arrays, dicts can be indexed like objects
            raise NovaError(expr, f"Cannot index into non-array/object: {type(arr).__name__}")
        
        return {"arr": arr, "index": index}

    def expand_prop_target(self, expr: PropertyAccess, env: Environment) -> Dict[str, Any]:
        current: Expression = expr
        chain: List[str] = []

        while isinstance(current, PropertyAccess):
            chain.insert(0, current.property) # Add to the beginning to keep order
            current = current.object
        
        if not isinstance(current, Identifier):
            raise NovaError(expr, "Invalid base for property access: " + current.type)
        
        obj = env.get(current.name, current)

        for i in range(len(chain) - 1):
            key = chain[i]
            if obj is None or not isinstance(obj, (dict, object, Environment)): # Check if obj is indexable
                raise NovaError(expr, f"Cannot access property '{key}' on non-object (type: {type(obj).__name__})")
            
            if isinstance(obj, Environment):
                obj = obj.get(key, expr) # Get from NovaScript Environment
            elif isinstance(obj, dict):
                obj = obj.get(key) # Get from Python dictionary
            else: # Regular Python object
                obj = getattr(obj, key, None) # Get attribute, return None if not found

        return {"obj": obj, "key": chain[-1]}

    def evaluate_expr(self, expr: Expression, env: Environment) -> Any:
        if isinstance(expr, Literal):
            return expr.value
        elif isinstance(expr, Identifier):
            return env.get(expr.name, expr)
        elif isinstance(expr, AssignmentExpr):
            value = self.evaluate_expr(expr.value, env)
            if isinstance(expr.target, ArrayAccess):
                target_data = self.expand_array_target(expr.target, env)
                arr = target_data["arr"]
                index = target_data["index"]
                if arr is None:
                    raise NovaError(expr.target, "Cannot assign to index of null or undefined value.")
                if not isinstance(index, (int, str)):
                    raise NovaError(expr.target, f"Array index must be a number or string. Got: {type(index).__name__}")
                arr[index] = value
            elif isinstance(expr.target, PropertyAccess):
                target_data = self.expand_prop_target(expr.target, env)
                obj = target_data["obj"]
                key = target_data["key"]
                if isinstance(obj, Environment):
                    raise NovaError(expr.target, "Cannot assign to environment properties directly.")
                if obj is None:
                    raise NovaError(expr.target, f"Cannot assign property '{key}' of null or undefined value.")
                
                # For Python objects, use setattr; for dicts, direct assignment
                if isinstance(obj, dict):
                    obj[key] = value
                else: # Assume it's a regular Python object
                    setattr(obj, key, value)
            elif isinstance(expr.target, Identifier):
                env.assign(expr.target.name, value, expr.target)
            else:
                raise NovaError(expr.target, f"Unsupported assignment target: {expr.target.type}({expr.target})")
            return value
        elif isinstance(expr, BinaryExpr):
            left = self.evaluate_expr(expr.left, env)
            right = self.evaluate_expr(expr.right, env)
            
            # Type checks for arithmetic operations
            if expr.operator in ["+", "-", "*", "/", "%"]:
                if not isinstance(left, (int, float)) or not isinstance(right, (int, float)):
                    raise NovaError(expr, f"Arithmetic operation '{expr.operator}' requires numbers. Got {type(left).__name__} and {type(right).__name__}.")

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
        elif isinstance(expr, UnaryExpr):
            right = self.evaluate_expr(expr.right, env)
            if expr.operator == "-":
                if not isinstance(right, (int, float)):
                    raise NovaError(expr, f"Unary '-' operator can only be applied to numbers. Got: {type(right).__name__}")
                return -right
            elif expr.operator == "!":
                return not right
            else:
                raise NovaError(expr, f"Unknown unary operator: {expr.operator}")
        elif isinstance(expr, FuncCall):
            func = env.get(expr.name, expr)
            if not callable(func):
                raise NovaError(expr, f"{expr.name} is not a function")
            args = [self.evaluate_expr(arg, env) for arg in expr.arguments]
            return func(*args)
        elif isinstance(expr, MethodCall):
            obj = self.evaluate_expr(expr.object, env)

            if obj is None:
                raise NovaError(expr, f"Cannot call method '{expr.method}' on null or undefined.")
            
            fn = None
            if isinstance(obj, Environment): # NovaScript namespace object
                fn = obj.get(expr.method, expr)
            elif isinstance(obj, dict): # Python dictionary
                fn = obj.get(expr.method)
            else: # Regular Python object
                fn = getattr(obj, expr.method, None) # Get attribute, return None if not found

            if not callable(fn):
                raise NovaError(expr, f"{expr.method} is not a function or method on this object (type: {type(obj).__name__})")
            
            arg_vals = [self.evaluate_expr(arg, env) for arg in expr.arguments]
            
            # If it's a method on a Python object, call it with obj as self
            if isinstance(obj, (dict, object)) and not isinstance(obj, Environment):
                return fn(obj, *arg_vals) if hasattr(fn, '__self__') and fn.__self__ is None else fn(*arg_vals)
            else: # For NovaScript functions or other callables
                return fn(*arg_vals)
        elif isinstance(expr, ArrayAccess):
            arr = env.get(expr.name, expr)
            index = self.evaluate_expr(expr.index, env)
            if not isinstance(arr, (list, dict)):
                raise NovaError(expr, f"Cannot access index of non-array/object: {type(arr).__name__}")
            if not isinstance(index, (int, str)):
                raise NovaError(expr, f"Array/object index must be a number or string. Got: {type(index).__name__}")
            return arr[index]
        elif isinstance(expr, PropertyAccess):
            obj = self.evaluate_expr(expr.object, env)
            if obj is None:
                raise NovaError(expr, f"Cannot access property '{expr.property}' of null or undefined.")
            
            if isinstance(obj, Environment): # NovaScript namespace object
                return obj.get(expr.property, expr)
            elif isinstance(obj, dict): # Python dictionary
                return obj.get(expr.property)
            else: # Regular Python object
                return getattr(obj, expr.property, None) # Get attribute, return None if not found
        elif isinstance(expr, ArrayLiteral):
            return [self.evaluate_expr(element, env) for element in expr.elements]
        elif isinstance(expr, ObjectLiteral):
            obj: Dict[str, Any] = {}
            for prop in expr.properties:
                obj[prop["key"]] = self.evaluate_expr(prop["value"], env)
            return obj
        elif isinstance(expr, LambdaDecl):
            # Create a Python function that wraps the NovaScript lambda logic
            def novalambda(*args):
                func_env = Environment(env) # Closure over the environment where lambda was defined

                if len(args) > len(expr.parameters):
                    raise NovaError(expr, f"Too many arguments passed to lambda. Expected {len(expr.parameters)}, got {len(args)}.")

                for i, param in enumerate(expr.parameters):
                    arg_val = args[i] if i < len(args) else None

                    if arg_val is None and param.default is not None:
                        arg_val = self.evaluate_expr(param.default, env)
                    elif arg_val is None and param.default is None:
                        if i >= len(args):
                            raise NovaError(param, f"Missing argument for parameter '{param.name}' in lambda.")

                    if param.type:
                        actual_type_name = type(arg_val).__name__
                        if param.type == "number" and not isinstance(arg_val, (int, float)):
                            raise NovaError(param, f"Type mismatch in lambda: parameter '{param.name}' expected number, got {actual_type_name}")
                        elif param.type == "string" and not isinstance(arg_val, str):
                            raise NovaError(param, f"Type mismatch in lambda: parameter '{param.name}' expected string, got {actual_type_name}")
                        elif param.type == "bool" and not isinstance(arg_val, bool):
                            raise NovaError(param, f"Type mismatch in lambda: parameter '{param.name}' expected bool, got {actual_type_name}")
                    
                    func_env.define(param.name, arg_val)

                try:
                    self.execute_block(expr.body, func_env)
                except ReturnException as e:
                    return e.value
                except Exception as e:
                    raise e
                return None
            return novalambda
        else:
            raise NovaError(expr, f"Unknown expression type: {expr.type}")

# Example Usage (assuming a test.nova file exists for import)
# if __name__ == "__main__":
#     # Create a dummy NovaScript file for testing
#     with open("test.nova", "w") as f:
#         f.write("""
#         var x = 10
#         func add(a, b)
#             return a + b
#         end
#         """)

#     # Create a main script
#     with open("main.nova", "w") as f:
#         f.write("""
#         import "test" as mylib
#         print("Hello from NovaScript!")
#         var result = mylib.add(5, 7)
#         print("Result of add:", result)
#         var my_list = [1, 2, 3]
#         forEach item in my_list do
#             print("Item:", item)
#         end
#         var my_dict = {"name": "Nova", "version": 0.7}
#         print("Name:", my_dict.name)
#         print("Version:", my_dict.version)
#         if result > 10 do
#             print("Result is greater than 10")
#         end else do
#             print("Result is not greater than 10")
#         end
#         var i = 0
#         while i < 3 do
#             print("While loop:", i)
#             i = i + 1
#         end
#         func testDefer()
#             defer
#                 print("Deferred 1")
#             end
#             defer
#                 print("Deferred 2")
#             end
#             print("Inside testDefer")
#         end
#         testDefer()
#         print("After testDefer")
#         var lambda_func = def (a, b)
#             return a * b
#         end
#         print("Lambda result:", lambda_func(3, 4))
#         """)

#     interpreter = Interpreter("main.nova")
#     try:
#         interpreter.interpret()
#     except NovaError as e:
#         print(f"NovaScript Error: {e}")
#     except Exception as e:
#         print(f"Python Error: {e}")

#     # Clean up dummy files
#     os.remove("test.nova")
#     os.remove("main.nova")