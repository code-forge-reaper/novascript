import os
import sys
import json
import re
import time
import urllib.parse
import inspect # For util.format equivalent if needed, though f-strings are often better

# --- Type Definitions (Python Classes instead of TypeScript Interfaces) ---

class Token:
    """Represents a lexical token in the NovaScript language."""
    def __init__(self, type: str, value: any, file: str, line: int, column: int):
        self.type = type
        self.value = value
        self.file = file
        self.line = line
        self.column = column

    def __repr__(self):
        return f"Token(type='{self.type}', value='{self.value}', file='{self.file}', line={self.line}, column={self.column})"

class RuntimeVersion:
    """Represents the runtime version."""
    def __init__(self, major: int, minor: int, patch: int):
        self.major = major
        self.minor = minor
        self.patch = patch

# Statement and Expression now extend Token to carry location information
class Statement(Token):
    """Base class for all statements."""
    def __init__(self, type: str, file: str, line: int, column: int, **kwargs):
        super().__init__(type, None, file, line, column) # value is not typically used for base Statement
        for k, v in kwargs.items():
            setattr(self, k, v)

class Expression(Token):
    """Base class for all expressions."""
    def __init__(self, type: str, file: str, line: int, column: int, **kwargs):
        super().__init__(type, None, file, line, column) # value is not typically used for base Expression
        for k, v in kwargs.items():
            setattr(self, k, v)

class Parameter(Token):
    """Represents a function parameter."""
    def __init__(self, name: str, type: str | None, default: Expression | None, file: str, line: int, column: int, token_type: str, token_value: any):
        super().__init__(token_type, token_value, file, line, column)
        self.name = name
        self.type = type
        self.default = default

class Case:
    """Represents a case in a switch statement."""
    def __init__(self, case_expr: any, body: list[Statement]):
        self.case_expr = case_expr
        self.body = body

class FunctionCall(Expression):
    """Represents a function call expression."""
    def __init__(self, name: str, arguments: list[Expression], file: str, line: int, column: int):
        super().__init__("FuncCall", file, line, column)
        self.name = name
        self.arguments = arguments

class MethodCall(Expression):
    """Represents a method call expression."""
    def __init__(self, object: Expression, method: str, arguments: list[Expression], file: str, line: int, column: int):
        super().__init__("MethodCall", file, line, column)
        self.object = object
        self.method = method
        self.arguments = arguments

class PropertyAccess(Expression):
    """Represents a property access expression."""
    def __init__(self, object: Expression, property: str, file: str, line: int, column: int):
        super().__init__("PropertyAccess", file, line, column)
        self.object = object
        self.property = property

class ArrayAccess(Expression):
    """Represents an array access expression."""
    def __init__(self, name: str, index: Expression, file: str, line: int, column: int):
        super().__init__("ArrayAccess", file, line, column)
        self.name = name
        self.index = index

class Literal(Expression):
    """Represents a literal value expression."""
    def __init__(self, value: any, file: str, line: int, column: int):
        super().__init__("Literal", file, line, column)
        self.value = value

class DeferStmt(Statement):
    """Represents a defer statement."""
    def __init__(self, body: list[Statement], file: str, line: int, column: int):
        super().__init__("DeferStmt", file, line, column)
        self.body = body

# --- Custom Exceptions ---

class ReturnException(Exception):
    """A special exception used to implement returning values from functions."""
    def __init__(self, value: any):
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
    def __init__(self, token: Token | None, user_message: str | None = None):
        # Provide fallback values if token is None or lacks properties
        file = token.file if token else "unknown_file"
        line = token.line if token else 0
        column = token.column if token else 0
        token_value = token.value if token is not None and hasattr(token, 'value') else 'N/A'

        message = f"{file}:{line}:{column} {user_message or 'unknown error at token: ' + str(token_value)}"
        super().__init__(message)
        self.line = line
        self.column = column
        self.file = file

# --- Environment for Variable Scoping ---

class Environment:
    """A simple environment for variable scoping."""
    def __init__(self, parent: 'Environment | None' = None):
        self.values: dict[str, any] = {}
        self.parent = parent
        self.deferred: list[Statement] = []

    def add_deferred(self, stmt: Statement):
        """Adds a statement to the deferred list."""
        self.deferred.append(stmt)

    def execute_deferred(self, interpreter: 'Interpreter'):
        """Executes deferred statements in reverse order."""
        # Execute deferred statements in reverse order (LIFO)
        while self.deferred:
            stmt = self.deferred.pop()
            interpreter.execute_stmt(stmt, self)

    def define(self, name: str, value: any):
        """Defines a new variable in the current scope."""
        self.values[name] = value

    def has(self, name: str) -> bool:
        """Checks if a variable exists in the current or parent scopes."""
        if name in self.values:
            return True
        elif self.parent:
            return self.parent.has(name)
        else:
            return False

    def assign(self, name: str, value: any, tok: Token):
        """Assigns a value to an existing variable in the current or parent scopes."""
        if name in self.values:
            self.values[name] = value
        elif self.parent:
            self.parent.assign(name, value, tok)
        else:
            raise NovaError(tok, f"Undefined variable {name}")

    def get(self, name: str, tok: Token | None = None) -> any:
        """Retrieves the value of a variable from the current or parent scopes."""
        if name in self.values:
            return self.values[name]
        elif self.parent:
            return self.parent.get(name, tok)
        else:
            if tok:
                raise NovaError(tok, f"Undefined variable {name}")
            raise NovaError(None, f"Undefined variable {name}") # Fallback for no token context

# --- Global Initialization ---

def init_globals(globals_env: Environment):
    """Initializes the global environment with built-in functions and variables."""
    globals_env.define('print', print)
    runtime_version = RuntimeVersion(major=0, minor=7, patch=0)

    # Create a dummy token for internal/bootstrap errors in init_globals
    internal_token = Token(type="internal", value="init", file="internal_init.ts", line=1, column=1)

    globals_env.define("isArray", lambda x: isinstance(x, list))
    globals_env.define("new", lambda class_obj, *args: class_obj(*args))

    # Logger implementation
    class Logger:
        def info(self, *args):
            print(f"[info {globals_env.get('Runtime', internal_token).get('versionString', internal_token)}| at: {time.localtime().tm_hour}:{time.localtime().tm_min}:{time.localtime().tm_sec}]:", *args)
        def warn(self, *args):
            print(f"[warn {globals_env.get('Runtime', internal_token).get('versionString', internal_token)}| at: {time.localtime().tm_hour}:{time.localtime().tm_min}:{time.localtime().tm_sec}]:", *args, file=sys.stderr)
        def error(self, *args):
            print(f"[error {globals_env.get('Runtime', internal_token).get('versionString', internal_token)}| at: {time.localtime().tm_hour}:{time.localtime().tm_min}:{time.localtime().tm_sec}]:", *args, file=sys.stderr)
    globals_env.define("Logger", Logger())

    # 'is' utility functions
    class IsUtility:
        def string(self, s: any) -> bool: return isinstance(s, str)
        def number(self, n: any) -> bool: return isinstance(n, (int, float))
        def boolean(self, b: any) -> bool: return isinstance(b, bool)
    globals_env.define("is", IsUtility())

    args = sys.argv[2:] # Skip script name and NovaScript file name
    globals_env.define("json", json)
    
    # parse utility functions
    class ParseUtility:
        def int(self, s: str) -> int: return int(s)
        def float(self, s: str) -> float: return float(s)
        def str(self, s: any) -> str: return str(s)
        def bool(self, s: any) -> bool: return bool(s)
    globals_env.define("parse", ParseUtility())

    globals_env.define("math", __import__('math')) # Import Python's math module
    globals_env.define("null", None)
    globals_env.define("undefined", None) # Python's None serves both null and undefined
    globals_env.define("NaN", float('nan'))

    # Runtime object
    class Runtime:
        def __init__(self):
            self.version = runtime_version
            self.versionString = f"v{runtime_version.major}.{runtime_version.minor}.{runtime_version.patch}"
            self.args = args
            self.dump = self.DumpUtility()
            self.fs = self.FileSystemUtility()
            self.URI = self.URIUtility()
            self.time = self.TimeUtility()
        class DumpUtility:
            def keys(self, obj: dict) -> list: return list(obj.keys())
            def values(self, obj: dict) -> list: return list(obj.values())

        def currentDirectory(self) -> str: return os.getcwd()
        def regex(self, str_patt: str, options: str = ""): return re.compile(str_patt, self._parse_regex_options(options))

        def _parse_regex_options(self, options: str) -> int:
            flags = 0
            if 'i' in options: flags |= re.IGNORECASE
            if 'm' in options: flags |= re.MULTILINE
            if 's' in options: flags |= re.DOTALL
            if 'x' in options: flags |= re.VERBOSE
            return flags

        def exit(self, code: int = 0): sys.exit(code)

        def versionAtLeast(self, maj: int, min: int, pat: int) -> bool:
            if self.version.major > maj: return True
            if self.version.major < maj: return False
            if self.version.minor > min: return True
            if self.version.minor < min: return False
            return self.version.patch >= pat

        def env(self, key: str | None = None) -> any:
            if key: return os.environ.get(key)
            return dict(os.environ) # Return a copy of the environment variables

        def throw(self, reason: any, *rest: any):
            if rest:
                # Python's f-strings or str.format can handle this
                reason = reason.format(*rest) if isinstance(reason, str) else str(reason)
            # For Runtime.throw, we don't have a direct token from the script.
            # We can create a dummy one or use a generic "runtime" token.
            runtime_throw_token = Token(type="runtime", value="throw", file="runtime_internal.py", line=0, column=0)
            raise NovaError(runtime_throw_token, reason)

        class FileSystemUtility:
            def read(self, path: str) -> str:
                with open(path, 'r', encoding='utf8') as f:
                    return f.read()
            def write(self, path: str, contents: str):
                with open(path, 'w', encoding='utf8') as f:
                    f.write(contents)
            def exists(self, path: str) -> bool: return os.path.exists(path)

        class URIUtility:
            def decode(self, s: str) -> str: return urllib.parse.unquote(s)
            def encode(self, s: str) -> str: return urllib.parse.quote(s)


        class TimeUtility:
            def now(self) -> int: return int(time.time() * 1000) # Milliseconds since epoch
            def str(self): return time.ctime() # Human-readable time string
            def hrtime(self) -> str: return str(time.perf_counter_ns()) # High-resolution time in nanoseconds

    globals_env.define('Runtime', Runtime())

# --- Helper function for type checking ---

def check_type(expected: str, value: any, token: Token):
    """Checks if a value matches the expected type."""
    if expected == "number":
        if not isinstance(value, (int, float)):
            raise NovaError(token, f"Type mismatch: expected number, got {type(value).__name__}")
    elif expected == "string":
        if not isinstance(value, str):
            raise NovaError(token, f"Type mismatch: expected string, got {type(value).__name__}")
    elif expected == "boolean":
        if not isinstance(value, bool):
            raise NovaError(token, f"Type mismatch: expected boolean, got {type(value).__name__}")
    elif expected == "void":
        if value is not None: # Python's None is equivalent to undefined/void
            raise NovaError(token, f"Type mismatch: expected void, got {type(value).__name__}")
    else:
        raise NovaError(token, f"Unknown type: {expected}")
    return value

# --- Interpreter Class ---

class Interpreter:
    """Main interpreter class for NovaScript."""
    keywords = [
        "var", "if", "else", "elseif", "end", "break", "continue", "func",
        "return", "import", "as", "namespace", "while", "forEach", "for",
        "do", "in", "try", "errored", "defer",
        "switch", "case", "default", "using", "def"
    ]

    def __init__(self, file_path: str):
        with open(file_path, "r", encoding="utf8") as f:
            self.source = f.read()
        self.file = file_path
        self.imported_files = set()

        self.tokens = self.tokenize(self.source, self.file)
        self.current = 0
        self.globals = Environment()
        self.functions = {} # Not directly used in the provided code, but kept for consistency
        init_globals(self.globals)
        self.globals.define("__SCRIPT_PATH__", os.path.dirname(self.file))
        self.current_env: Environment | None = None

    # ----------------------
    # Tokenization
    # ----------------------
    def tokenize(self, source: str, file: str) -> list[Token]:
        """Converts source code into a list of tokens."""
        tokens: list[Token] = []
        i = 0
        line = 1
        col = 1
        length = len(source)

        while i < length:
            char = source[i]

            # Skip whitespace.
            if char == "\n":
                line += 1
                col = 1
                i += 1
                continue
            if char.isspace(): # Handles other whitespace like spaces, tabs
                i += 1
                col += 1
                continue

            # --- Comment support ---
            # Single-line comment //
            if char == "/" and i + 1 < length and source[i + 1] == "/":
                while i < length and source[i] != "\n":
                    i += 1
                # The loop ends either at newline or EOF, the outer loop will handle newline increment
                continue
            # Multi-line comment /* */
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

            # --- Logical operators (&& and ||) ---
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

            # Numbers (supporting decimals)
            if char.isdigit():
                num_str = ""
                start_col = col
                while i < length and (source[i].isdigit() or source[i] == '.'):
                    num_str += source[i]
                    i += 1
                    col += 1
                tokens.append(Token(type="number", value=float(num_str) if '.' in num_str else int(num_str), line=line, column=start_col, file=file))
                continue

            # Strings: delimited by double quotes.
            if char == '"':
                i += 1
                start_col = col
                str_val = ""
                while i < length and source[i] != '"':
                    if source[i] == "\\" and i + 1 < length:
                        i += 1
                        if source[i] == "n": str_val += "\n"
                        elif source[i] == "t": str_val += "\t"
                        elif source[i] == "r": str_val += "\r"
                        elif source[i] == "\\": str_val += "\\"
                        elif source[i] == '"': str_val += '"'
                        else: str_val += source[i] # Unrecognized escape sequence
                    else:
                        str_val += source[i]
                    i += 1
                    col += 1
                if i >= length: # Unterminated string
                    raise NovaError(Token("error", char, file, line, col), "Unterminated string literal")
                i += 1 # Consume closing quote
                col += 1
                tokens.append(Token(type="string", value=str_val, line=line, column=start_col, file=file))
                continue

            # Identifiers, keywords, booleans.
            if char.isalpha() or char == '_':
                id_str = ""
                start_col = col
                while i < length and (source[i].isalnum() or source[i] == '_'):
                    id_str += source[i]
                    i += 1
                    col += 1
                if id_str == "true" or id_str == "false":
                    tokens.append(Token(type="boolean", value=(id_str == "true"), line=line, column=start_col, file=file))
                elif id_str in self.keywords:
                    tokens.append(Token(type="keyword", value=id_str, line=line, column=start_col, file=file))
                else:
                    tokens.append(Token(type="identifier", value=id_str, line=line, column=start_col, file=file))
                continue

            # Multi-character operators: ==, !=, >=, <=.
            if char in ("=", "!", "<", ">"):
                op = char
                start_col = col
                if i + 1 < length and source[i + 1] == "=":
                    op += "="
                    i += 2
                    col += 2
                else:
                    i += 1
                    col += 1
                tokens.append(Token(type="operator", value=op, line=line, column=start_col, file=file))
                continue

            # Dot operator for property access.
            if char == ".":
                tokens.append(Token(type="operator", value=".", line=line, column=col, file=file))
                i += 1
                col += 1
                continue

            # Single-character operators/punctuation.
            if char in "#+-*/%(),{}[]:":
                tokens.append(Token(type="operator", value=char, line=line, column=col, file=file))
                i += 1
                col += 1
                continue

            raise NovaError(Token(type="error", value=char, file=file, line=line, column=col), f"Unexpected character: {char}")
        return tokens

    # ----------------------
    # Token Parser Helpers
    # ----------------------
    def get_next_token(self) -> Token | None:
        """Returns the next token without consuming it."""
        if self.current < len(self.tokens):
            return self.tokens[self.current]
        return None

    def consume_token(self) -> Token:
        """Consumes and returns the current token, advancing the pointer."""
        if self.current >= len(self.tokens):
            # This should ideally not happen if parsing logic is correct
            last_token = self.tokens[-1] if self.tokens else Token("EOF", "EOF", self.file, 1, 1)
            raise NovaError(last_token, "Unexpected end of input while consuming token")
        token = self.tokens[self.current]
        self.current += 1
        return token

    def expect_type(self, type: str) -> Token:
        """Expects the next token to be of a specific type."""
        token = self.get_next_token()
        if not token or token.type != type:
            raise NovaError(token, f"Expected token type {type}, got {token.type if token else 'EOF'}")
        return token

    def expect_token(self, value: str) -> Token:
        """Expects the next token to have a specific value."""
        token = self.get_next_token()
        if not token:
            last_token = self.tokens[-1] if self.tokens else Token("EOF", "EOF", self.file, 1, 1)
            raise NovaError(last_token, f"Expected token '{value}', got EOF")
        if token.value != value:
            raise NovaError(token, f"Expected token '{value}', got {token.value}")
        return token

    def consume_expected(self, type: str) -> Token:
        """Helper to expect a type and then consume it."""
        self.expect_type(type)
        return self.consume_token()

    # ----------------------
    # Parsing Helpers
    # ----------------------
    def parse_block_until(self, terminators: list[str] | None = None) -> list[Statement]:
        """Parses a block of statements until a terminator keyword/operator is found."""
        statements: list[Statement] = []
        terminators = terminators or []
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

    def parse_block(self) -> list[Statement]:
        """Parses a block of statements until EOF or an implicit block end."""
        return self.parse_block_until()

    # ----------------------
    # Parsing Statements and Expressions
    # ----------------------
    def parse_statement(self) -> Statement:
        """Parses a single statement."""
        token = self.get_next_token()
        if not token:
            last_token = self.tokens[-1] if self.tokens else Token("EOF", "EOF", self.file, 1, 1)
            raise NovaError(last_token, "Unexpected end of input")

        # --- Label statement (not fully implemented in evaluator) ---
        if token.type == "keyword" and token.value == "label":
            self.consume_token()
            name_token = self.expect_type("identifier")
            name = name_token.value
            self.consume_token()
            return Statement(type="LabelStmt", name=name, file=token.file, line=token.line, column=token.column)

        # --- Defer statement ---
        if token.type == "keyword" and token.value == "defer":
            self.consume_token()
            body = self.parse_block_until(["end"])
            self.expect_token("end")
            self.consume_token()
            return DeferStmt(body=body, file=token.file, line=token.line, column=token.column)

        # --- Variable declaration with optional type annotation ---
        if token.type == "keyword" and token.value == "var":
            self.consume_token()
            name_token = self.expect_type("identifier")
            name = name_token.value
            self.consume_token()

            type_annotation: str | None = None
            next_token = self.get_next_token()
            if next_token and next_token.type == "identifier" and next_token.value in ["string", "number", "boolean", "void"]:
                type_annotation = next_token.value
                self.consume_token()

            modifier: str | None = None
            next_token = self.get_next_token()
            if next_token and next_token.type == "operator" and next_token.value == "#":
                self.consume_token()
                mod_token = self.expect_type("identifier")
                modifier = mod_token.value
                self.consume_token()

            self.expect_token("=")
            self.consume_token()
            initializer = self.parse_expression()
            return Statement(type="VarDecl", name=name, typeAnnotation=type_annotation, initializer=initializer, modifier=modifier, file=token.file, line=token.line, column=token.column)

        # --- Switch statement ---
        if token.type == "keyword" and token.value == "switch":
            self.consume_token()
            expression = self.parse_expression()
            cases: list[Case] = []
            while self.get_next_token() and self.get_next_token().type == "keyword" and self.get_next_token().value == "case":
                self.consume_token()
                case_expr = self.parse_expression()
                self.expect_token("do")
                self.consume_token()
                body = self.parse_block_until(["end"])
                self.expect_token("end")
                self.consume_token()
                cases.append(Case(case_expr=case_expr, body=body))
            
            # Default case
            if self.get_next_token() and self.get_next_token().type == "keyword" and self.get_next_token().value == "default":
                self.consume_token()
                self.expect_token("do")
                self.consume_token()
                body = self.parse_block_until(["end"])
                self.expect_token("end")
                self.consume_token()
                cases.append(Case(case_expr=None, body=body)) # None indicates default case

            self.expect_token("end")
            self.consume_token()
            return Statement(type="SwitchStmt", expression=expression, cases=cases, file=token.file, line=token.line, column=token.column)

        # --- using statement ---
        if token.type == "keyword" and token.value == "using":
            self.consume_token()
            name_token = self.expect_type("identifier")
            self.consume_token()
            return Statement(type="UsingStmt", name=name_token.value, file=token.file, line=token.line, column=token.column)

        # --- Try/Catch statement ---
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
            return Statement(type="TryStmt", tryBlock=try_block, errorVar=error_var, catchBlock=catch_block, file=token.file, line=token.line, column=token.column)

        # --- ForEach loop ---
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
            return Statement(type="ForEachStmt", variable=variable, list=list_expr, body=body, file=token.file, line=token.line, column=token.column)

        # --- For loop ---
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
            step_expr: Expression | None = None
            if self.get_next_token() and self.get_next_token().value == ",":
                self.consume_token()
                step_expr = self.parse_expression()
            self.expect_token("do")
            self.consume_token()
            body = self.parse_block_until(["end"])
            self.expect_token("end")
            self.consume_token()
            return Statement(type="ForStmt", variable=variable, start=start_expr, end=end_expr, step=step_expr, body=body, file=token.file, line=token.line, column=token.column)

        # --- While loop ---
        if token.type == "keyword" and token.value == "while":
            self.consume_token()
            condition = self.parse_expression()
            body = self.parse_block_until(["end"])
            self.expect_token("end")
            self.consume_token()
            return Statement(type="WhileStmt", condition=condition, body=body, file=token.file, line=token.line, column=token.column)

        # --- Break statement ---
        if token.type == "keyword" and token.value == "break":
            self.consume_token()
            return Statement(type="BreakStmt", file=token.file, line=token.line, column=token.column)

        # --- Continue statement ---
        if token.type == "keyword" and token.value == "continue":
            self.consume_token()
            return Statement(type="ContinueStmt", file=token.file, line=token.line, column=token.column)

        # --- If/Elseif/Else statement ---
        if token.type == "keyword" and token.value == "if":
            self.consume_token()
            condition = self.parse_expression()
            then_block = self.parse_block_until(["else", "elseif", "end"])

            else_if_blocks: list[dict] = []
            else_block: list[Statement] | None = None

            while self.get_next_token() and self.get_next_token().type == "keyword" and self.get_next_token().value == "elseif":
                self.consume_token() # consume 'elseif'
                elseif_condition = self.parse_expression()
                elseif_body = self.parse_block_until(["else", "elseif", "end"])
                else_if_blocks.append({"condition": elseif_condition, "body": elseif_body})

            if self.get_next_token() and self.get_next_token().type == "keyword" and self.get_next_token().value == "else":
                self.consume_token()
                else_block = self.parse_block_until(["end"])

            self.expect_token("end")
            self.consume_token()

            return Statement(
                type="IfStmt",
                condition=condition,
                thenBlock=then_block,
                elseBlock=else_block,
                elseIf=else_if_blocks if else_if_blocks else None,
                file=token.file, line=token.line, column=token.column
            )

        # --- Namespace statement ---
        if token.type == "keyword" and token.value == "namespace":
            self.consume_token()
            name_token = self.expect_type("identifier")
            name = name_token.value
            self.consume_token()
            body = self.parse_block_until(["end"])
            self.expect_token("end")
            self.consume_token()
            return Statement(type="NamespaceStmt", name=name, body=body, file=token.file, line=token.line, column=token.column)

        # --- Function Declaration ---
        if token.type == "keyword" and token.value == "func":
            self.consume_token()
            name_token = self.expect_type("identifier")
            name = name_token.value
            self.consume_token()
            self.expect_token("(")
            self.consume_token()

            parameters: list[Parameter] = []
            if self.get_next_token() and self.get_next_token().value != ")":
                while True:
                    param_token = self.expect_type("identifier")
                    param_name = param_token.value
                    self.consume_token()

                    param_type: str | None = None
                    default_expr: Expression | None = None

                    if self.get_next_token() and self.get_next_token().type == "identifier":
                        type_token = self.get_next_token()
                        if type_token.value in ["string", "number", "bool"]:
                            param_type = type_token.value
                            self.consume_token()

                    if self.get_next_token() and self.get_next_token().value == "=":
                        self.consume_token()
                        default_expr = self.parse_expression()
                        if param_type:
                            raise NovaError(token, "cannot have both type and default value, as that prevents type inferring")
                        # Infer type from default value if not explicitly given
                        if default_expr.type == "Literal":
                            if isinstance(default_expr.value, str): param_type = "string"
                            elif isinstance(default_expr.value, (int, float)): param_type = "number"
                            elif isinstance(default_expr.value, bool): param_type = "bool"
                        # else: param_type = default_expr.type # This might be too broad

                    parameters.append(Parameter(name=param_name, type=param_type, default=default_expr,
                                                file=param_token.file, line=param_token.line, column=param_token.column,
                                                token_type=param_token.type, token_value=param_token.value))

                    if self.get_next_token() and self.get_next_token().value == ",":
                        self.consume_token()
                    else:
                        break
            self.expect_token(")")
            self.consume_token()
            body = self.parse_block_until(["end"])
            self.expect_token("end")
            self.consume_token()

            return Statement(
                type="FuncDecl",
                name=name,
                parameters=parameters,
                body=body,
                file=token.file, line=token.line, column=token.column
            )

        # --- Lambda Declaration (def) ---
        if token.type == "keyword" and token.value == "def":
            self.consume_token()
            self.expect_token("(")
            self.consume_token()

            parameters: list[Parameter] = []
            if self.get_next_token() and self.get_next_token().value != ")":
                while True:
                    param_token = self.expect_type("identifier")
                    param_name = param_token.value
                    self.consume_token()

                    param_type: str | None = None
                    default_expr: Expression | None = None

                    if self.get_next_token() and self.get_next_token().type == "identifier":
                        type_token = self.get_next_token()
                        if type_token.value in ["string", "number", "bool"]:
                            param_type = type_token.value
                            self.consume_token()

                    if self.get_next_token() and self.get_next_token().value == "=":
                        self.consume_token()
                        default_expr = self.parse_expression()
                        if param_type:
                            raise NovaError(token, "cannot have both type and default value, as that prevents type inferring")
                        # Infer type from default value
                        if default_expr.type == "Literal":
                            if isinstance(default_expr.value, str): param_type = "string"
                            elif isinstance(default_expr.value, (int, float)): param_type = "number"
                            elif isinstance(default_expr.value, bool): param_type = "bool"

                    parameters.append(Parameter(name=param_name, type=param_type, default=default_expr,
                                                file=param_token.file, line=param_token.line, column=param_token.column,
                                                token_type=param_token.type, token_value=param_token.value))

                    if self.get_next_token() and self.get_next_token().value == ",":
                        self.consume_token()
                    else:
                        break
            self.expect_token(")")
            self.consume_token()
            body = self.parse_block_until(["end"])
            self.expect_token("end")
            self.consume_token()

            return Statement(
                type="LambdaDecl",
                parameters=parameters,
                body=body,
                file=token.file, line=token.line, column=token.column
            )

        # --- Return statement ---
        if token.type == "keyword" and token.value == "return":
            self.consume_token()
            expression: Expression | None = None
            if self.get_next_token() and self.get_next_token().value not in self.keywords:
                expression = self.parse_expression()
            return Statement(type="ReturnStmt", expression=expression, file=token.file, line=token.line, column=token.column)

        # --- Import statement ---
        if token.type == "keyword" and token.value == "import":
            self.consume_token()
            file_token = self.expect_type("string")
            filename = file_token.value
            self.consume_token()
            alias: str | None = None
            if self.get_next_token() and self.get_next_token().value == "as":
                self.consume_token()
                alias_token = self.expect_type("identifier")
                alias = alias_token.value
                self.consume_token()
            return Statement(type="ImportStmt", filename=filename, alias=alias, file=token.file, line=token.line, column=token.column)

        # --- Expression statement (fallback) ---
        expr = self.parse_expression()
        return Statement(type="ExpressionStmt", expression=expr, file=token.file, line=token.line, column=token.column)

    # --- Expression Parsing (Recursive Descent) ---
    def parse_expression(self) -> Expression:
        """Entry point for expression parsing."""
        return self.parse_assignment()

    def parse_assignment(self) -> Expression:
        """Parses assignment expressions (lowest precedence)."""
        expr = self.parse_logical_or()
        next_token = self.get_next_token()
        if next_token and next_token.type == "operator" and next_token.value == "=":
            assignment_op_token = self.consume_token()
            value_expr = self.parse_assignment()
            expr = Expression(type="AssignmentExpr", target=expr, value=value_expr,
                              file=assignment_op_token.file, line=assignment_op_token.line, column=assignment_op_token.column)
        return expr

    def parse_logical_or(self) -> Expression:
        """Parses logical OR expressions."""
        expr = self.parse_logical_and()
        while self.get_next_token() and self.get_next_token().type == "operator" and self.get_next_token().value == "||":
            operator_token = self.consume_token()
            operator = operator_token.value
            right = self.parse_logical_and()
            expr = Expression(type="BinaryExpr", operator=operator, left=expr, right=right,
                              file=operator_token.file, line=operator_token.line, column=operator_token.column)
        return expr

    def parse_logical_and(self) -> Expression:
        """Parses logical AND expressions."""
        expr = self.parse_equality()
        while self.get_next_token() and self.get_next_token().type == "operator" and self.get_next_token().value == "&&":
            operator_token = self.consume_token()
            operator = operator_token.value
            right = self.parse_equality()
            expr = Expression(type="BinaryExpr", operator=operator, left=expr, right=right,
                              file=operator_token.file, line=operator_token.line, column=operator_token.column)
        return expr

    def parse_equality(self) -> Expression:
        """Parses equality expressions (==, !=)."""
        expr = self.parse_comparison()
        while self.get_next_token() and self.get_next_token().type == "operator" and self.get_next_token().value in ("==", "!="):
            operator_token = self.consume_token()
            operator = operator_token.value
            right = self.parse_comparison()
            expr = Expression(type="BinaryExpr", operator=operator, left=expr, right=right,
                              file=operator_token.file, line=operator_token.line, column=operator_token.column)
        return expr

    def parse_comparison(self) -> Expression:
        """Parses comparison expressions (<, <=, >, >=)."""
        expr = self.parse_term()
        while self.get_next_token() and self.get_next_token().type == "operator" and self.get_next_token().value in ("<", "<=", ">", ">="):
            operator_token = self.consume_token()
            operator = operator_token.value
            right = self.parse_term()
            expr = Expression(type="BinaryExpr", operator=operator, left=expr, right=right,
                              file=operator_token.file, line=operator_token.line, column=operator_token.column)
        return expr

    def parse_term(self) -> Expression:
        """Parses additive expressions (+, -)."""
        expr = self.parse_factor()
        while self.get_next_token() and self.get_next_token().type == "operator" and self.get_next_token().value in ("+", "-"):
            operator_token = self.consume_token()
            operator = operator_token.value
            right = self.parse_factor()
            expr = Expression(type="BinaryExpr", operator=operator, left=expr, right=right,
                              file=operator_token.file, line=operator_token.line, column=operator_token.column)
        return expr

    def parse_factor(self) -> Expression:
        """Parses multiplicative expressions (*, /, %)."""
        expr = self.parse_unary()
        while self.get_next_token() and self.get_next_token().type == "operator" and self.get_next_token().value in ("*", "/", "%"):
            operator_token = self.consume_token()
            operator = operator_token.value
            right = self.parse_unary()
            expr = Expression(type="BinaryExpr", operator=operator, left=expr, right=right,
                              file=operator_token.file, line=operator_token.line, column=operator_token.column)
        return expr

    def parse_unary(self) -> Expression:
        """Parses unary expressions (-, !)."""
        next_token = self.get_next_token()
        if next_token and next_token.type == "operator" and next_token.value in ("-", "!"):
            operator_token = self.consume_token()
            operator = operator_token.value
            right = self.parse_unary()
            return Expression(type="UnaryExpr", operator=operator, right=right,
                              file=operator_token.file, line=operator_token.line, column=operator_token.column)
        return self.parse_primary()

    def parse_primary(self) -> Expression:
        """Parses primary expressions (literals, identifiers, grouped expressions, array/object literals)."""
        node: Expression
        token = self.get_next_token()
        if not token:
            last_token = self.tokens[-1] if self.tokens else Token("EOF", "EOF", self.file, 1, 1)
            raise NovaError(last_token, "Unexpected end of input")

        if token.type == "boolean":
            self.consume_token()
            node = Literal(value=token.value, file=token.file, line=token.line, column=token.column)
        elif token.type in ("number", "string"):
            self.consume_token()
            node = Literal(value=token.value, file=token.file, line=token.line, column=token.column)
        elif token.value == "[": # Array literal
            self.consume_token()
            elements: list[Expression] = []
            if self.get_next_token() and self.get_next_token().value != "]":
                while True:
                    elements.append(self.parse_expression())
                    if self.get_next_token() and self.get_next_token().value == ",":
                        self.consume_token()
                    else:
                        break
            self.expect_token("]")
            self.consume_token()
            node = Expression(type="ArrayLiteral", elements=elements, file=token.file, line=token.line, column=token.column)
        elif token.value == "{": # Object literal
            self.consume_token()
            properties: list[dict] = []
            if self.get_next_token() and self.get_next_token().value != "}":
                while True:
                    key_token = self.get_next_token()
                    if not key_token or key_token.type not in ("identifier", "string"):
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
            node = Expression(type="ObjectLiteral", properties=properties, file=token.file, line=token.line, column=token.column)
        elif token.type == "identifier":
            self.consume_token()
            if self.get_next_token() and self.get_next_token().value == "[": # Array access
                self.consume_token()
                index = self.parse_expression()
                self.expect_token("]")
                self.consume_token()
                node = ArrayAccess(name=token.value, index=index, file=token.file, line=token.line, column=token.column)
            elif self.get_next_token() and self.get_next_token().value == "(": # Function call
                self.consume_token()
                args: list[Expression] = []
                if self.get_next_token() and self.get_next_token().value != ")":
                    while True:
                        args.append(self.parse_expression())
                        if self.get_next_token() and self.get_next_token().value == ",":
                            self.consume_token()
                        else:
                            break
                self.expect_token(")")
                self.consume_token()
                node = FunctionCall(name=token.value, arguments=args, file=token.file, line=token.line, column=token.column)
            else: # Simple identifier
                node = Expression(type="Identifier", name=token.value, file=token.file, line=token.line, column=token.column)
        elif token.value == "def": # Inline Lambda Declaration
            # This is handled as a statement in parse_statement, but also as an expression here.
            # The structure is identical to the FuncDecl parsing for parameters and body.
            self.consume_token()
            self.expect_token("(")
            self.consume_token()

            parameters: list[Parameter] = []
            if self.get_next_token() and self.get_next_token().value != ")":
                while True:
                    param_token = self.expect_type("identifier")
                    param_name = param_token.value
                    self.consume_token()

                    param_type: str | None = None
                    default_expr: Expression | None = None

                    if self.get_next_token() and self.get_next_token().type == "identifier":
                        type_token = self.get_next_token()
                        if type_token.value in ["string", "number", "bool"]:
                            param_type = type_token.value
                            self.consume_token()

                    if self.get_next_token() and self.get_next_token().value == "=":
                        self.consume_token()
                        default_expr = self.parse_expression()
                        if param_type:
                            raise NovaError(token, "cannot have both type and default value, as that prevents type inferring")
                        # Infer type from default value
                        if default_expr.type == "Literal":
                            if isinstance(default_expr.value, str): param_type = "string"
                            elif isinstance(default_expr.value, (int, float)): param_type = "number"
                            elif isinstance(default_expr.value, bool): param_type = "bool"

                    parameters.append(Parameter(name=param_name, type=param_type, default=default_expr,
                                                file=param_token.file, line=param_token.line, column=param_token.column,
                                                token_type=param_token.type, token_value=param_token.value))

                    if self.get_next_token() and self.get_next_token().value == ",":
                        self.consume_token()
                    else:
                        break
            self.expect_token(")")
            self.consume_token()
            body = self.parse_block_until(["end"])
            self.expect_token("end")
            self.consume_token()

            node = Expression(
                type="LambdaDecl",
                parameters=parameters,
                body=body,
                file=token.file, line=token.line, column=token.column
            )
        elif token.value == "(": # Grouped expression
            self.consume_token()
            node = self.parse_expression()
            self.expect_token(")")
            self.consume_token()
        else:
            raise NovaError(token, f"Unexpected token: {token.value}")

        # Handle property access and method calls (chained calls)
        while self.get_next_token() and self.get_next_token().value == ".":
            dot_token = self.consume_token() # consume "."
            prop_token = self.expect_type("identifier")
            prop_name = prop_token.value
            self.consume_token() # consume identifier

            if self.get_next_token() and self.get_next_token().value == "(": # Method call
                self.consume_token() # consume "("
                args: list[Expression] = []
                while self.get_next_token() and self.get_next_token().value != ")":
                    args.append(self.parse_expression())
                    if self.get_next_token() and self.get_next_token().value == ",":
                        self.consume_token()
                    else:
                        break
                self.expect_token(")")
                self.consume_token() # consume ")"
                node = MethodCall(object=node, method=prop_name, arguments=args,
                                  file=prop_token.file, line=prop_token.line, column=prop_token.column)
            else: # Plain property access
                node = PropertyAccess(object=node, property=prop_name,
                                      file=prop_token.file, line=prop_token.line, column=prop_token.column)
        return node

    # ----------------------
    # Evaluation / Execution
    # ----------------------
    def interpret(self):
        """Starts the interpretation process."""
        statements = self.parse_block()
        try:
            self.execute_block(statements, self.globals)
        except Exception as err:
            # Ensure deferred statements still execute even if there's an error
            self.globals.execute_deferred(self)
            raise err

    def execute_block(self, statements: list[Statement], env: Environment):
        """Executes a block of statements within a given environment."""
        previous_env = self.current_env
        self.current_env = env

        try:
            for stmt in statements:
                self.execute_stmt(stmt, env)
        finally:
            # Execute deferred statements when exiting the block
            env.execute_deferred(self)
            self.current_env = previous_env

    def get_current_context(self) -> Environment | None:
        """Returns the current execution environment."""
        return self.current_env

    def execute_stmt(self, stmt: Statement, env: Environment):
        """Executes a single statement."""
        if stmt.type == "VarDecl":
            value = self.evaluate_expr(stmt.initializer, env)
            if stmt.typeAnnotation:
                check_type(stmt.typeAnnotation, value, stmt)
            env.define(stmt.name, value)
        elif stmt.type == "DeferStmt":
            # Add statements to be executed when the block exits
            # The original JS code reversed the order of the body statements before adding to deferred.
            # Python's list.extend(reversed(list)) or just appending and popping will achieve this.
            # The current TS code seems to push each statement and then reverse the stack, which is equivalent to pushing in reverse.
            # Let's push in reverse order so pop() gets the first deferred statement.
            for s in reversed(stmt.body):
                env.add_deferred(s)
        elif stmt.type == "ExpressionStmt":
            self.evaluate_expr(stmt.expression, env)
        elif stmt.type == "BreakStmt":
            raise BreakException()
        elif stmt.type == "ContinueStmt":
            raise ContinueException()
        elif stmt.type == "TryStmt":
            try:
                self.execute_block(stmt.tryBlock, env)
            except Exception as e:
                catch_env = Environment(env)
                # If e is an Error or NovaError, pass it. Otherwise, wrap it.
                error_to_define = e if isinstance(e, (Exception, NovaError)) else NovaError(stmt, str(e))
                catch_env.define(stmt.errorVar, error_to_define)
                self.execute_block(stmt.catchBlock, catch_env)
        elif stmt.type == "IfStmt":
            condition_val = self.evaluate_expr(stmt.condition, env)
            if condition_val:
                self.execute_block(stmt.thenBlock, Environment(env))
            elif stmt.elseIf:
                matched = False
                # No need for a separate elseifEnv, just use a new Environment for each block if needed
                for elseif_block in stmt.elseIf:
                    elseif_condition = self.evaluate_expr(elseif_block["condition"], env)
                    if elseif_condition:
                        self.execute_block(elseif_block["body"], Environment(env))
                        matched = True
                        break
                if not matched and stmt.elseBlock:
                    self.execute_block(stmt.elseBlock, Environment(env))
            elif stmt.elseBlock:
                self.execute_block(stmt.elseBlock, Environment(env))
        elif stmt.type == "WhileStmt":
            while self.evaluate_expr(stmt.condition, env):
                try:
                    self.execute_block(stmt.body, Environment(env))
                except BreakException:
                    break
                except ContinueException:
                    continue
                except Exception as e:
                    raise e
        elif stmt.type == "ForEachStmt":
            list_val = self.evaluate_expr(stmt.list, env)
            if not isinstance(list_val, list):
                raise NovaError(stmt, f"Cannot iterate over non-list type for forEach loop. Got: {type(list_val).__name__}")
            for item in list_val:
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
        elif stmt.type == "ForStmt":
            start = self.evaluate_expr(stmt.start, env)
            end = self.evaluate_expr(stmt.end, env)
            step = self.evaluate_expr(stmt.step, env) if stmt.step else 1

            if not all(isinstance(val, (int, float)) for val in [start, end, step]):
                raise NovaError(stmt, f"For loop bounds and step must be numbers. Got start: {type(start).__name__}, end: {type(end).__name__}, step: {type(step).__name__}")

            # Python's range/xrange is for integers. For floats, we need a manual loop.
            current_val = start
            while (step > 0 and current_val <= end) or (step < 0 and current_val >= end):
                loop_env = Environment(env)
                loop_env.define(stmt.variable, current_val)
                try:
                    self.execute_block(stmt.body, loop_env)
                except BreakException:
                    break
                except ContinueException:
                    pass # Continue to next iteration
                except Exception as e:
                    raise e
                current_val += step
        elif stmt.type == "ImportStmt":
            file_path = stmt.filename
            if file_path.startswith("os:"):
                file_path = file_path[3:]
                if not env.has("os-import-handler"):
                    raise NovaError(stmt, "os-import-handler is not defined, your runtime should define it, interpreter.globals.define('os-import-handler', handler)")
                handler = env.get("os-import-handler", stmt)
                result = handler(file_path)
                name = stmt.alias or file_path # Use filename as name if no alias
                env.define(name, result)
            else:
                file_path += ".nova" # NovaScript files typically end with .nova
                file_dir = os.path.dirname(self.file)
                full_path = os.path.abspath(os.path.join(file_dir, file_path))

                if full_path in self.imported_files:
                    return # Already imported

                self.imported_files.add(full_path)

                if not os.path.exists(full_path):
                    raise NovaError(stmt, f"Import error: File not found at '{full_path}'")

                imported_interpreter = Interpreter(full_path)
                imported_env = Environment(self.globals) # Imported files share global scope
                imported_interpreter.globals = imported_env # Overwrite globals for the imported interpreter
                imported_interpreter.functions = self.functions # Share function definitions
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
            env.define(stmt.name, n_env) # Store the environment as the namespace
        elif stmt.type == "SwitchStmt":
            value = self.evaluate_expr(stmt.expression, env)
            matched = False
            default_case_body: list[Statement] | None = None

            for c in stmt.cases:
                if c.case_expr is None: # This is the default case
                    default_case_body = c.body
                else:
                    case_val = self.evaluate_expr(c.case_expr, env)
                    if value == case_val:
                        self.execute_block(c.body, Environment(env))
                        matched = True
                        break # Exit switch after first match

            if not matched and default_case_body:
                self.execute_block(default_case_body, Environment(env))
        elif stmt.type == "ReturnStmt":
            value = self.evaluate_expr(stmt.expression, env) if stmt.expression else None
            raise ReturnException(value)
        elif stmt.type == "FuncDecl":
            # Define a Python function that wraps the NovaScript function logic
            def nova_function(*args_passed):
                func_env = Environment(env)

                if len(args_passed) > len(stmt.parameters):
                    raise NovaError(stmt, f"Too many arguments passed to function '{stmt.name}'. Expected {len(stmt.parameters)}, got {len(args_passed)}.")

                for i, param in enumerate(stmt.parameters):
                    arg_val = args_passed[i] if i < len(args_passed) else None

                    # Apply default if missing
                    if arg_val is None and param.default is not None:
                        arg_val = self.evaluate_expr(param.default, env)
                    elif arg_val is None and param.default is None:
                        raise NovaError(param, f"Missing argument for parameter '{param.name}' in function '{stmt.name}'.")

                    # Soft type check
                    if param.type:
                        expected_type = param.type
                        if expected_type == "number" and not isinstance(arg_val, (int, float)):
                            raise NovaError(param, f"Type mismatch in function '{stmt.name}': parameter '{param.name}' expected number, got {type(arg_val).__name__}")
                        elif expected_type == "string" and not isinstance(arg_val, str):
                            raise NovaError(param, f"Type mismatch in function '{stmt.name}': parameter '{param.name}' expected string, got {type(arg_val).__name__}")
                        elif expected_type == "bool" and not isinstance(arg_val, bool):
                            raise NovaError(param, f"Type mismatch in function '{stmt.name}': parameter '{param.name}' expected bool, got {type(arg_val).__name__}")

                    func_env.define(param.name, arg_val)

                try:
                    self.execute_block(stmt.body, func_env)
                except ReturnException as e:
                    return e.value
                except Exception as e:
                    raise e
                return None # Functions without explicit return return None

            env.define(stmt.name, nova_function)
        elif stmt.type == "UsingStmt":
            namespace = env.get(stmt.name, stmt)
            if isinstance(namespace, Environment):
                for key, value in namespace.values.items():
                    env.define(key, value)
            elif isinstance(namespace, dict): # For Python objects exposed as namespaces
                for key, value in namespace.items():
                    env.define(key, value)
            else:
                raise NovaError(stmt, f"Cannot 'use' non-namespace value '{stmt.name}'.")
        else:
            raise NovaError(stmt, f"Unknown statement type: {stmt.type}")

    def expand_array_target(self, expr: ArrayAccess, env: Environment) -> tuple[any, any]:
        """Helper to get the array and index for array assignments/access."""
        array_name = expr.name
        index_expr = expr.index

        arr = env.get(array_name, expr)
        index = self.evaluate_expr(index_expr, env)

        if not isinstance(arr, (list, dict)): # Python lists/dicts can be indexed
            raise NovaError(expr, f"Cannot index into non-array/object: {type(arr).__name__}")

        return arr, index

    def expand_prop_target(self, expr: PropertyAccess, env: Environment) -> tuple[any, str]:
        """Helper to get the object and key for property assignments/access."""
        current: Expression = expr
        chain: list[str] = []

        # Traverse the property access chain to get the base object and all properties
        while isinstance(current, PropertyAccess):
            chain.insert(0, current.property) # Insert at beginning to reverse order
            current = current.object

        if not isinstance(current, Expression) or current.type != "Identifier":
            raise NovaError(expr, "Invalid base for property access: " + current.type)

        obj = env.get(current.name, current)

        for i in range(len(chain) - 1):
            key = chain[i]
            if obj is None or not isinstance(obj, (dict, object)): # Check for dict or object
                raise NovaError(expr, f"Cannot access property '{key}' on non-object (or None). Got: {type(obj).__name__}")
            obj = getattr(obj, key, obj.get(key)) if isinstance(obj, dict) else getattr(obj, key) # Handle dicts and objects

        return obj, chain[-1] # Return the final object and the last key

    def evaluate_expr(self, expr: Expression, env: Environment) -> any:
        """Evaluates a single expression."""
        if expr.type == "Literal":
            return expr.value
        elif expr.type == "Identifier":
            return env.get(expr.name, expr)
        elif expr.type == "AssignmentExpr":
            value = self.evaluate_expr(expr.value, env)
            if expr.target.type == "ArrayAccess":
                arr, index = self.expand_array_target(expr.target, env)
                if arr is None:
                    raise NovaError(expr.target, "Cannot assign to index of null or undefined value.")
                if not isinstance(index, (int, str)):
                    raise NovaError(expr.target, f"Array index must be a number or string. Got: {type(index).__name__}")
                arr[index] = value
            elif expr.target.type == "PropertyAccess":
                obj, key = self.expand_prop_target(expr.target, env)
                if isinstance(obj, Environment):
                    raise NovaError(expr.target, "Cannot assign to environment properties directly.")
                if obj is None:
                    raise NovaError(expr.target, f"Cannot assign property '{key}' of null or undefined value.")
                if isinstance(obj, dict):
                    obj[key] = value
                else: # Assume it's a Python object
                    setattr(obj, key, value)
            elif expr.target.type == "Identifier":
                env.assign(expr.target.name, value, expr.target)
            else:
                raise NovaError(expr.target, f"Unsupported assignment target: {expr.target.type}({expr.target.__dict__})")
            return value
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
                raise NovaError(expr, f"Cannot call method '{expr.method}' on null or undefined.")

            # Try getting method from Environment first if obj is an Environment
            if isinstance(obj, Environment):
                fn = obj.get(expr.method, expr)
            else:
                fn = getattr(obj, expr.method, None) # Get method from Python object

            if not callable(fn):
                raise NovaError(expr, f"{expr.method} is not a function or method on this object")

            arg_vals = [self.evaluate_expr(arg, env) for arg in expr.arguments]
            # If it's a method from a Python object, bind it.
            # If it's a function from an Environment, it's already bound or standalone.
            if isinstance(obj, Environment):
                return fn(*arg_vals)
            else:
                return fn(*arg_vals) # Python automatically binds methods
        elif expr.type == "ArrayAccess":
            arr = env.get(expr.name, expr)
            index = self.evaluate_expr(expr.index, env)
            if not isinstance(arr, (list, dict)):
                raise NovaError(expr, f"Cannot access index of non-array/object: {type(arr).__name__}")
            if not isinstance(index, (int, str)):
                raise NovaError(expr, f"Array/object index must be a number or string. Got: {type(index).__name__}")
            return arr[index]
        elif expr.type == "PropertyAccess":
            obj = self.evaluate_expr(expr.object, env)
            if obj is None:
                raise NovaError(expr, f"Cannot access property '{expr.property}' of null or undefined.")
            if isinstance(obj, Environment):
                return obj.get(expr.property, expr)
            elif isinstance(obj, dict):
                return obj.get(expr.property)
            else: # Assume it's a Python object
                return getattr(obj, expr.property, None) # Return None if attribute doesn't exist
        elif expr.type == "ArrayLiteral":
            return [self.evaluate_expr(element, env) for element in expr.elements]
        elif expr.type == "LambdaDecl":
            # Define a Python function that wraps the NovaScript lambda logic
            def nova_lambda(*args_passed):
                func_env = Environment(env) # Closure over the environment where lambda was defined

                if len(args_passed) > len(expr.parameters):
                    raise NovaError(expr, f"Too many arguments passed to lambda. Expected {len(expr.parameters)}, got {len(args_passed)}.")

                for i, param in enumerate(expr.parameters):
                    arg_val = args_passed[i] if i < len(args_passed) else None

                    if arg_val is None and param.default is not None:
                        arg_val = self.evaluate_expr(param.default, env) # Evaluate default in definition env
                    elif arg_val is None and param.default is None:
                        raise NovaError(param, f"Missing argument for parameter '{param.name}' in lambda.")

                    if param.type:
                        expected_type = param.type
                        if expected_type == "number" and not isinstance(arg_val, (int, float)):
                            raise NovaError(param, f"Type mismatch in lambda: parameter '{param.name}' expected number, got {type(arg_val).__name__}")
                        elif expected_type == "string" and not isinstance(arg_val, str):
                            raise NovaError(param, f"Type mismatch in lambda: parameter '{param.name}' expected string, got {type(arg_val).__name__}")
                        elif expected_type == "bool" and not isinstance(arg_val, bool):
                            raise NovaError(param, f"Type mismatch in lambda: parameter '{param.name}' expected bool, got {type(arg_val).__name__}")

                    func_env.define(param.name, arg_val)

                try:
                    self.execute_block(expr.body, func_env)
                except ReturnException as e:
                    return e.value
                except Exception as e:
                    raise e
                return None # Lambdas without explicit return return None
            return nova_lambda
        elif expr.type == "ObjectLiteral":
            obj: dict[str, any] = {}
            for prop in expr.properties:
                obj[prop["key"]] = self.evaluate_expr(prop["value"], env)
            return obj
        else:
            raise NovaError(expr, f"Unknown expression type: {expr.type}")

# Example usage (if this were a script):
# if __name__ == "__main__":
#     if len(sys.argv) < 2:
#         print("Usage: python nova_interpreter.py <script_file.nova>")
#         sys.exit(1)
#     script_file = sys.argv[1]
#     interpreter = Interpreter(script_file)
#     try:
#         interpreter.interpret()
#     except NovaError as e:
#         print(f"NovaScript Error: {e}")
#         sys.exit(1)
#     except Exception as e:
#         print(f"Interpreter Error: {e}")
#         sys.exit(1)