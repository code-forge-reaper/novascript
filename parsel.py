#!/usr/bin/env python3
"""
Parsel - a Ren'Py-like game engine for text adventures.
Reuses NovaScript's lexer and adds a complete parser/runtime.
"""

import sys
import os
import re
import importlib
from typing import List, Dict, Any, Optional, Union, Tuple, Callable

# ----------------------------------------------------------------------
# Tokenizer (copied from NovaScript)
# ----------------------------------------------------------------------


class Token:
    __slots__ = ("type", "value", "file", "line", "column")

    def __init__(self, type_: str, value: Any,
                 file: str, line: int, column: int):
        self.type = type_
        self.value = value
        self.file = file
        self.line = line
        self.column = column

    def __repr__(self):
        return (f"Token({self.type}, {self.value!r}, " +
                f"{self.file}:{self.line}:{self.column})")


class ParselError(Exception):
    def __init__(self, token: Optional[Token], message: str):
        if token:
            super().__init__(
                f"{token.file}:{token.line}:{token.column} {message}")
        else:
            super().__init__(message)
        self.token = token


def tokenize(source: str, file: str) -> List[Token]:
    keywords = {
        "var",
        "func",
        "return",
        "def",
        "as",
        "end",
        "char",
        "scene",
        "say",
        "narrate",
        "think",
        "options",
        "begin",
        "goto",
        "if",
        "else",
        "elseif",
        "pause",
        "exit",
        "using",
        "true",
        "false",
    }
    tokens = []
    i = 0
    line = 1
    col = 1
    length = len(source)

    while i < length:
        ch = source[i]
        start_col = col

        if ch.isspace():
            if ch == "\n":
                line += 1
                col = 1
            else:
                col += 1
            i += 1
            continue

        # line comment
        if ch == "/" and i + 1 < length and source[i + 1] == "/":
            while i < length and source[i] != "\n":
                i += 1
            continue

        # block comment
        if ch == "/" and i + 1 < length and source[i + 1] == "*":
            i += 2
            col += 2
            while i + 1 < length and not (source[i] == "*" and source[i + 1] == "/"):
                if source[i] == "\n":
                    line += 1
                    col = 1
                else:
                    col += 1
                i += 1
            if i + 1 < length:
                i += 2
                col += 2
            else:
                raise ParselError(None, "Unterminated multi-line comment")
            continue

        # two‑char operators
        two_char_ops = [
            "+=",
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
        ]
        if i + 1 < length and source[i: i + 2] in two_char_ops:
            tokens.append(
                Token("operator", source[i: i + 2], file, line, start_col))
            i += 2
            col += 2
            continue

        # single‑char operators / punctuation
        single_ops = "=+-*/%<>!."
        punct = "()[]{},:#"
        if ch in single_ops or ch in punct:
            tokens.append(Token("operator", ch, file, line, start_col))
            i += 1
            col += 1
            continue

        # numbers
        if ch.isdigit():
            num = ""
            while i < length and (source[i].isdigit() or source[i] == "."):
                num += source[i]
                i += 1
                col += 1
            value = float(num) if "." in num else int(num)
            tokens.append(Token("number", value, file, line, start_col))
            continue

        # strings
        if ch == '"':
            i += 1
            s = ""
            while i < length and source[i] != '"':
                if source[i] == "\\" and i + 1 < length:
                    i += 1
                    esc = source[i]
                    if esc == "n":
                        s += "\n"
                    elif esc == "t":
                        s += "\t"
                    elif esc == "r":
                        s += "\r"
                    elif esc == "\\":
                        s += "\\"
                    elif esc == '"':
                        s += '"'
                    else:
                        s += esc
                else:
                    s += source[i]
                i += 1
                col += 1
            if i < length and source[i] == '"':
                i += 1
                col += 1
            else:
                raise ParselError(None, "Unterminated string")
            tokens.append(Token("string", s, file, line, start_col))
            continue

        # identifiers & keywords
        if ch.isalpha() or ch == "_":
            ident = ""
            while i < length and (source[i].isalnum() or source[i] == "_"):
                ident += source[i]
                i += 1
                col += 1
            if ident in ("true", "false"):
                tokens.append(Token("boolean", ident ==
                              "true", file, line, start_col))
            elif ident in keywords:
                tokens.append(Token("keyword", ident, file, line, start_col))
            else:
                tokens.append(
                    Token("identifier", ident, file, line, start_col))
            continue

        raise ParselError(
            Token("error", ch, file, line,
                  start_col), f"Unexpected character: {ch}"
        )

    return tokens


# ----------------------------------------------------------------------
# AST Nodes (game-specific)
# ----------------------------------------------------------------------


class Expr:
    pass


class Stmt:
    pass


class Literal(Expr):
    def __init__(self, value: Any, token: Token):
        self.value = value
        self.token = token


class Identifier(Expr):
    def __init__(self, name: str, token: Token):
        self.name = name
        self.token = token


class BinaryExpr(Expr):
    def __init__(self, op: str, left: Expr, right: Expr, token: Token):
        self.op = op
        self.left = left
        self.right = right
        self.token = token


class UnaryExpr(Expr):
    def __init__(self, op: str, right: Expr, token: Token):
        self.op = op
        self.right = right
        self.token = token


class Assignment(Expr):
    def __init__(self, target: Expr, value: Expr, token: Token):
        self.target = target
        self.value = value
        self.token = token


class FuncCall(Expr):
    def __init__(self, name: str, args: List[Expr], token: Token):
        self.name = name
        self.args = args
        self.token = token


class PropertyAccess(Expr):
    def __init__(self, obj: Expr, prop: str, token: Token):
        self.obj = obj
        self.prop = prop
        self.token = token


class MethodCall(Expr):
    def __init__(self, obj: Expr, method: str, args: List[Expr], token: Token):
        self.obj = obj
        self.method = method
        self.args = args
        self.token = token


class ArrayAccess(Expr):
    def __init__(self, obj: Expr, index: Expr, token: Token):
        self.obj = obj
        self.index = index
        self.token = token


class ArrayLiteral(Expr):
    def __init__(self, elements: List[Expr], token: Token):
        self.elements = elements
        self.token = token


class ObjectLiteral(Expr):
    def __init__(self, props: List[Tuple[str, Expr]], token: Token):
        self.props = props
        self.token = token


class VarDecl(Stmt):
    def __init__(self, name: str, init: Expr, token: Token):
        self.name = name
        self.init = init
        self.token = token


class FuncDecl(Stmt):
    def __init__(self, name: str, params: List[str], body: List[Stmt], token: Token):
        self.name = name
        self.params = params
        self.body = body
        self.token = token


class ReturnStmt(Stmt):
    def __init__(self, expr: Optional[Expr], token: Token):
        self.expr = expr
        self.token = token


class CharDecl(Stmt):
    def __init__(self, name: str, display: Expr, token: Token):
        self.name = name
        self.display = display
        self.token = token


class SceneDecl(Stmt):
    def __init__(self, name: str, body: List[Stmt], token: Token):
        self.name = name
        self.body = body
        self.token = token


class SayStmt(Stmt):
    def __init__(self, text: str, who: Optional[Identifier], token: Token):
        self.text = text
        self.who = who
        self.token = token


class NarrateStmt(Stmt):
    def __init__(self, text: str, token: Token):
        self.text = text
        self.token = token


class ThinkStmt(Stmt):
    def __init__(self, text: str, character: Identifier, token: Token):
        self.text = text
        self.character = character
        self.token = token


class OptionChoice:
    def __init__(self, text: Expr, condition: Optional[Expr], body: List[Stmt]):
        self.text = text
        self.condition = condition
        self.body = body


class OptionsBlock(Stmt):
    def __init__(self, choices: List[OptionChoice], token: Token):
        self.choices = choices
        self.token = token


class IfStmt(Stmt):
    def __init__(
        self,
        cond: Expr,
        then_body: List[Stmt],
        else_body: Optional[List[Stmt]],
        token: Token,
    ):
        self.cond = cond
        self.then_body = then_body
        self.else_body = else_body
        self.token = token


class GotoStmt(Stmt):
    def __init__(self, scene: str, token: Token):
        self.scene = scene
        self.token = token


class PauseStmt(Stmt):
    def __init__(self, duration: Optional[Expr], token: Token):
        self.duration = duration   # None means default (e.g., 2 seconds)
        self.token = token


class ExitStmt(Stmt):
    def __init__(self, token: Token):
        self.token = token


class UsingStmt(Stmt):
    def __init__(self, name: str, token: Token):
        self.name = name
        self.token = token


class ExpressionStmt(Stmt):
    def __init__(self, expr: Expr, token: Token):
        self.expr = expr
        self.token = token


# ----------------------------------------------------------------------
# Parser (recursive descent)
# ----------------------------------------------------------------------


class ParselParser:
    def __init__(self, source: str, filename: str):
        self.tokens = tokenize(source, filename)
        self.pos = 0
        self.filename = filename

    @classmethod
    def parse_expr_from_string(cls, source: str, filename: str = "<string>"):
        """Tokenize and parse a single expression from source."""
        parser = cls(source, filename)
        expr = parser.parse_expr()
        # Ensure there are no leftover tokens
        if parser.current() is not None:
            raise ParselError(parser.current(),
                              "Unexpected extra tokens after expression")
        return expr

    def current(self) -> Optional[Token]:
        return self.tokens[self.pos] if self.pos < len(self.tokens) else None

    def consume(self) -> Token:
        tok = self.current()
        if tok is None:
            raise ParselError(None, "Unexpected end of input")
        self.pos += 1
        return tok

    def expect(self, typ: str, value: Optional[str] = None) -> Token:
        tok = self.current()
        if not tok:
            raise ParselError(None, f"Expected {typ} but got EOF")
        if tok.type != typ:
            raise ParselError(tok, f"Expected token type {
                              typ}, got {tok.type}")
        if value is not None and tok.value != value:
            raise ParselError(tok, f"Expected '{value}', got {tok.value}")
        return self.consume()

    def parse(self) -> List[Stmt]:
        stmts = []
        while self.current():
            stmts.append(self.parse_stmt())
        return stmts

    def parse_stmt(self) -> Stmt:
        tok = self.current()
        if tok is None:
            raise ParselError(None, "Unexpected EOF")
        if tok.type == "keyword":
            kw = tok.value
            if kw == "char":
                return self.parse_char()
            if kw == "scene":
                return self.parse_scene()
            if kw == "say":
                return self.parse_say()
            if kw == "narrate":
                return self.parse_narrate()
            if kw == "think":
                return self.parse_think()
            if kw == "options":
                return self.parse_options()
            if kw == "goto":
                return self.parse_goto()
            if kw == "if":
                return self.parse_if()
            if kw == "pause":
                pause_token = self.consume()
                duration_expr = None
                # Check if the next token is a number or an expression
                if self.current() and self.current().type in ("number", "identifier", "("):
                    duration_expr = self.parse_expr()
                return PauseStmt(duration_expr, pause_token)
            if kw == "exit":
                self.consume()
                return ExitStmt(tok)
            if kw == "var":
                return self.parse_var()
            if kw == "func":
                return self.parse_func()
            if kw == "return":
                return self.parse_return()
            if kw == "using":
                return self.parse_using()
        # expression statement
        expr = self.parse_expr()
        return ExpressionStmt(expr, self.current() or tok)

    def parse_char(self) -> CharDecl:
        tok = self.consume()  # 'char'
        name = self.expect("identifier").value
        display = self.parse_expr()
        return CharDecl(name, display, tok)

    def parse_scene(self) -> SceneDecl:
        tok = self.consume()  # 'scene'
        name = self.expect("string").value
        body = self.parse_block_until("end")
        self.expect("keyword", "end")
        return SceneDecl(name, body, tok)

    def parse_say(self) -> SayStmt:
        tok = self.consume()  # 'say'
        text = self.expect("string").value
        who = None
        if self.current() and self.current().value == "as":
            self.consume()  # 'as'
            who_tok = self.expect("identifier")
            who = Identifier(who_tok.value, who_tok)
        return SayStmt(text, who, tok)

    def parse_narrate(self) -> NarrateStmt:
        tok = self.consume()  # 'narrate'
        text = self.expect("string").value
        return NarrateStmt(text, tok)

    def parse_think(self) -> ThinkStmt:
        tok = self.consume()  # 'think'
        text = self.expect("string").value
        self.expect("keyword", "as")
        char_tok = self.expect("identifier")
        return ThinkStmt(text, Identifier(char_tok.value, char_tok), tok)

    def parse_options(self) -> OptionsBlock:
        tok = self.consume()  # 'options'
        choices = []
        while self.current() and self.current().value != "end":
            text_expr = self.parse_expr()
            condition = None
            if self.current() and self.current().value == "if":
                self.consume()  # 'if'
                condition = self.parse_expr()
            self.expect("keyword", "begin")
            body = self.parse_block_until("end")
            self.expect("keyword", "end")
            choices.append(OptionChoice(text_expr, condition, body))
        self.expect("keyword", "end")
        return OptionsBlock(choices, tok)

    def parse_goto(self) -> GotoStmt:
        tok = self.consume()  # 'goto'
        scene = self.expect("string").value
        return GotoStmt(scene, tok)

    def parse_if(self) -> IfStmt:
        tok = self.consume()  # 'if'
        cond = self.parse_expr()
        then_body = self.parse_block_until(["else", "end"])
        else_body = None
        if self.current() and self.current().value == "else":
            self.consume()  # 'else'
            else_body = self.parse_block_until("end")
        self.expect("keyword", "end")
        return IfStmt(cond, then_body, else_body, tok)

    def parse_var(self) -> VarDecl:
        tok = self.consume()  # 'var'
        name = self.expect("identifier").value
        self.expect("operator", "=")
        init = self.parse_expr()
        return VarDecl(name, init, tok)

    def parse_func(self) -> FuncDecl:
        tok = self.consume()  # 'func'
        name = self.expect("identifier").value
        self.expect("operator", "(")
        params = []
        if self.current() and self.current().value != ")":
            while True:
                param = self.expect("identifier").value
                params.append(param)
                if self.current() and self.current().value == ",":
                    self.consume()
                else:
                    break
        self.expect("operator", ")")
        body = self.parse_block_until("end")
        self.expect("keyword", "end")
        return FuncDecl(name, params, body, tok)

    def parse_return(self) -> ReturnStmt:
        tok = self.consume()  # 'return'
        expr = None
        if self.current() and not (
            self.current().type == "keyword"
            and self.current().value in ("end", "else", "elseif")
        ):
            expr = self.parse_expr()
        return ReturnStmt(expr, tok)

    def parse_using(self) -> UsingStmt:
        tok = self.consume()  # 'using'
        name = self.expect("identifier").value
        return UsingStmt(name, tok)

    def parse_block_until(self, terminators: Union[str, List[str]]) -> List[Stmt]:
        if isinstance(terminators, str):
            terminators = [terminators]
        stmts = []
        while self.current() and self.current().value not in terminators:
            stmt = self.parse_stmt()
            stmts.append(stmt)
        return stmts

    # Expression parsing (recursive descent, same as before)
    def parse_expr(self) -> Expr:
        return self.parse_assignment()

    def parse_assignment(self) -> Expr:
        left = self.parse_logical_or()
        if (
            self.current()
            and self.current().type == "operator"
            and self.current().value in ("=", "+=", "-=", "*=", "/=", "%=")
        ):
            op_tok = self.consume()
            right = self.parse_assignment()
            if not isinstance(left, (Identifier, PropertyAccess, ArrayAccess)):
                raise ParselError(
                    left.token, f"Invalid assignment target: {
                        type(left).__name__}"
                )
            return Assignment(left, right, op_tok)
        return left

    def parse_logical_or(self) -> Expr:
        left = self.parse_logical_and()
        while self.current() and self.current().value == "||":
            op_tok = self.consume()
            right = self.parse_logical_and()
            left = BinaryExpr("||", left, right, op_tok)
        return left

    def parse_logical_and(self) -> Expr:
        left = self.parse_equality()
        while self.current() and self.current().value == "&&":
            op_tok = self.consume()
            right = self.parse_equality()
            left = BinaryExpr("&&", left, right, op_tok)
        return left

    def parse_equality(self) -> Expr:
        left = self.parse_comparison()
        while self.current() and self.current().value in ("==", "!="):
            op_tok = self.consume()
            right = self.parse_comparison()
            left = BinaryExpr(op_tok.value, left, right, op_tok)
        return left

    def parse_comparison(self) -> Expr:
        left = self.parse_term()
        while self.current() and self.current().value in ("<", "<=", ">", ">="):
            op_tok = self.consume()
            right = self.parse_term()
            left = BinaryExpr(op_tok.value, left, right, op_tok)
        return left

    def parse_term(self) -> Expr:
        left = self.parse_factor()
        while self.current() and self.current().value in ("+", "-"):
            op_tok = self.consume()
            right = self.parse_factor()
            left = BinaryExpr(op_tok.value, left, right, op_tok)
        return left

    def parse_factor(self) -> Expr:
        left = self.parse_unary()
        while self.current() and self.current().value in ("*", "/", "%"):
            op_tok = self.consume()
            right = self.parse_unary()
            left = BinaryExpr(op_tok.value, left, right, op_tok)
        return left

    def parse_unary(self) -> Expr:
        if (
            self.current()
            and self.current().type == "operator"
            and self.current().value in ("-", "!")
        ):
            op_tok = self.consume()
            right = self.parse_unary()
            return UnaryExpr(op_tok.value, right, op_tok)
        return self.parse_call_member()

    def parse_call_member(self) -> Expr:
        expr = self.parse_primary()
        while True:
            tok = self.current()
            if not tok:
                break
            if tok.value == ".":
                self.consume()
                prop_tok = self.expect("identifier")
                prop = prop_tok.value
                if self.current() and self.current().value == "(":
                    self.consume()
                    args = self.parse_arguments()
                    self.expect("operator", ")")
                    expr = MethodCall(expr, prop, args, prop_tok)
                else:
                    expr = PropertyAccess(expr, prop, prop_tok)
            elif tok.value == "[":
                self.consume()
                index = self.parse_expr()
                self.expect("operator", "]")
                expr = ArrayAccess(expr, index, tok)
            elif tok.value == "(" and isinstance(expr, Identifier):
                self.consume()
                args = self.parse_arguments()
                self.expect("operator", ")")
                expr = FuncCall(expr.name, args, expr.token)
            else:
                break
        return expr

    def parse_arguments(self) -> List[Expr]:
        args = []
        if self.current() and self.current().value != ")":
            while True:
                args.append(self.parse_expr())
                if self.current() and self.current().value == ",":
                    self.consume()
                else:
                    break
        return args

    def parse_primary(self) -> Expr:
        tok = self.current()
        if tok is None:
            raise ParselError(None, "Unexpected EOF")
        if tok.type == "number":
            self.consume()
            return Literal(tok.value, tok)
        if tok.type == "string":
            self.consume()
            return Literal(tok.value, tok)
        if tok.type == "boolean":
            self.consume()
            return Literal(tok.value, tok)
        if tok.type == "identifier":
            self.consume()
            return Identifier(tok.value, tok)
        if tok.value == "[":
            self.consume()
            elems = []
            if self.current() and self.current().value != "]":
                while True:
                    elems.append(self.parse_expr())
                    if self.current() and self.current().value == ",":
                        self.consume()
                    else:
                        break
            self.expect("operator", "]")
            return ArrayLiteral(elems, tok)
        if tok.value == "{":
            self.consume()
            props = []
            if self.current() and self.current().value != "}":
                while True:
                    key_tok = self.current()
                    if key_tok.type not in ("identifier", "string"):
                        raise ParselError(
                            key_tok, "Expected identifier or string as object key"
                        )
                    key = key_tok.value
                    self.consume()
                    self.expect("operator", ":")
                    val = self.parse_expr()
                    props.append((key, val))
                    if self.current() and self.current().value == ",":
                        self.consume()
                    else:
                        break
            self.expect("operator", "}")
            return ObjectLiteral(props, tok)
        if tok.value == "(":
            self.consume()
            expr = self.parse_expr()
            self.expect("operator", ")")
            return expr
        raise ParselError(tok, f"Unexpected token in expression: {tok.value}")


# ----------------------------------------------------------------------
# Runtime
# ----------------------------------------------------------------------


class ReturnSignal(Exception):
    def __init__(self, value: Any = None):
        self.value = value


class GotoSignal(Exception):
    def __init__(self, scene: str):
        self.scene = scene


class ExitSignal(Exception):
    pass


class Environment:
    def __init__(self, parent: Optional["Environment"] = None):
        self.vars: Dict[str, Any] = {}
        self.parent = parent

    def define(self, name: str, value: Any):
        self.vars[name] = value

    def get(self, name: str, token: Token) -> Any:
        if name in self.vars:
            return self.vars[name]
        if self.parent:
            return self.parent.get(name, token)
        raise ParselError(token, f"Undefined variable '{name}'")

    def set(self, name: str, value: Any, token: Token):
        if name in self.vars:
            self.vars[name] = value
        elif self.parent:
            self.parent.set(name, value, token)
        else:
            raise ParselError(token, f"Undefined variable '{name}'")


class ParselRuntime:
    def __init__(self, ast: List[Stmt], filename: str = "<main>"):
        self.globals = Environment()
        self.scenes: Dict[str, List[Stmt]] = {}
        self.current_scene: Optional[str] = None
        self.scene_stack: List[str] = []
        self.ast = ast
        self.filename = filename

        # collect scenes
        for stmt in ast:
            if isinstance(stmt, SceneDecl):
                if stmt.name in self.scenes:
                    raise ParselError(
                        stmt, "This scene has already been declared")
                self.scenes[stmt.name] = stmt.body

        # built‑ins
        self.globals.define("print", print)
        self.globals.define("input", input)
        self.globals.define("includes", lambda x, y: y in x)
        self.globals.define("nil", None)
        self.globals.define("fmtStr", lambda s, t: s.format(**t))

        # default handler for os: imports
        def os_import_handler(module_name: str) -> Any:
            try:
                return importlib.import_module(
                    module_name)
            except ImportError as e:
                raise ParselError(
                    None, f"Could not import os module '{module_name}': {e}"
                )

        self.globals.define("import", os_import_handler)

    def run(self):
        # execute top‑level statements (non‑scene)
        for stmt in self.ast:
            if not isinstance(stmt, SceneDecl):
                self.execute_stmt(stmt, self.globals)
        # start game
        self.current_scene = "start"
        if self.current_scene not in self.scenes:
            raise ParselError(None, "No scene named 'start' found")
        self._game_loop()

    def _game_loop(self):
        while True:
            try:
                self._run_scene(self.current_scene)
            except GotoSignal as g:
                self.scene_stack.append(self.current_scene)
                self.current_scene = g.scene
            except ReturnSignal:
                if self.scene_stack:
                    self.current_scene = self.scene_stack.pop()
                else:
                    break
            except ExitSignal:
                break

    def _run_scene(self, name: str):
        # overwrite this part of the engine, if you want to add a gui
        env = Environment(self.globals)
        i = 0
        while i < len(self.scenes[name]):
            stmt = self.scenes[name][i]
            self.execute_stmt(stmt, env)
            i += 1

    def execute_stmt(self, stmt: Stmt, env: Environment):
        if isinstance(stmt, VarDecl):
            val = self.evaluate_expr(stmt.init, env)
            env.define(stmt.name, val)
        elif isinstance(stmt, FuncDecl):
            def func_wrapper(*args):
                func_env = Environment(env)
                for i, param in enumerate(stmt.params):
                    if i < len(args):
                        arg_val = args[i]
                    else:
                        raise ParselError(
                            stmt.token, f"Missing argument for parameter '{
                                param}'"
                        )
                    func_env.define(param, arg_val)
                try:
                    for s in stmt.body:
                        self.execute_stmt(s, func_env)
                except ReturnSignal as ret:
                    return ret.value
                return None

            env.define(stmt.name, func_wrapper)
        elif isinstance(stmt, ReturnStmt):
            val = self.evaluate_expr(stmt.expr, env) if stmt.expr else None
            raise ReturnSignal(val)
        elif isinstance(stmt, CharDecl):
            display = self.evaluate_expr(stmt.display, env)
            env.define(stmt.name, display)
        elif isinstance(stmt, SayStmt):
            who = ""
            if stmt.who:
                who_val = self.evaluate_expr(stmt.who, env)
                who = f"{who_val}: "
            text = self.interpolate(stmt.text, env)
            print(f'{who}"{text}"')
        elif isinstance(stmt, NarrateStmt):
            print(self.interpolate(stmt.text, env))
        elif isinstance(stmt, ThinkStmt):
            who = self.evaluate_expr(stmt.character, env)
            text = self.interpolate(stmt.text, env)
            print(f"{who}: *{text}*")
        elif isinstance(stmt, OptionsBlock):
            available = []
            for ch in stmt.choices:
                if ch.condition and not self.evaluate_expr(ch.condition, env):
                    continue
                text = str(self.evaluate_expr(ch.text, env))
                available.append((text, ch.body))
            if not available:
                print("No options available.")
                return
            print("Choose an option:")
            for i, (txt, _) in enumerate(available, 1):
                print(f"{i}) {txt}")
            while True:
                try:
                    raw = input("Choice: ")
                    idx = int(raw) - 1
                    if 0 <= idx < len(available):
                        _, body = available[idx]
                        for sub in body:
                            self.execute_stmt(sub, env)
                        break
                    else:
                        print(f"Invalid choice: {raw}")
                except ValueError:
                    print(f"Invalid choice: {raw}")
                except EOFError:
                    sys.exit(0)
        elif isinstance(stmt, IfStmt):
            if self.evaluate_expr(stmt.cond, env):
                for sub in stmt.then_body:
                    self.execute_stmt(sub, env)
            elif stmt.else_body:
                for sub in stmt.else_body:
                    self.execute_stmt(sub, env)
        elif isinstance(stmt, GotoStmt):
            if stmt.scene not in self.scenes:
                raise ParselError(stmt.token, f"Scene '{
                                  stmt.scene}' not defined")
            raise GotoSignal(stmt.scene)
        elif isinstance(stmt, PauseStmt):
            import time
            if stmt.duration is None:
                time.sleep(2)      # default 2 seconds
            else:
                dur = self.evaluate_expr(stmt.duration, env)
                if not isinstance(dur, (int, float)):
                    raise ParselError(stmt.token, f"Pause duration must be a number, got {
                                      type(dur).__name__}")
                if dur < 0:
                    raise ParselError(
                        stmt.token, "Pause duration cannot be negative")
                time.sleep(dur)
        elif isinstance(stmt, ExitStmt):
            raise ExitSignal()
        elif isinstance(stmt, UsingStmt):
            # bring a namespace's members into current env
            namespace = env.get(stmt.name, stmt.token)
            if isinstance(namespace, dict):
                for k, v in namespace.items():
                    env.define(k, v)
            elif isinstance(namespace, Environment):
                for name, var in namespace.vars.items():
                    env.define(name, var)
            else:
                raise ParselError(
                    stmt.token, f"Cannot 'use' non-namespace value: {
                        stmt.name}"
                )

        elif isinstance(stmt, ExpressionStmt):
            self.evaluate_expr(stmt.expr, env)
        else:
            raise ParselError(
                getattr(stmt, "token", None),
                f"Unknown statement: {type(stmt).__name__}",
            )

    def evaluate_expr(self, expr: Expr, env: Environment) -> Any:
        if isinstance(expr, Literal):
            return expr.value
        if isinstance(expr, Identifier):
            return env.get(expr.name, expr.token)
        if isinstance(expr, BinaryExpr):
            op = expr.op
            if op == "&&":
                return self.evaluate_expr(expr.left, env) and self.evaluate_expr(expr.right, env)
            if op == "||":
                return self.evaluate_expr(expr.left, env) or self.evaluate_expr(expr.right, env)
            left = self.evaluate_expr(expr.left, env)
            right = self.evaluate_expr(expr.right, env)
            if op == "+":
                return left + right
            if op == "-":
                return left - right
            if op == "*":
                return left * right
            if op == "/":
                if right == 0:
                    raise ParselError(expr.token, "Division by zero")
                return left / right
            if op == "%":
                return left % right
            if op == "==":
                return left == right
            if op == "!=":
                return left != right
            if op == "<":
                return left < right
            if op == "<=":
                return left <= right
            if op == ">":
                return left > right
            if op == ">=":
                return left >= right
            raise ParselError(expr.token, f"Unknown binary operator: {op}")
        if isinstance(expr, UnaryExpr):
            val = self.evaluate_expr(expr.right, env)
            if expr.op == "-":
                return -val
            if expr.op == "!":
                return not val
            raise ParselError(expr.token, f"Unknown unary operator: {expr.op}")
        if isinstance(expr, Assignment):
            val = self.evaluate_expr(expr.value, env)
            target = expr.target
            if isinstance(target, Identifier):
                env.set(target.name, val, target.token)
            elif isinstance(target, PropertyAccess):
                obj = self.evaluate_expr(target.obj, env)
                if isinstance(obj, dict):
                    obj[target.prop] = val
                else:
                    setattr(obj, target.prop, val)
            elif isinstance(target, ArrayAccess):
                obj = self.evaluate_expr(target.obj, env)
                idx = self.evaluate_expr(target.index, env)
                obj[idx] = val
            else:
                raise ParselError(
                    expr.token, f"Invalid assignment target: {
                        type(target).__name__}"
                )
            return val
        if isinstance(expr, FuncCall):
            func = env.get(expr.name, expr.token)
            if not callable(func):
                raise ParselError(
                    expr.token, f"'{expr.name}' is not a function")
            args = [self.evaluate_expr(a, env) for a in expr.args]
            return func(*args)
        if isinstance(expr, MethodCall):
            obj = self.evaluate_expr(expr.obj, env)
            method = getattr(obj, expr.method, None)
            if method is None and isinstance(obj, dict):
                method = obj.get(expr.method)
            if not callable(method):
                raise ParselError(
                    expr.token, f"Method '{
                        expr.method}' not found or not callable"
                )
            args = [self.evaluate_expr(a, env) for a in expr.args]
            return method(*args)
        if isinstance(expr, PropertyAccess):
            obj = self.evaluate_expr(expr.obj, env)
            if isinstance(obj, dict):
                if expr.prop not in obj:
                    raise ParselError(expr.token, f"Property '{
                                      expr.prop}' not found")
                return obj[expr.prop]
            return getattr(obj, expr.prop)
        if isinstance(expr, ArrayAccess):
            obj = self.evaluate_expr(expr.obj, env)
            idx = self.evaluate_expr(expr.index, env)
            return obj[idx]
        if isinstance(expr, ArrayLiteral):
            return [self.evaluate_expr(e, env) for e in expr.elements]
        if isinstance(expr, ObjectLiteral):
            obj = {}
            for key, val_expr in expr.props:
                obj[key] = self.evaluate_expr(val_expr, env)
            return obj
        raise ParselError(
            getattr(expr, "token", None), f"Unknown expression: {
                type(expr).__name__}"
        )

    def interpolate(self, text: str, env: Environment) -> str:
        def repl(match):
            expr_str = match.group(1).strip()
            expr = ParselParser.parse_expr_from_string(
                expr_str, "<interpolate>")
            val = self.evaluate_expr(expr, env)
            return str(val)
        return re.sub(r"\{\{(.+?)\}\}", repl, text)


def main():
    if len(sys.argv) not in (1, 2):
        print("Usage: python parsel.py [game_directory]")
        sys.exit(1)

    root_dir = sys.argv[1] if len(sys.argv) == 2 else os.getcwd()

    # 1. Collect all .par files, sorted alphabetically
    par_files = []
    for dirpath, dirs, filenames in os.walk(root_dir):
        # Remove hidden directories from the search
        dirs[:] = [d for d in dirs if not d.startswith('.')]
        for fname in filenames:
            if fname.endswith(".par"):
                par_files.append(os.path.join(dirpath, fname))

    if not par_files:
        print(f"No .par files found in {root_dir}.")
        sys.exit(1)
    par_files.sort()
    # 2. Parse each file and combine ASTs
    all_stmts = []
    for fpath in par_files:
        with open(fpath, "r", encoding="utf-8") as f:
            source = f.read()
        parser = ParselParser(source, fpath)
        ast = parser.parse()
        all_stmts.extend(ast)

    # 3. Run the game with the combined AST
    runtime = ParselRuntime(all_stmts, root_dir)
    runtime.run()


if __name__ == "__main__":
    main()
