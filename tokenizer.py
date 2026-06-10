from nodes import *

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
                potential_single_ops = "=+-*/%^<>!.|"
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
            if char in ['"', "'"]:
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

                        if esc not in escapes:
                            raise NovaError(
                                Token(
                                    "error", "Invalid escape sequence", file, line, col
                                ),
                                f"Invalid escape sequence \\{esc}",
                            )

                        value.append(escapes[esc])

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

                tokens.append(Token("string", "".join(value), file, line, start_col))
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
                        annotation_type = BUILTIN_VAR_TYPES_INFER.get(inferred_type, None)
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
            # print(member_token)
            while True:
                member_token = self.get_next_token()

                if member_token.type == "keyword" and member_token.value == "static":
                    self.consume_token()  # consume 'static'
                    is_static = True

                elif member_token.type == "keyword" and member_token.value == "private":
                    if is_static:
                        raise NovaError(member_token, "Cannot have a private static method/variable")
                    self.consume_token()  # consume 'private'
                    is_private = True
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
                    type_annotation = type_token.value
                    self.consume_token()

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
                        prop_name_token.file,
                        prop_name_token.line,
                        prop_name_token.column,
                    )
                )
            elif (
                self.get_next_token()
                and self.get_next_token().type == "keyword"
                and self.get_next_token().value == "func"
            ):
                # Method Definition
                self.consume_token()  # consume 'func'
                method_name_token = self.expect_type("identifier")
                method_name = method_name_token.value
                self.consume_token()

                is_constructor = False
                if method_name == "init":
                    is_constructor = True

                self.expect_token("(")
                self.consume_token()

                parameters = []
                if self.get_next_token() and self.get_next_token().value != ")":
                    while True:
                        is_compact = False
                        if (
                            self.get_next_token()
                            and self.get_next_token().value == "compact"
                        ):
                            self.consume_token()  # consume 'compact'
                            is_compact = True

                        param_token = self.expect_type("identifier")
                        param_name = param_token.value
                        self.consume_token()

                        annotation_type = None
                        if (
                            self.get_next_token()
                            and self.get_next_token().type == "identifier"
                        ):
                            type_token = self.get_next_token()
                            if (
                                type_token.value in BUILTIN_VAR_TYPES
                                or type_token.value in CUSTOM_TYPES
                            ):
                                annotation_type = type_token.value
                                self.consume_token()
                            else:
                                raise NovaError(
                                    type_token,
                                    f"Invalid type annotation: {type_token.value}",
                                )

                        default_expr = None
                        if self.get_next_token() and self.get_next_token().value == "=":
                            self.consume_token()
                            default_expr = self.parse_expression()

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

                body_statements = self.parse_block_until(["end"])
                self.expect_token("end")
                self.consume_token()

                body.append(
                    MethodDefinition(
                        method_name,
                        parameters,
                        body_statements,
                        is_static,
                        is_constructor,
                        is_private,
                        method_name_token.file,
                        method_name_token.line,
                        method_name_token.column,
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
                        raise NovaError(self.consume_token(), "Cannot use 'default' when strict is enabled")
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
                # Check if the next token is not a keyword (indicating end of statement or start of expression)
                if self.get_next_token() and self.get_next_token().value:
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
            and self.get_next_token().value in ["<", "<=", ">", ">=", "<<", ">>", "|", "between"]
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
            and self.get_next_token().value in ["*", "/", "%", "^"]
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
                    args = []
                    if self.get_next_token() and self.get_next_token().value != ")":
                        while True:
                            if self.get_next_token().value == "unpack":
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
                            if (
                                self.get_next_token()
                                and self.get_next_token().value == ","
                            ):
                                self.consume_token()
                            else:
                                break
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
                args = []
                if self.get_next_token() and self.get_next_token().value != ")":
                    while True:
                        if self.get_next_token().value == "unpack":
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
                        annotation_type = BUILTIN_VAR_TYPES_INFER.get(inferred_type, None)
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
                args = []
                if self.get_next_token() and self.get_next_token().value != ")":
                    while True:
                        if self.get_next_token().value == "unpack":
                            unpack_token = self.consume_token()
                            args.append(
                                ExplodeExpr(
                                    self.parse_expression(),
                                    unpack_token.file,
                                    unpack_token.line,
                                    unpack_token.column,
                                )
                            )
                        else:
                            args.append(self.parse_expression())
                        if self.get_next_token() and self.get_next_token().value == ",":
                            self.consume_token()
                        else:
                            break
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
