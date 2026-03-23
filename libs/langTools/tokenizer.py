from dataclasses import dataclass


@dataclass
class Token:
    type: str
    value: str
    line: int
    column: int
    file: str


KEYWORDS = set()


class TokenizeError(Exception):
    def __init__(self, message: str, line: int, col: int, line_text: str):
        pointer = " " * (col) + "^"
        full_message = (
            f"Tokenization error at line {line}, column {col}:\n"
            f"{line_text}\n{pointer}\n{message}"
        )
        super().__init__(full_message)


def tokenize(source: str, file: str) -> list[Token]:
    tokens = []
    line = 0
    column = 0
    index = 0
    lines = source.splitlines()

    def addToken(type, value, line, col):
        tokens.append(Token(type, value, line + 1, col + 1, file))

    while index < len(source):
        currentToken = source[index]
        if currentToken in " \t":
            index += 1
            column += 1
            continue

        if currentToken == "\n":
            line += 1
            column = 0
            index += 1
            continue
        # Attributes starting with '@'
        if currentToken == "@":
            start = index
            start_col = column
            index += 1
            column += 1
            while index < len(source) and not (source[index] in " \t\n"):
                index += 1
                column += 1
            addToken("ATTR", source[start:index], line, start_col)
            continue
        if currentToken == "#":
            start = index
            start_col = column

            while index < len(source) and source[index] != "\n":
                index += 1
                column += 1

            addToken("PP_DIRECTIVE", source[start:index], line, column)
            continue
        if currentToken == "/" and index + 1 < len(source):
            if source[index + 1] == "/":
                while index < len(source) and source[index] != "\n":
                    index += 1
                continue
            elif source[index + 1] == "*":
                comment_start_line = line
                comment_start_col = column
                index += 2
                column += 2
                while index + 1 < len(source) and not (
                    source[index] == "*" and source[index + 1] == "/"
                ):
                    if source[index] == "\n":
                        line += 1
                        column = 0
                    else:
                        column += 1
                    index += 1

                if index + 1 < len(source):
                    index += 2
                    column += 2

                else:
                    lineText = (
                        lines[comment_start_line - 1]
                        if comment_start_line - 1 < len(lines)
                        else ""
                    )

                    raise TokenizeError(
                        "unterminated multiline comment",
                        comment_start_line,
                        comment_start_col,
                        lineText,
                    )
                continue
        if source[index : index + 3] == "...":
            addToken("ELLIPSIS", "...", line, column)
            index += 3
            column += 3
            continue
        if currentToken == "'":
            start = index
            start_col = column
            index += 1  # skip the "'" from the start
            column += 1
            if index >= len(source):
                raise TokenizeError(
                    "Unterminated character literal", line, start_col, lines[line - 1]
                )
            if source[index] == "\\" and index + 1 < len(source):
                index += 2
                column += 2

            else:
                index += 1
                column += 1

            if index >= len(source) and source[index] != "'":
                raise TokenizeError(
                    "Unterminated character literal", line, start_col, lines[line - 1]
                )

            index += 1
            column += 1
            addToken("CHAR", source[start:index], line, start_col)
            continue
        if currentToken == '"':
            start = index
            start_col = column

            index += 1
            column += 1

            strVal = ""

            while index < len(source) and source[index] != '"':
                if source[index] == "\\" and index + 1 < len(source):
                    strVal += source[index] + source[index + 1]
                    index += 2
                    column += 2
                else:
                    strVal += source[index]
                    index += 1
                    column += 1
            if index < len(source) and source[index] == '"':
                index += 1
                column += 1
                addToken("STRING", strVal, line, start_col)
            else:
                line_text = lines[line - 1] if line - 1 < len(lines) else ""
                raise TokenizeError(
                    "Unterminated string literal", line, start_col, line_text
                )

            continue
        if currentToken == "-":
            start = index
            start_col = column
            if index + 1 < len(source):
                next_char = source[index + 1]
                if next_char.isdigit():
                    # Negative number
                    index += 1
                    column += 1
                    while index < len(source) and source[index].isdigit():
                        index += 1
                        column += 1
                    if (
                        index < len(source)
                        and source[index] == "."
                        and index + 1 < len(source)
                        and source[index + 1].isdigit()
                    ):
                        index += 1
                        column += 1
                        while index < len(source) and source[index].isdigit():
                            index += 1
                            column += 1
                    addToken("NUMBER", source[start:index], line, start_col)
                    continue
                elif next_char.isalpha() or next_char == "_":
                    # Negative identifier
                    index += 1
                    column += 1
                    while index < len(source) and (
                        source[index].isalnum() or source[index] == "_"
                    ):
                        index += 1
                        column += 1
                    addToken("NEG_ID", source[start:index], line, start_col)
                    continue

        # Number literal (int and float)
        if currentToken.isdigit():
            start = index
            start_col = column
            while index < len(source) and source[index].isdigit():
                index += 1
                column += 1
            if (
                index < len(source)
                and source[index] == "."
                and index + 1 < len(source)
                and source[index + 1].isdigit()
            ):
                index += 1
                column += 1
                while index < len(source) and source[index].isdigit():
                    index += 1
                    column += 1
            addToken("NUMBER", source[start:index], line, start_col)
            continue
        if currentToken.isalpha() or currentToken == "_":
            start = index
            start_col = column
            while index < len(source) and (
                source[index].isalnum() or source[index] == "_"
            ):
                index += 1
                column += 1
            val = source[start:index]
            if val in KEYWORDS:
                addToken(val, val, line, start_col)
            else:
                addToken("ID", val, line, start_col)
            continue

        # Operators and punctuation (including '&', '!', etc.)
        if currentToken in "=!<>+-*/&%|":
            if index + 1 < len(source):
                two_char = source[index : index + 2]
                if two_char in {
                    "==",
                    "!=",
                    ">=",
                    "<=",
                    "+=",
                    "-=",
                    "*=",
                    "/=",
                    "&&",
                    "||",
                }:
                    addToken("OP", two_char, line, column)
                    index += 2
                    column += 2
                    continue
            addToken("OP", currentToken, line, column)
            index += 1
            column += 1
            continue

        # Punctuation including square brackets.
        if currentToken in "().,{}:;[]":
            addToken("OP", currentToken, line, column)
            index += 1
            column += 1
            continue

        lineT = lines[line - 1] if line - 1 < len(lines) else ""
        raise TokenizeError(f"Unknown character '{currentToken}'", line, column, lineT)

    return tokens


if __name__ == "__main__":
    source = """
    prototype foo(...);
    var x int = -42.5;
    // comment
    /* multi
        line */
    """

    tokens = tokenize(source, "<test>")
    print(tokens)
