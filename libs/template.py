#!/usr/bin/env python
"""Simple template engine with @header, {expr} and !{stmt} support.

Paths (includes and @doc output) are always resolved relative to the
input file (or the current template when nested).
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, Optional, Union
import re


def is_numeric(val: Any) -> bool:
    """Return True if the full string matches an integer or float."""
    return bool(re.match(r"^[+-]?\d+(?:\.\d+)?$", str(val)))


# Example usage:
#
# @doc: sample.html
# @title: sample format testing
# <?
# this is like Latex, basically
# also, this is a comment, hello :)
# ?>
# !{import numpy as np}
# !{mat = np.array([[52,12],
#                  [24,41]])
# }
# This is a simple test
# <ul>
# !{
# for i in range(20):
#     write(f"<li key='{i}'>{mat * i}</li>")
# }
# </ul>


@dataclass
class Ctx:  # pylint: disable=invalid-name
    """Parser context that walks the template source and accumulates output."""

    source: str
    index: int
    # typing / shape information – what actually exists where
    doc = {
        "header": {"name": "", "title": ""},
        "content": "",
        "fileDir": "",
        "state": {},  # where eval can just dump stuff into
    }

    def write(self, s: str) -> None:
        """Append a string to the generated content."""
        self.doc["content"] += s

    def __post_init__(self) -> None:
        self.doc = {
            "header": {"name": "", "title": ""},
            "content": "",
            "state": {},
            "fileDir": Path("."),
        }
        self.doc["state"]["write"] = self.write
        # load and genHtml are installed by load() so they see the correct fileDir

    def moveIndex(self, offset: int) -> None:  # pylint: disable=invalid-name
        """Advance (or rewind) the current parse position."""
        self.index += offset

    def consumeNextChar(self) -> str:  # pylint: disable=invalid-name
        """Return the next character, honouring backslash escapes."""
        char = self.source[self.index]
        if char == "\\":
            v = self.nextChar(1)
            self.moveIndex(2)
            return v
        self.moveIndex(1)
        return char

    def nextChar(self, i: int = 0) -> str:  # pylint: disable=invalid-name
        """Peek at the character i positions ahead without consuming it."""
        return self.source[self.index + i]

    def skipSpace(self) -> None:  # pylint: disable=invalid-name
        """Skip over consecutive whitespace characters."""
        while self.isWhiteSpace():
            self.moveIndex(1)

    def isWhiteSpace(self) -> bool:  # pylint: disable=invalid-name
        """True if the current character is whitespace."""
        return self.source[self.index].isspace()

    def isEnd(self) -> bool:  # pylint: disable=invalid-name
        """True if the parse index is at or past the end of the source."""
        return self.index >= len(self.source)

    def isNum(self) -> bool:  # pylint: disable=invalid-name
        """True if the current character is a digit."""
        return self.source[self.index].isdigit()

    def isAlpha(self) -> bool:  # pylint: disable=invalid-name
        """True if the current character is alphabetic."""
        return self.source[self.index].isalpha()

    def isChar(self, c: str) -> bool:  # pylint: disable=invalid-name
        """True if the current character equals c."""
        return self.source[self.index] == c

    def getString(self) -> str:  # pylint: disable=invalid-name
        """Parse a double-quoted string and return its content."""
        self.moveIndex(1)
        output = ""
        while not self.isChar('"'):
            output += self.source[self.index]
            self.moveIndex(1)
        self.moveIndex(1)
        return output


def get_braced_content(context: Ctx, skip: int = 1) -> str:
    """Extract the content of a balanced {…} or !{…} block.

    Shared by both expression and statement handlers to avoid duplication.
    """
    brace_count = 1
    context.moveIndex(skip)
    string = ""
    while brace_count > 0:
        if context.isChar("{"):
            brace_count += 1
        elif context.isChar("}"):
            brace_count -= 1
        if brace_count > 0:
            string += context.source[context.index]
        context.moveIndex(1)
    return string


def handle_at(context: Ctx) -> None:
    """Process an @name: value header (or @include)."""
    context.moveIndex(1)
    name = ""
    output = ""
    while not context.isChar(":"):
        name += context.source[context.index]
        context.moveIndex(1)
    context.moveIndex(1)  # skip ":"
    context.skipSpace()
    while not context.isChar("\n"):
        output += context.source[context.index]
        context.moveIndex(1)
    output = output.strip()

    if name == "include":
        # already relative to the *current* template’s directory
        out = load(context.doc["fileDir"] / output, context.doc["state"])
        context.doc["state"].update(out["state"])
        context.write(out["content"])
        return

    if is_numeric(output):
        try:
            output = int(output)
        except ValueError:
            output = float(output)
    context.doc["header"][name] = output


def handle_text(context: Ctx) -> None:
    """Copy plain text until a special construct or newline is found."""
    output = ""
    while (
        not context.isEnd()
        and not context.isChar("\n")
        and not (context.isChar("!") and context.nextChar(1) == "{")
        and not context.isChar("{")
    ):
        e = context.consumeNextChar()
        output += e
    context.write(output)


def handle_embedded_stmt(context: Ctx) -> None:
    """Evaluate a {…} expression and write its result."""
    string = get_braced_content(context, skip=1)
    c = eval(string, context.doc["state"])  # pylint: disable=eval-used
    if c is not None:
        context.write(str(c))


def handle_embedded_expr(context: Ctx) -> None:
    """Execute a !{…} statement (supports nested braces)."""
    string = get_braced_content(context, skip=2)
    exec(string, context.doc["state"])  # pylint: disable=exec-used


# You can use this for simple profiles – no @doc / @title required:
#
# <div>
#     <h1>name: {name}</h1>
#     <h2>age: {age}</h2>
# </div>


def load(
    filePath: Union[str, Path],  # pylint: disable=invalid-name
    state: Optional[Dict[str, Any]] = None,
) -> Dict[str, Any]:
    """Load and render a template file.

    All relative paths (includes and the final @doc output) are resolved
    against the directory of the input file (or the current template when
    nested).
    """
    filePath = Path(filePath)
    with open(filePath, "r", encoding="utf-8") as fh:
        content = fh.read()
        if not content.endswith("\n"):
            content += "\n"

    if state is None:
        state = {}

    context = Ctx(
        re.sub(r"<\?(.*?)\?>", "", content, flags=re.DOTALL).strip(),
        0,
    )
    context.doc["fileDir"] = filePath.parent
    context.doc["state"].update(state)
    context.doc["state"]["fileDir"] = context.doc["fileDir"]
    context.doc["state"]["genHtml"] = genHtml

    def relative_load(
        fp: Union[str, Path],
        st: Optional[Dict[str, Any]] = None,
    ) -> Dict[str, Any]:
        p = Path(fp)
        if not p.is_absolute():
            p = context.doc["fileDir"] / p
        return load(p, st)

    context.doc["state"]["load"] = relative_load

    while not context.isEnd():
        if context.isChar("\n"):
            if not context.doc["content"].endswith(
                "\n" * context.doc["header"].get("max-new-lines", 1)
            ):
                context.doc["content"] += "\n"
            context.moveIndex(1)
        elif context.isWhiteSpace():
            context.write(context.nextChar())
            context.moveIndex(1)
        elif context.isChar("@"):
            handle_at(context)
        elif context.isChar("!") and context.nextChar(1) == "{":
            handle_embedded_expr(context)
        elif context.isChar("{"):
            handle_embedded_stmt(context)
        else:
            handle_text(context)

    if not context.doc["content"].endswith("\n"):
        context.doc["content"] += "\n"
    return context.doc


def genHtml(doc: Dict[str, Any]) -> str:  # pylint: disable=invalid-name
    """Wrap rendered content in a minimal HTML document."""
    parts = [
        "<!DOCTYPE html>\n",
        "<html>\n",
        "<head>\n",
        f"<title>{doc['header']['title']}</title>\n",
        "</head>\n",
        "<body>\n",
        doc["content"],
        "</body>\n",
        "</html>\n",
    ]
    return "".join(parts)


if __name__ == "__main__":
    import sys

    if len(sys.argv) != 2:
        print("Usage: python template.py <file>")
        sys.exit(1)

    result = load(sys.argv[1])
    out_name = result["header"].get("doc")
    if not out_name:
        print("Error: template must contain @doc: <filename>")
        sys.exit(1)

    out_path = Path(out_name)
    if not out_path.is_absolute():
        out_path = result["fileDir"] / out_path

    with open(out_path, "w", encoding="utf-8") as fh:
        if result["header"].get("type") == "html":
            fh.write(genHtml(result))
        else:
            fh.write(result["content"])
    print("Wrote", out_path)
