#!/usr/bin/env python
"""Extensible template engine with @directives, @pre hooks, {expr} and !{stmt}.

Quick syntax reference
----------------------
    @name: value           Header / directive.  If no directive named
                           ``name`` is registered, ``value`` is stored in
                           ``doc["header"]["name"]`` (numeric strings are
                           coerced to int/float).
    @include: path         Splice another template in place.
    @pre: a, b             Run preprocessors ``a`` then ``b`` over the
                           source *before* it is parsed.
    {expr}                 Evaluate ``expr`` and write ``str(result)``.
                           ``None`` writes nothing.
    !{stmt}                Execute ``stmt`` (multi-line, nested braces OK).
    <? ... ?>              Comment - stripped before anything else runs.
    \\@ \\{ \\!            Escape a sigil so it appears literally.

Extending the engine
--------------------
    engine = TemplateEngine()

    @engine.directive("greet")
    def _greet(ctx, value):
        ctx.write(f"Hello, {value}!")

    @engine.preprocessor("upper")
    def _upper(source, pctx):
        return source.upper()

    @engine.renderer("html")
    def _html(doc):
        return "<html>...</html>"

Register a renderer under ``"html"`` / ``"text"`` to change how ``@type:``
is applied, or register any new name and reference it with ``@type: <name>``.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional, Union
import re


__all__ = [
    "TemplateEngine",
    "RenderContext",
    "PreprocessContext",
    "is_numeric",
    "load",
    "genHtml",
]


DirectiveFn = Callable[["RenderContext", str], None]
PreprocessorFn = Callable[[str, "PreprocessContext"], str]
RendererFn = Callable[[Dict[str, Any]], str]


_COMMENT_RE = re.compile(r"<\?.*?\?>", re.DOTALL)
_PRE_LINE_RE = re.compile(r"^[ \t]*@pre[ \t]*:[ \t]*(.+?)[ \t]*$", re.MULTILINE)
_NUM_RE = re.compile(r"^[+-]?\d+(?:\.\d+)?$")


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def is_numeric(val: Any) -> bool:
    """Return True if the whole string looks like an int or float."""
    return bool(_NUM_RE.match(str(val)))


def _coerce(value: str) -> Any:
    """Turn ``"12"`` into ``12`` and ``"1.5"`` into ``1.5``; leave rest alone."""
    if is_numeric(value):
        try:
            return int(value)
        except ValueError:
            return float(value)
    return value


# ---------------------------------------------------------------------------
# Contexts
# ---------------------------------------------------------------------------


@dataclass
class PreprocessContext:
    """Passed to every preprocessor so it can see where it is running."""

    engine: "TemplateEngine"
    fileDir: Path
    state: Dict[str, Any] = field(default_factory=dict)
    name: str = ""


@dataclass
class RenderContext:
    """Walks a template source and accumulates rendered output."""

    engine: "TemplateEngine"
    source: str
    fileDir: Path = field(default_factory=lambda: Path("."))
    index: int = 0
    header: Dict[str, Any] = field(default_factory=dict)
    state: Dict[str, Any] = field(default_factory=dict)
    content: str = ""

    def __post_init__(self) -> None:
        self._install_globals()

    # -- globals exposed to ``{expr}`` / ``!{stmt}`` -------------------
    def _install_globals(self) -> None:
        """Bind the helper names templates are allowed to use."""
        self.state["write"] = self.write
        self.state["load"] = self.load
        self.state["genHtml"] = self.engine.render_html
        self.state["ctx"] = self
        self.state.setdefault("fileDir", self.fileDir)

    # -- output --------------------------------------------------------
    def write(self, s: Any) -> None:
        """Append a value to the generated content (``None`` is skipped)."""
        if s is not None:
            self.content += str(s)

    # -- cursor helpers ------------------------------------------------
    def moveIndex(self, offset: int) -> None:
        self.index += offset

    def nextChar(self, i: int = 0) -> str:
        return self.source[self.index + i]

    def isEnd(self) -> bool:
        return self.index >= len(self.source)

    def isWhiteSpace(self) -> bool:
        return not self.isEnd() and self.source[self.index].isspace()

    def isNum(self) -> bool:
        return not self.isEnd() and self.source[self.index].isdigit()

    def isAlpha(self) -> bool:
        return not self.isEnd() and self.source[self.index].isalpha()

    def isChar(self, c: str) -> bool:
        return not self.isEnd() and self.source[self.index] == c

    def consumeNextChar(self) -> str:
        """Return the current char, honouring ``\\X`` escapes."""
        char = self.source[self.index]
        if char == "\\" and self.index + 1 < len(self.source):
            nxt = self.source[self.index + 1]
            self.index += 2
            return nxt
        self.index += 1
        return char

    def skipSpace(self) -> None:
        while self.isWhiteSpace():
            self.index += 1

    def getString(self) -> str:
        """Parse a double-quoted string and return its contents."""
        self.moveIndex(1)  # opening quote
        out: List[str] = []
        while not self.isChar('"') and not self.isEnd():
            out.append(self.source[self.index])
            self.moveIndex(1)
        self.moveIndex(1)  # closing quote
        return "".join(out)

    # -- nested loads --------------------------------------------------
    def load(
        self,
        fp: Union[str, Path],
        state: Optional[Dict[str, Any]] = None,
    ) -> Dict[str, Any]:
        """Load a nested template relative to *this* template's directory."""
        p = Path(fp)
        if not p.is_absolute():
            p = self.fileDir / p
        return self.engine.load(p, state)


# ---------------------------------------------------------------------------
# Built-in preprocessors
# ---------------------------------------------------------------------------


def _pre_strip_trailing_ws(source: str, _pctx: PreprocessContext) -> str:
    return "\n".join(line.rstrip() for line in source.split("\n"))


def _pre_collapse_blank_lines(source: str, _pctx: PreprocessContext) -> str:
    return re.sub(r"\n{3,}", "\n\n", source)


def _pre_dedent(source: str, _pctx: PreprocessContext) -> str:
    """Remove the common leading whitespace from every non-blank line."""
    lines = source.split("\n")
    indents = [len(l) - len(l.lstrip()) for l in lines if l.strip()]
    if not indents:
        return source
    common = min(indents)
    return "\n".join(l[common:] if l.strip() else l for l in lines)


# ---------------------------------------------------------------------------
# Engine
# ---------------------------------------------------------------------------


class TemplateEngine:
    """A small, extensible template engine.

    Subclass it and override ``_register_builtins`` to swap defaults, or
    call the ``register_*`` methods / use the decorators on an instance.
    """

    def __init__(self) -> None:
        self.directives: Dict[str, DirectiveFn] = {}
        self.preprocessors: Dict[str, PreprocessorFn] = {}
        self.renderers: Dict[str, RendererFn] = {}
        # Preprocessors that run on every template, before ``@pre:`` ones.
        self.always_preprocessors: List[str] = []
        self._register_builtins()

    # ------------------------------------------------------------------
    # Registration API
    # ------------------------------------------------------------------
    def register_directive(self, name: str, fn: DirectiveFn) -> DirectiveFn:
        self.directives[name] = fn
        return fn

    def register_preprocessor(self, name: str, fn: PreprocessorFn) -> PreprocessorFn:
        self.preprocessors[name] = fn
        return fn

    def register_renderer(self, name: str, fn: RendererFn) -> RendererFn:
        self.renderers[name] = fn
        return fn

    def directive(self, name: str):
        """Decorator form of :meth:`register_directive`."""

        def deco(fn: DirectiveFn) -> DirectiveFn:
            return self.register_directive(name, fn)

        return deco

    def preprocessor(self, name: str):
        """Decorator form of :meth:`register_preprocessor`."""

        def deco(fn: PreprocessorFn) -> PreprocessorFn:
            return self.register_preprocessor(name, fn)

        return deco

    def renderer(self, name: str):
        """Decorator form of :meth:`register_renderer`."""

        def deco(fn: RendererFn) -> RendererFn:
            return self.register_renderer(name, fn)

        return deco

    # ------------------------------------------------------------------
    # Built-ins (override in a subclass to change defaults)
    # ------------------------------------------------------------------
    def _register_builtins(self) -> None:
        self.directives["include"] = self._directive_include
        self.directives["pre"] = self._directive_pre

        self.renderers["html"] = self.render_html
        self.renderers["text"] = lambda doc: doc["content"]

        self.preprocessors["strip_trailing_ws"] = _pre_strip_trailing_ws
        self.preprocessors["collapse_blank_lines"] = _pre_collapse_blank_lines
        self.preprocessors["dedent"] = _pre_dedent

    # ------------------------------------------------------------------
    # Overridable evaluation hooks
    # ------------------------------------------------------------------
    def evaluate(self, src: str, state: Dict[str, Any]) -> Any:
        """Evaluate an ``{expr}`` block.  Override for sandboxing."""
        return eval(src, state)  # noqa: S307 - deliberate, templates are code

    def execute(self, src: str, state: Dict[str, Any]) -> None:
        """Execute a ``!{stmt}`` block.  Override for sandboxing."""
        exec(src, state)  # noqa: S102 - deliberate, templates are code

    # ------------------------------------------------------------------
    # Built-in directives
    # ------------------------------------------------------------------
    def _directive_include(self, ctx: RenderContext, value: str) -> None:
        out = ctx.load(value)
        ctx.state.update(out["state"])
        ctx._install_globals()  # re-bind helpers stolen by the child
        ctx.write(out["content"])

    def _directive_pre(self, ctx: RenderContext, value: str) -> None:
        # Preprocessors already ran; just record what was requested.
        ctx.header.setdefault("pre", []).append(value)

    # ------------------------------------------------------------------
    # Renderers
    # ------------------------------------------------------------------
    def render_html(self, doc: Dict[str, Any]) -> str:
        return (
            "<!DOCTYPE html>\n"
            "<html>\n<head>\n"
            f"<title>{doc['header'].get('title', '')}</title>\n"
            "</head>\n<body>\n"
            f"{doc['content']}"
            "</body>\n</html>\n"
        )

    def render(self, doc: Dict[str, Any], kind: Optional[str] = None) -> str:
        """Render *doc* through the named (or ``@type:``) renderer."""
        if kind is None:
            kind = doc["header"].get("type", "text")
        fn = self.renderers.get(kind)
        if fn is None:
            raise KeyError(f"unknown renderer: {kind!r}")
        return fn(doc)

    # ------------------------------------------------------------------
    # Preprocessing
    # ------------------------------------------------------------------
    def _apply_preprocessors(
        self,
        source: str,
        file_dir: Path,
        state: Optional[Dict[str, Any]],
    ) -> str:
        names: List[str] = list(self.always_preprocessors)
        for m in _PRE_LINE_RE.finditer(source):
            for n in m.group(1).split(","):
                n = n.strip()
                if n:
                    names.append(n)

        if not names:
            return source

        pctx = PreprocessContext(engine=self, fileDir=file_dir, state=dict(state or {}))
        for name in names:
            fn = self.preprocessors.get(name)
            if fn is None:
                raise KeyError(f"unknown preprocessor: {name!r}")
            pctx.name = name
            source = fn(source, pctx)
        return source

    # ------------------------------------------------------------------
    # Parsing
    # ------------------------------------------------------------------
    def _get_braced(self, ctx: RenderContext, skip: int = 1) -> str:
        """Return the contents of a balanced ``{...}`` block."""
        depth = 1
        ctx.moveIndex(skip)
        out: List[str] = []
        while depth > 0 and not ctx.isEnd():
            ch = ctx.source[ctx.index]
            if ch == "{":
                depth += 1
            elif ch == "}":
                depth -= 1
            if depth > 0:
                out.append(ch)
            ctx.moveIndex(1)
        return "".join(out)

    def _handle_expr(self, ctx: RenderContext) -> None:
        src = self._get_braced(ctx, skip=1)
        result = self.evaluate(src, ctx.state)
        if result is not None:
            ctx.write(str(result))

    def _handle_stmt(self, ctx: RenderContext) -> None:
        src = self._get_braced(ctx, skip=2)
        self.execute(src, ctx.state)

    def _handle_text(self, ctx: RenderContext) -> None:
        out: List[str] = []
        while not ctx.isEnd():
            ch = ctx.source[ctx.index]
            if ch == "\\" and ctx.index + 1 < len(ctx.source):
                out.append(ctx.source[ctx.index + 1])
                ctx.moveIndex(2)
                continue
            if ch == "\n":
                break
            if (
                ch == "!"
                and ctx.index + 1 < len(ctx.source)
                and ctx.source[ctx.index + 1] == "{"
            ):
                break
            if ch == "{":
                break
            out.append(ch)
            ctx.moveIndex(1)
        ctx.write("".join(out))

    def _handle_at(self, ctx: RenderContext) -> None:
        ctx.moveIndex(1)  # skip '@'
        name_chars: List[str] = []
        while not ctx.isEnd() and ctx.source[ctx.index] not in ":\n":
            name_chars.append(ctx.source[ctx.index])
            ctx.moveIndex(1)

        if ctx.isEnd() or ctx.source[ctx.index] != ":":
            # Not actually a directive (e.g. an email at start of a line)
            ctx.write("@" + "".join(name_chars))
            return

        name = "".join(name_chars).strip()
        ctx.moveIndex(1)  # skip ':'
        while ctx.isWhiteSpace():
            ctx.moveIndex(1)

        value_chars: List[str] = []
        while not ctx.isEnd() and ctx.source[ctx.index] != "\n":
            value_chars.append(ctx.source[ctx.index])
            ctx.moveIndex(1)
        value = "".join(value_chars).strip()

        handler = self.directives.get(name)
        if handler is not None:
            handler(ctx, value)
        else:
            ctx.header[name] = _coerce(value)

    def _render(self, ctx: RenderContext) -> None:
        while not ctx.isEnd():
            ch = ctx.source[ctx.index]
            if ch == "\n":
                max_nl = ctx.header.get("max-new-lines", 1)
                if not ctx.content.endswith("\n" * max_nl):
                    ctx.content += "\n"
                ctx.moveIndex(1)
            elif ch.isspace():
                ctx.write(ch)
                ctx.moveIndex(1)
            elif ch == "@":
                self._handle_at(ctx)
            elif (
                ch == "!"
                and ctx.index + 1 < len(ctx.source)
                and ctx.source[ctx.index + 1] == "{"
            ):
                self._handle_stmt(ctx)
            elif ch == "{":
                self._handle_expr(ctx)
            else:
                self._handle_text(ctx)

    # ------------------------------------------------------------------
    # Public entry points
    # ------------------------------------------------------------------
    def load(
        self,
        filePath: Union[str, Path],
        state: Optional[Dict[str, Any]] = None,
    ) -> Dict[str, Any]:
        """Render *filePath* and return ``{header, content, state, fileDir}``.

        Relative paths (includes, ``@doc`` output) resolve against the
        directory of the input file, or the current template when nested.
        """
        filePath = Path(filePath)
        source = filePath.read_text(encoding="utf-8")
        if not source.endswith("\n"):
            source += "\n"

        # 1. Comments are stripped first so preprocessors never see them.
        source = _COMMENT_RE.sub("", source)

        # 2. ``@pre:`` lines and always-on preprocessors transform the source.
        source = self._apply_preprocessors(source, filePath.parent, state)

        # 3. Normal parse.
        ctx = RenderContext(
            engine=self,
            source=source.strip(),
            fileDir=filePath.parent,
        )
        if state:
            ctx.state.update(state)
            ctx._install_globals()

        self._render(ctx)

        if not ctx.content.endswith("\n"):
            ctx.content += "\n"

        return {
            "header": ctx.header,
            "content": ctx.content,
            "state": ctx.state,
            "fileDir": filePath.parent,
        }

    def render_file(
        self,
        filePath: Union[str, Path],
        state: Optional[Dict[str, Any]] = None,
    ) -> Path:
        """Render *filePath* to the file named by its ``@doc:`` header."""
        result = self.load(filePath, state)
        out_name = result["header"].get("doc")
        if not out_name:
            raise ValueError("template must contain @doc: <filename>")

        out_path = Path(out_name)
        if not out_path.is_absolute():
            out_path = result["fileDir"] / out_path

        out_path.write_text(self.render(result), encoding="utf-8")
        return out_path


# ---------------------------------------------------------------------------
# Backwards-compatible module-level helpers
# ---------------------------------------------------------------------------

_DEFAULT_ENGINE: Optional[TemplateEngine] = None


def _default_engine() -> TemplateEngine:
    global _DEFAULT_ENGINE
    if _DEFAULT_ENGINE is None:
        _DEFAULT_ENGINE = TemplateEngine()
    return _DEFAULT_ENGINE


def load(
    filePath: Union[str, Path],
    state: Optional[Dict[str, Any]] = None,
) -> Dict[str, Any]:
    """Module-level shortcut for :meth:`TemplateEngine.load`."""
    return _default_engine().load(filePath, state)


def genHtml(doc: Dict[str, Any]) -> str:  # noqa: N802 - kept for compat
    """Module-level shortcut for :meth:`TemplateEngine.render_html`."""
    return _default_engine().render_html(doc)


# ---------------------------------------------------------------------------
# CLI
# -------------------1--------------------------------------------------------

if __name__ == "__main__":
    import sys

    if len(sys.argv) != 2:
        print("Usage: python template.py <file>")
        sys.exit(1)

    try:
        written = _default_engine().render_file(sys.argv[1])
    except Exception as exc:  # pylint: disable=broad-except
        print(f"Error: {exc}")
        sys.exit(1)
    print("Wrote", written)
