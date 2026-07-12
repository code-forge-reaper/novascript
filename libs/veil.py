#!/usr/bin/env python
"""
Veil Language Transformer – CLI + Reusable API

Library usage:
    from veil import load_rules, apply_rules, format_code, execute_code

    rules, includes = load_rules("lang.reg")
    code = apply_rules("your_input_text", rules)
    full_code = "\n".join(includes) + "\n" + code
    formatted = format_code(full_code, use_black=True)
    execute_code(formatted, sandbox=True)   # prints to stdout/stderr

CLI usage (unchanged):
    python veil.py input.txt [--rules lang.reg] [--sandbox] [--verbose] ...
"""

import re
import sys
import os
import shlex
import builtins
import argparse
from typing import List, Tuple, Optional, Dict

try:
    import black
    HAS_BLACK = True
except ImportError:
    HAS_BLACK = False


# ---------------------------------------------------------------------------
# Safe built‑ins for sandboxing (same as original)
# ---------------------------------------------------------------------------
SAFE_BUILTINS = {
    "__build_class__": builtins.__build_class__,
    "print": builtins.print,
    "input": builtins.input,
    "open": builtins.open,
    "len": builtins.len,
    "range": builtins.range,
    "str": builtins.str,
    "abs": builtins.abs,
    "bool": builtins.bool,
    "int": builtins.int,
    "float": builtins.float,
    "object": builtins.object,
    "__name__": "__main__",
    "exit": builtins.exit,
}


# ---------------------------------------------------------------------------
# Rule parsing (API) – now with variable system
# ---------------------------------------------------------------------------
class RulesError(Exception):
    pass


def parse_variable_file(path: str) -> Dict[str, str]:
    """
    Parse a variable definition file.
    Format per line: NAME : VALUE  or  NAME = VALUE
    Lines starting with '#' or empty are ignored.
    """
    vars_dict = {}
    with open(path, encoding='utf-8') as f:
        for line_num, raw_line in enumerate(f, 1):
            line = raw_line.strip()
            if not line or line.startswith('#'):
                continue
            m = re.match(r'^([a-zA-Z_]\w*)\s*[:=]\s*(.+)$', line)
            if m:
                name = m.group(1)
                value = m.group(2).strip()
                vars_dict[name] = value
            else:
                raise RulesError(f"Invalid variable definition at line {line_num}: {raw_line!r}")
    return vars_dict


def substitute_vars(pattern: str, variables: Dict[str, str]) -> str:
    """
    Replace $NAME or ${NAME} with the value from variables.
    A backslash before the dollar (\\$) prevents substitution.
    """
    def repl(match):
        name = match.group(1) or match.group(2)
        if name not in variables:
            raise RulesError(f"Undefined variable: ${name}")
        return variables[name]

    return re.sub(
        r'(?<!\\)\$(\w+)|(?<!\\)\$\{(\w+)\}',
        repl,
        pattern
    )


def parse_rules(source: str, base_dir: Optional[str] = None) -> Tuple[List[Tuple[re.Pattern, str]], List[str]]:
    """
    Parse rules from a string (content of a .reg file).
    Returns: (rules, python_includes)
      - rules: list of (compiled_regex, replacement_string)
      - python_includes: list of Python source strings from %include directives

    New directive: %load <file>  - loads variable definitions from <file>.
    Variables are used as $NAME or ${NAME} in rule patterns.
    """
    lines = source.splitlines()
    cleaned_lines = [re.sub(r"(?<!\\)#.*$", "", line) for line in lines]

    rules = []
    python_includes = []
    seen_includes = set()
    variables = {}
    loaded_vars = set()

    i = 0
    n = len(cleaned_lines)
    in_rule = False
    current_pattern = None
    current_replacement = []

    def finalise_rule():
        nonlocal in_rule, current_pattern, current_replacement
        if current_pattern is not None:
            # Substitute variables in the pattern
            try:
                substituted = substitute_vars(current_pattern, variables)
            except RulesError as e:
                raise RulesError(f"Error in pattern '{current_pattern}': {e}")
            try:
                compiled = re.compile(substituted)
            except re.error as exc:
                raise RulesError(f"Invalid regex pattern: {substituted} – {exc}")
            replacement = "\n".join(current_replacement)
            rules.append((compiled, replacement))
            in_rule = False
            current_pattern = None
            current_replacement = []

    while i < n:
        raw_line = cleaned_lines[i]
        line_num = i + 1
        stripped = raw_line.strip()
        if not stripped:
            i += 1
            continue

        # Pre‑processor directives
        if stripped.startswith("%"):
            try:
                tokens = shlex.split(stripped[1:].strip())
            except ValueError:
                tokens = stripped[1:].strip().split()
            if tokens:
                cmd = tokens[0].lower()
                if cmd == "include":
                    if len(tokens) < 2:
                        raise RulesError(f"%include missing path at line {line_num}")
                    path = tokens[1]
                    resolved = os.path.normpath(os.path.join(base_dir or ".", path))
                    if resolved not in seen_includes:
                        if not os.path.exists(resolved):
                            raise RulesError(f"Python include not found: {resolved} (line {line_num})")
                        with open(resolved, encoding="utf-8") as f:
                            py_src = f.read()
                        seen_includes.add(resolved)
                        python_includes.append(py_src)
                elif cmd == "load":
                    if len(tokens) < 2:
                        raise RulesError(f"%load missing path at line {line_num}")
                    path = tokens[1]
                    resolved = os.path.normpath(os.path.join(base_dir or ".", path))
                    if resolved not in loaded_vars:
                        if not os.path.exists(resolved):
                            raise RulesError(f"Variable file not found: {resolved} (line {line_num})")
                        new_vars = parse_variable_file(resolved)
                        variables.update(new_vars)   # later definitions override earlier ones
                        loaded_vars.add(resolved)
                else:
                    raise RulesError(f"Unknown preprocessing directive: {stripped}")
            i += 1
            continue

        # Rule pattern line
        if not raw_line.startswith((" ", "\t")) and "=>" in stripped:
            finalise_rule()
            pattern_part, _, replacement_part = stripped.partition("=>")
            current_pattern = pattern_part.strip()
            current_replacement = [replacement_part.strip()]
            in_rule = True
            i += 1
            continue

        # Continuation line
        if in_rule and (raw_line.startswith((" ", "\t")) or not stripped):
            current_replacement.append(stripped)
            i += 1
            continue

        i += 1

    finalise_rule()
    return rules, python_includes


def load_rules(path: str) -> Tuple[List[Tuple[re.Pattern, str]], List[str]]:
    """Load rules from a file (same as original)."""
    with open(path, encoding="utf-8") as f:
        source = f.read()
    base_dir = os.path.dirname(os.path.abspath(path)) or "."
    return parse_rules(source, base_dir)

# ---------------------------------------------------------------------------
# Transformation, formatting, execution (unchanged)
# ---------------------------------------------------------------------------
def apply_rules(text: str, rules: List[Tuple[re.Pattern, str]], args=None) -> str:
    """Apply all rules sequentially (same as original)."""
    for pattern, replacement in rules:
        try:
            text = pattern.sub(replacement, text)
            if args and args.verbose:
                preview = replacement.replace("\n", "\\n")[:50]
                print(f"🔧 Applied: {pattern.pattern} => {preview}...")
        except Exception as exc:
            raise RuntimeError(f"Error applying rule '{pattern.pattern}': {exc}")
    if args and args.verbose:
        print(f"✅ Transformations applied: {len(rules)}")
    return text


def format_code(code: str, use_black: bool = True) -> str:
    """Format with Black if available and enabled."""
    if use_black and HAS_BLACK:
        try:
            return black.format_str(code, mode=black.Mode())
        except Exception:
            pass
    return code


def execute_code(code: str, env: Optional[dict] = None, sandbox: bool = False) -> None:
    """Execute the given Python code. If sandbox is True, restrict builtins."""
    if env is None:
        env = {}
    if sandbox:
        env["__builtins__"] = SAFE_BUILTINS
    return exec(code, env)


# ---------------------------------------------------------------------------
# Command‑line interface (unchanged from original, uses the API)
# ---------------------------------------------------------------------------
def setup_cli() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Veil Language Transformer")
    parser.add_argument("input_file", help="Input file to process (e.g., input.txt)")
    parser.add_argument(
        "--rules", "-r", default="lang.reg", help="Rules file (default: lang.reg)"
    )
    parser.add_argument(
        "--verbose", "-v", action="store_true", help="Enable verbose logging"
    )
    parser.add_argument("--show-rules", action="store_true", help="Show parsed rules")
    parser.add_argument(
        "--show-original", action="store_true", help="Show original transformed code"
    )
    parser.add_argument(
        "--show-formatted", action="store_true", help="Show Black-formatted code"
    )
    parser.add_argument(
        "--no-format", action="store_true", help="Disable Black code formatting"
    )
    parser.add_argument(
        "--sandbox", action="store_true", help="Enables sandboxing", default=False
    )
    parser.add_argument(
        "--save-format", default="", help="Where to save the formatted code"
    )
    parser.add_argument(
        "--dry-run", action="store_true", help="Show transformations without executing"
    )
    parser.add_argument(
        "--print-code", action="store_true", help="Print generated code without executing"
    )
    return parser.parse_args()


def veil(input_text, env=None, rules="lang.reg"):
    rules, includes = load_rules(rules)
    code = apply_rules(input_text, rules)
    full = "\n".join(includes) + "\n" + code
    formatted = format_code(full)

    return execute_code(formatted, env)        # runs, prints directly


def main() -> None:
    args = setup_cli()

    if args.verbose:
        print("🚀 Starting Veil Transformer")
        print(f"📁 Input: {args.input_file}")
        print(f"📜 Rules: {args.rules}")

    # Load rules
    try:
        rules, python_includes = load_rules(args.rules)
    except FileNotFoundError:
        print(f"❌ Rules file not found: {args.rules}")
        sys.exit(1)
    except RulesError as exc:
        print(f"❌ {exc}")
        sys.exit(1)

    if args.show_rules:
        print("=== Parsed Rules ===")
        for idx, (pattern, replacement) in enumerate(rules, 1):
            print(f"Rule {idx}:")
            print(f"  Pattern: {pattern.pattern}")
            print(f"  Replacement: {replacement}")
            print()

    # Load input
    try:
        with open(args.input_file, encoding="utf-8") as f:
            input_text = f.read()
    except FileNotFoundError:
        print(f"❌ Input file not found: {args.input_file}")
        sys.exit(1)

    if args.show_original:
        print("=== Original Code ===")
        print(input_text)
        print("=====================")

    # Apply transformations
    transformed = apply_rules(input_text, rules, args)

    # Combine includes
    full_code = "\n".join(python_includes) + "\n" + transformed

    # Format
    formatted = full_code
    if not args.no_format:
        formatted = format_code(full_code)
        if args.verbose:
            print("ℹ  Black formatting applied")

    # Show / save
    if args.show_formatted:
        print("=== Formatted Code ===")
        print(formatted)
        print("======================")

    if args.save_format:
        with open(args.save_format, "w", encoding="utf-8") as f:
            f.write(formatted)
        print(f"💾 Formatted code saved to {args.save_format}")
        sys.exit(0)

    if args.dry_run:
        print("🚫 Dry run – execution skipped")
        if args.print_code:
            print(formatted)
        return

    if args.print_code:
        print(formatted)
        return

    # Execute (identical to original)
    try:
        if args.sandbox:
            env = {"__builtins__": SAFE_BUILTINS}
        else:
            env = {}
        exec(formatted, env)
    except Exception as e:
        print(f"❌ Execution error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()