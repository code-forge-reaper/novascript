#!/usr/bin/env python3
"""
Minimal test runner for NovaScript.

Runs every *.nova file in this directory (except this script).
A test passes if the interpreter exits 0 and does not leave an uncaught error.
"""

from __future__ import annotations

import os
import sys
import traceback
from pathlib import Path

# Allow running from repo root or from tests/
ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT))

from novascript.globals import init_globals
from novascript.nodes import NovaError
from novascript.runtime import Interpreter


def run_one(path: Path) -> tuple[bool, str]:
    source = path.read_text(encoding="utf-8")
    interp = Interpreter(source, str(path))
    init_globals(interp, interp.globals)
    try:
        interp.interpret()
        return True, "OK"
    except NovaError as e:
        return False, f"NovaError: {e}"
    except SystemExit as e:
        code = e.code if isinstance(e.code, int) else 1
        if code == 0:
            return True, "OK"
        return False, f"SystemExit({code})"
    except Exception as e:
        return False, f"{type(e).__name__}: {e}\n{traceback.format_exc()}"


def main() -> int:
    tests_dir = Path(__file__).resolve().parent
    files = sorted(tests_dir.glob("test_*.nova"))
    if not files:
        print("No test_*.nova files found.")
        return 1

    passed = 0
    failed = 0
    print(f"Running {len(files)} test file(s)...\n")

    for f in files:
        ok, detail = run_one(f)
        status = "PASS" if ok else "FAIL"
        print(f"  [{status}] {f.name}")
        if not ok:
            for line in detail.strip().splitlines():
                print(f"         {line}")
            failed += 1
        else:
            passed += 1

    print()
    print(f"Results: {passed} passed, {failed} failed, {passed + failed} total")
    return 0 if failed == 0 else 1


if __name__ == "__main__":
    raise SystemExit(main())
