#!/usr/bin/env python

import argparse
import sys

from novascript.globals import init_globals
from novascript.nodes import NovaError
from novascript.runtime import Interpreter


def main():
    parser = argparse.ArgumentParser(
        prog="nova",
        description="Nova programming language interpreter",
    )

    parser.add_argument(
        "script",
        nargs="?",
        help="Nova source file (or '-' for stdin)",
    )
    parser.add_argument(
        "-d",
        "--dump",
        default=False,
        help="dump ast?",
        required=False,
        action="store_true",
    )

    args = parser.parse_args()

    if args.script is None:
        parser.print_help()
        return 0

    if args.script == "-":
        source = sys.stdin.read()
        filename = "<stdin>"
    else:
        with open(args.script, "r", encoding="utf8") as f:
            source = f.read()
        filename = args.script

    interp = Interpreter(source, filename)
    init_globals(interp, interp.globals)
    if args.dump:
        from pprint import pprint

        bb = interp.tk.parse_block()
        for b in bb:
            pprint(b.to_dict())
    else:
        interp.interpret()

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
