#!/usr/bin/env python

from novascript import Interpreter, NovaError
import sys
import os


def main():
    if len(sys.argv) < 2:
        print("Usage: python nova_interpreter.py <nova_script_file.nova>")
        sys.exit(1)

    script_file = sys.argv[1]
    if script_file == "-":
        source = sys.stdin.read()
        script_file = "<stdin>"
    else:
        with open(script_file, "r", encoding="utf8") as f:
            source = f.read()

    interpreter = Interpreter(source, script_file)
    interpreter.interpret()


# Example usage (if this were a script):
if __name__ == "__main__":
    main()
