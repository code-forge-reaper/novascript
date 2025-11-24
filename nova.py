#!/usr/bin/env pypy3
from novascript import Interpreter, NovaError
import sys
import os

sys.path.insert(0, os.getcwd())


def main():
    if len(sys.argv) < 2:
        print("Usage: python nova_interpreter.py <nova_script_file.nova>")
        sys.exit(1)

    script_file = sys.argv[1]
    interpreter = Interpreter(script_file)
    interpreter.globals.define("os-import-handler", __import__)
    interpreter.interpret()


# Example usage (if this were a script):
if __name__ == "__main__":
    main()
