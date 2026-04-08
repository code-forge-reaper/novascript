#!/usr/bin/env python3

from novascript import Interpreter, NovaError
import sys
import importlib
import os


def main():
    if len(sys.argv) < 2:
        print("Usage: python nova_interpreter.py <nova_script_file.nova>")
        sys.exit(1)

    script_file = sys.argv[1]
    if script_file == "-":
        if sys.platform == "linux":
            script_file = "/dev/stdin"
        else:
            raise ValueError("cannot set stdin as the file on this platform")
    interpreter = Interpreter(script_file)
    interpreter.globals.define("os-import-handler", importlib.import_module)
    interpreter.interpret()


# Example usage (if this were a script):
if __name__ == "__main__":
    main()
