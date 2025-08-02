#!/usr/bin/env python
from novascript import Interpreter, NovaError
import sys
import os

sys.path.insert(0, os.getcwd())


# Example usage (if this were a script):
if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python nova_interpreter.py <nova_script_file.nova>")
        sys.exit(1)
    
    script_file = sys.argv[1]
    try:
        interpreter = Interpreter(script_file)
        interpreter.globals.define("os-import-handler", __import__)
        interpreter.interpret()
    except NovaError as e:
        print(f"NovaScript Error: {e}")
        sys.exit(1)
    except Exception as e:
        print(f"Runtime Error: {e}")
        sys.exit(1)