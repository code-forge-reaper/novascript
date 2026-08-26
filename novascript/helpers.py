
from pprint import pprint
import os

from .tokenizer import Token
from .nodes import NovaError
class Proxy:
    def __init__(self, set_func, get_func, instance, interpreter, cls):
        self._set = set_func
        self._get = get_func
        self.defining = instance
        self.interpreter = interpreter  # Need reference for stack
        self.cls = cls  # The defining class for private access

    def __str__(self):
        return str(self.get())

    def __repr__(self):
        return str(self.get())

    def get(self):
        if not self._get:
            raise NovaError(None, "Property has no getter.")
        try:
            if self.cls and self.interpreter:
                self.interpreter.current_class_stack.append(self.cls)
            return self._get()
        finally:
            if self.cls and self.interpreter and self.interpreter.current_class_stack:
                self.interpreter.current_class_stack.pop()

    def set(self, value):
        if not self._set:
            raise NovaError(None, "Property has no setter.")
        try:
            if self.cls and self.interpreter:
                self.interpreter.current_class_stack.append(self.cls)
            return self._set(value)
        finally:
            if self.cls and self.interpreter and self.interpreter.current_class_stack:
                self.interpreter.current_class_stack.pop()


def dprint(str: str, node: Token):
    if os.environ.get("debugMode", "") == "Pretty":
        pprint.pprint(" " * node.column + f"- {str} {node.to_dict()}")
    elif os.environ.get("debugMode", "") == "Node":
        pprint.pprint(" " * node.column + f"- {str} {node.to_json()}")
    elif os.environ.get("debugMode", "") == "Simple":
        print(" " * node.column + node.__str__())

_RUNTIME_REF = None