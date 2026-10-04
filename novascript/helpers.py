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


_DEBUG_MODE = os.environ.get("DEBUG", "")


def dprint(msg: str, node: Token):
    if not _DEBUG_MODE:
        return
    if _DEBUG_MODE == "Dict":
        pprint(" " * node.column + f"- {msg} {node.to_dict()}")
    elif _DEBUG_MODE == "Json":
        pprint(" " * node.column + f"- {msg} {node.to_json()}")
    elif _DEBUG_MODE == "Node":
        print(" " * node.column + node.__str__())
    else:
        raise ValueError("DEBUG must be 'Dict', 'Json' or 'Node'")
