from . import helpers
from .typechecker import check_type
from .nodes import NovaError

class FuncWrapp:
    __name__:str = "FuncWrapp"
    def __init__(self, func, desc_repr, desc_str, node, interp, name=None):
        self.func = func
        self.desc_repr = desc_repr
        self.desc_str = desc_str
        self._name = name
        self._node = node
        self.interp = interp

    def __call__(self, *args, **kw):
        return self.func(*args, **kw)

    def _short_name(self):
        if self._name:
            return self._name
        if self._node is not None and getattr(self._node, "name", None):
            return self._node.name
        return "lambda"

    def _want_ast(self):
        rt = self.interp
        if rt is None:
            return False
        return bool(getattr(rt, "showFunctionAst", False))

    def __str__(self):
        if self._want_ast() and self.desc_str is not None:
            try:
                return self.desc_str()
            except Exception:
                pass
        return f"<function {self._short_name()}>"

    def __repr__(self):
        if self._want_ast() and self.desc_repr is not None:
            try:
                return self.desc_repr()
            except Exception:
                pass
        return f"<function {self._short_name()}>"


class Var:
    def __init__(
        self, name, value, const=False, annotation=None, private=False, origin=None
    ):
        self.name = name
        self.value = value
        self.const = const
        self.type_annotation = annotation
        self.private = private
        self.origin = origin

    def __repr__(self):
        const_flag = "const " if self.const else ""
        type_info = f" {self.type_annotation} " if self.type_annotation else ""
        return f"Var({const_flag}{self.name}{type_info}= {self.value!r})"


# --- Environment for variable scoping ---
class Environment:
    def __init__(self, parent=None):
        self.values: dict[str, Var] = {
            # name, value, const
            "true": Var("true", True, True),
            "false": Var("false", False, True),
        }
        self.parent = parent
        self.deferred = []
        self.locked = False
        self.localsOnly = False

    def lock(self):
        self.locked = True

    def unlock(self):
        self.locked = False

    def add_deferred(self, stmt):
        if self.locked:
            raise NovaError(stmt, "Cannot add deferred statement to locked environment")
        self.deferred.append(stmt)

    def execute_deferred(self, interpreter):
        if self.locked:
            raise NovaError(
                None, "Cannot execute deferred statements in locked environment"
            )
        # Execute in reverse order of deferral
        while self.deferred:
            stmt = self.deferred.pop()
            interpreter.execute_stmt(stmt, self)

    def define(self, name: str, value, const=False, typeAnnotation=None):
        if self.locked:
            raise NovaError(None, "Cannot define variable in locked environment")
        self.values[name] = Var(name, value, const, typeAnnotation)

    def has(self, name: str):
        if name in self.values:
            return True
        elif self.parent and not self.localsOnly:
            return self.parent.has(name)
        else:
            return False

    def assign(self, name: str, value, tok):
        if self.locked:
            raise NovaError(tok, "Cannot assign to variable in locked environment")

        if name in self.values:
            var = self.values[name]

            # Check for constant violation (existing logic)
            if var.const:
                raise NovaError(tok, f"Cannot re-assign constant variable '{name}'")

            check_type(var.type_annotation, value, tok)
            var.value = value

        elif self.parent:
            if self.localsOnly:
                raise NovaError(
                    tok,
                    f"Cannot modify variable '{name}', since it is not created in this scope",
                )
            self.parent.assign(name, value, tok)
        else:
            raise NovaError(tok, f"Undefined variable {name}")

    def get(self, name: str, tok=None):
        if name in self.values:
            return self.values[name]
        elif self.parent:
            return self.parent.get(name, tok)
        else:
            if tok:
                raise NovaError(tok, f"Undefined variable {name}")
            raise Exception(
                f"Undefined variable {name}"
            )  # Fallback for internal errors without token

    def __repr__(self):
        v = {}
        for k in self.values:
            vv = self.values[k]
            if k not in ["true", "false"]:
                v[k] = vv
        return str(v)

