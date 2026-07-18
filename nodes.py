import json


# --- Control Flow Classes ---
class ControlFlow:
    """Base class for control flow signals"""

    def to_dict(self):
        return {"type": self.__class__.__name__}


class ReturnFlow(ControlFlow):
    def __init__(self, value):
        self.value = value

    def to_dict(self):
        return {"type": "ReturnFlow", "value": self._serialize_value(self.value)}

    @staticmethod
    def _serialize_value(val):
        if hasattr(val, "to_dict"):
            return val.to_dict()
        return repr(val) if val is not None else None


class BreakFlow(ControlFlow):
    def to_dict(self):
        return {"type": "BreakFlow"}


class ContinueFlow(ControlFlow):
    def to_dict(self):
        return {"type": "ContinueFlow"}


class NovaError(Exception):
    """Custom error class for NovaScript, including location information."""

    def __init__(self, token, user_message=None):
        file = getattr(token, "file", "unknown_file") if token else "unknown_file"
        line = getattr(token, "line", 0) if token else 0
        column = getattr(token, "column", 0) if token else 0
        token_value = getattr(token, "value", "N/A") if token else "N/A"
        self.message = f"{file}:{line}:{column}: {user_message or 'unknown error at token: ' + str(token_value)}"
        super().__init__(self.message)
        self.line = line
        self.column = column
        self.file = file
        self.user_message = user_message
        self.token_value = token_value

    def to_dict(self):
        return {
            "type": "NovaError",
            "file": self.file,
            "line": self.line,
            "column": self.column,
            "user_message": self.user_message,
            "token_value": str(self.token_value),
        }


# Helper for indentation in __str__ methods (not used, kept for compatibility)
INDENT_STEP = "  "


# --- AST Node Base Classes ---
class Token:
    def __init__(self, type, value, file, line, column):
        self.type = type
        self.value = value
        self.file = file
        self.line = line
        self.column = column

    def __str__(self):
        return f"Token(type='{self.type}', value={repr(self.value)}, file='{self.file}', line={self.line}, column={self.column})"

    def to_dict(self):
        """Base serialization – overridden in subclasses."""
        return {
            "type": self.type,
            "value": repr(self.value),
            "file": self.file,
            "line": self.line,
            "column": self.column,
        }

    def to_json(self):
        """Return a JSON string for debug."""
        return json.dumps(self.to_dict(), indent=2)


class Statement(Token):
    def __init__(self, type, file, line, column):
        super().__init__(type, None, file, line, column)


class Expression(Token):
    def __init__(self, type, file, line, column):
        super().__init__(type, None, file, line, column)


# Helper to serialize child nodes or lists of them (for to_dict)
def _serialize_node(node):
    if hasattr(node, "to_dict"):
        return node.to_dict()
    return repr(node)


def _serialize_list(lst):
    return [_serialize_node(item) for item in lst]


# --- Specific AST Node Classes ---
class Literal(Expression):
    def __init__(self, value, file, line, column):
        super().__init__("Literal", file, line, column)
        self.value = value

    def __str__(self):
        return f"Literal(value={repr(self.value)})"

    def to_dict(self):
        base = super().to_dict()
        base["literal_value"] = self.value
        return base


class Identifier(Expression):
    def __init__(self, name, file, line, column):
        super().__init__("Identifier", file, line, column)
        self.name = name

    def __str__(self):
        return f"Identifier(name='{self.name}')"

    def to_dict(self):
        base = super().to_dict()
        base["name"] = self.name
        return base


class BinaryExpr(Expression):
    def __init__(self, operator, left, right, file, line, column):
        super().__init__("BinaryExpr", file, line, column)
        self.operator = operator
        self.left = left
        self.right = right

    def __str__(self):
        return f"BinaryExpr(operator='{self.operator}', left={self.left}, right={self.right})"

    def to_dict(self):
        base = super().to_dict()
        base["operator"] = self.operator
        base["left"] = _serialize_node(self.left)
        base["right"] = _serialize_node(self.right)
        return base


class PipeExpr(BinaryExpr):
    def __init__(self, left, right, file, line, column):
        super().__init__("->", left, right, file, line, column)

    def __str__(self):
        return f"PipeExpr(left={self.left}, right={self.right})"


class MapExpr(BinaryExpr):
    def __init__(self, left, right, file, line, column):
        super().__init__("=>", left, right, file, line, column)

    def __str__(self):
        return f"MapExpr(left={self.left}, right={self.right})"


class UnaryExpr(Expression):
    def __init__(self, operator, right, file, line, column):
        super().__init__("UnaryExpr", file, line, column)
        self.operator = operator
        self.right = right

    def __str__(self):
        return f"UnaryExpr(operator='{self.operator}', right={self.right})"

    def to_dict(self):
        base = super().to_dict()
        base["operator"] = self.operator
        base["right"] = _serialize_node(self.right)
        return base


class FuncCall(Expression):
    def __init__(self, name, arguments, file, line, column):
        super().__init__("FuncCall", file, line, column)
        self.name = name
        self.arguments = arguments

    def __str__(self):
        return f"FuncCall(name={self.name}, arguments={self._str_args()})"

    def _str_args(self):
        return "[" + ", ".join(str(a) for a in self.arguments) + "]"

    def to_dict(self):
        base = super().to_dict()
        base["function_name"] = (
            str(self.name) if not isinstance(self.name, str) else self.name
        )
        base["arguments"] = _serialize_list(self.arguments)
        return base


class MethodCall(Expression):
    def __init__(self, object, method, arguments, file, line, column):
        super().__init__("MethodCall", file, line, column)
        self.object = object
        self.method = method
        self.arguments = arguments

    def __str__(self):
        return f"MethodCall(object={self.object}, method='{self.method}', arguments={self._str_args()})"

    def _str_args(self):
        return "[" + ", ".join(str(a) for a in self.arguments) + "]"

    def to_dict(self):
        base = super().to_dict()
        base["object"] = _serialize_node(self.object)
        base["method"] = self.method
        base["arguments"] = _serialize_list(self.arguments)
        return base


class PropertyAccess(Expression):
    def __init__(self, object, property, file, line, column):
        super().__init__("PropertyAccess", file, line, column)
        self.object = object
        self.property = property

    def __str__(self):
        return f"PropertyAccess(object={self.object}, property='{self.property}')"

    def to_dict(self):
        base = super().to_dict()
        base["object"] = _serialize_node(self.object)
        base["property"] = self.property
        return base


class ArrayAccess(Expression):
    def __init__(self, object, index, file, line, column):
        super().__init__("ArrayAccess", file, line, column)
        self.object = object
        self.index = index

    def __str__(self):
        return f"ArrayAccess(object={self.object}, index={self.index})"

    def to_dict(self):
        base = super().to_dict()
        base["object"] = _serialize_node(self.object)
        base["index"] = _serialize_node(self.index)
        return base


class ArrayLiteral(Expression):
    def __init__(self, elements, file, line, column):
        super().__init__("ArrayLiteral", file, line, column)
        self.elements = elements

    def __str__(self):
        return f"ArrayLiteral(elements={self._str_elements()})"

    def _str_elements(self):
        return "[" + ", ".join(str(e) for e in self.elements) + "]"

    def to_dict(self):
        base = super().to_dict()
        base["elements"] = _serialize_list(self.elements)
        return base


class ObjectLiteral(Expression):
    def __init__(self, properties, file, line, column):
        super().__init__("ObjectLiteral", file, line, column)
        self.properties = properties  # list of dicts with 'key' and 'value'

    def __str__(self):
        props = []
        for p in self.properties:
            key = p["key"] if isinstance(p["key"], str) else str(p["key"])
            props.append(f"{key}: {p['value']}")
        return f"ObjectLiteral(properties={{{', '.join(props)}}})"

    def to_dict(self):
        base = super().to_dict()
        props = []
        for p in self.properties:
            props.append(
                {
                    "key": p["key"]
                    if isinstance(p["key"], str)
                    else _serialize_node(p["key"]),
                    "value": _serialize_node(p["value"]),
                }
            )
        base["properties"] = props
        return base


class AssignmentExpr(Expression):
    def __init__(self, target, value, operator, file, line, column):
        super().__init__("AssignmentExpr", file, line, column)
        self.target = target
        self.value = value
        self.operator = operator

    def __str__(self):
        return f"AssignmentExpr(target={self.target}, value={self.value}, operator='{self.operator}')"

    def to_dict(self):
        base = super().to_dict()
        base["target"] = _serialize_node(self.target)
        base["value"] = _serialize_node(self.value)
        base["operator"] = self.operator
        return base


class NewInstance(Expression):
    def __init__(self, class_name, arguments, file, line, column):
        super().__init__("NewInstance", file, line, column)
        self.class_name = class_name
        self.arguments = arguments

    def __str__(self):
        return (
            f"NewInstance(class_name='{self.class_name}', arguments={self._str_args()})"
        )

    def _str_args(self):
        return "[" + ", ".join(str(a) for a in self.arguments) + "]"

    def to_dict(self):
        base = super().to_dict()
        base["class_name"] = self.class_name
        base["arguments"] = _serialize_list(self.arguments)
        return base


class ExplodeExpr(Expression):
    def __init__(self, args, file, line, col):
        super().__init__("ExplodeExpr", file, line, col)
        self.args = args

    def __str__(self):
        return f"ExplodeExpr(args={self._str_args()})"

    def _str_args(self):
        return "[" + ", ".join(str(a) for a in self.args) + "]"

    def to_dict(self):
        base = super().to_dict()
        base["args"] = _serialize_list(self.args)
        return base


class DecoratorExpr(Expression):
    def __init__(self, expr, body, file, line, column):
        super().__init__("DecoratorExpr", file, line, column)
        self.expr = expr
        self.body = body

    def __str__(self):
        return f"DecoratorExpr(expr={self.expr}, body={self.body})"

    def to_dict(self):
        base = super().to_dict()
        base["expr"] = _serialize_node(self.expr)
        base["body"] = _serialize_node(self.body)
        return base


class Parameter(Token):
    def __init__(
        self,
        name,
        annotation_type,
        default,
        is_compact,
        file,
        line,
        column,
        type,
        value,
    ):
        super().__init__(type, value, file, line, column)
        self.name = name
        self.annotation_type = annotation_type
        self.default = default
        self.is_compact = is_compact

    def __str__(self):
        default_str = f", default={self.default}" if self.default is not None else ""
        return f"Parameter(name='{self.name}', annotation_type={self.annotation_type}{default_str}, is_compact={self.is_compact})"

    def to_dict(self):
        base = super().to_dict()
        base["name"] = self.name
        base["annotation_type"] = self.annotation_type
        base["default"] = _serialize_node(self.default) if self.default else None
        base["is_compact"] = self.is_compact
        return base


class Case:
    def __init__(self, case_expr, body):
        self.case_expr = case_expr
        self.body = body

    def __str__(self):
        return f"Case(case_expr={self.case_expr if self.case_expr else 'default'}, body={self._str_body()})"

    def _str_body(self):
        return "[" + ", ".join(str(s) for s in self.body) + "]"

    def to_dict(self):
        return {
            "type": "Case",
            "case_expr": _serialize_node(self.case_expr)
            if self.case_expr
            else "default",
            "body": _serialize_list(self.body),
        }


class PropertyHandler(Statement):
    def __init__(self, name, getter, setter, file, line, column):
        super().__init__("PropertyHandler", file, line, column)
        self.name = name
        self.getter = getter
        self.setter = setter

    def __str__(self):
        return f"PropertyHandler(name={self.name}, getter={self.getter}, setter={self.setter})"

    def to_dict(self):
        base = super().to_dict()
        base["name"] = _serialize_node(self.name)
        base["getter"] = _serialize_node(self.getter)
        base["setter"] = _serialize_node(self.setter)
        return base


class ExportStmt(Statement):
    def __init__(self, expr, file, line, column):
        super().__init__("ExportStmt", file, line, column)
        self.expr = expr

    def __str__(self):
        return f"ExportStmt(expr={self.expr})"

    def to_dict(self):
        base = super().to_dict()
        base["expression"] = _serialize_node(self.expr)
        return base


class DeferStmt(Statement):
    def __init__(self, body, file, line, column):
        super().__init__("DeferStmt", file, line, column)
        self.body = body

    def __str__(self):
        return f"DeferStmt(body={self._str_body()})"

    def _str_body(self):
        return "[" + ", ".join(str(s) for s in self.body) + "]"

    def to_dict(self):
        base = super().to_dict()
        base["body"] = _serialize_list(self.body)
        return base


class VarDeclStmt(Statement):
    def __init__(self, name, type_annotation, initializer, file, line, column):
        super().__init__("VarDecl", file, line, column)
        self.name = name
        self.type_annotation = type_annotation
        self.initializer = initializer

    def __str__(self):
        type_str = (
            f", type_annotation={self.type_annotation}" if self.type_annotation else ""
        )
        init_str = f", initializer={self.initializer}" if self.initializer else ""
        return f"VarDeclStmt(name='{self.name}'{type_str}{init_str})"

    def to_dict(self):
        base = super().to_dict()
        base["name"] = self.name
        base["type_annotation"] = self.type_annotation
        base["initializer"] = _serialize_node(self.initializer)
        return base


class ConstDeclStmt(Statement):
    def __init__(self, name, type_annotation, initializer, file, line, column):
        super().__init__("ConstDecl", file, line, column)
        self.name = name
        self.type_annotation = type_annotation
        self.initializer = initializer

    def __str__(self):
        type_str = (
            f", type_annotation={self.type_annotation}" if self.type_annotation else ""
        )
        init_str = f", initializer={self.initializer}" if self.initializer else ""
        return f"ConstDeclStmt(name='{self.name}'{type_str}{init_str})"

    def to_dict(self):
        base = super().to_dict()
        base["name"] = self.name
        base["type_annotation"] = self.type_annotation
        base["initializer"] = _serialize_node(self.initializer)
        return base


class CustomTypeProperty:
    def __init__(self, name, type):
        self.name = name
        self.type = type

    def __str__(self):
        return f"CustomTypeProperty(name='{self.name}', type='{self.type}')"

    def to_dict(self):
        return {"name": self.name, "type": self.type}


class CustomType:
    def __init__(self, name, properties, file, line, column):
        self.name = name
        self.properties = properties
        self.file = file
        self.line = line
        self.column = column

    def __str__(self):
        props = ", ".join(str(p) for p in self.properties)
        return f"CustomType(name='{self.name}', properties=[{props}])"

    def to_dict(self):
        return {
            "type": "CustomType",
            "name": self.name,
            "properties": [p.to_dict() for p in self.properties],
            "file": self.file,
            "line": self.line,
            "column": self.column,
        }


class CustomTypeDeclStmt(Statement):
    def __init__(self, name, definition, file, line, column):
        super().__init__("CustomTypeDeclStmt", file, line, column)
        self.name = name
        self.definition = definition  # list of CustomTypeProperty

    def __str__(self):
        props = ", ".join(str(p) for p in self.definition)
        return f"CustomTypeDeclStmt(name='{self.name}', properties=[{props}])"

    def to_dict(self):
        base = super().to_dict()
        base["name"] = self.name
        base["properties"] = [p.to_dict() for p in self.definition]
        return base


class AssertStmt(Statement):
    def __init__(self, expression, message, file, line, column):
        super().__init__("AssertStmt", file, line, column)
        self.expression = expression
        self.message = message

    def __str__(self):
        msg_str = f", message='{self.message}'" if self.message else ""
        return f"AssertStmt(expression={self.expression}{msg_str})"

    def to_dict(self):
        base = super().to_dict()
        base["expression"] = _serialize_node(self.expression)
        base["message"] = self.message
        return base


class ClassDefinition(Statement):
    def __init__(self, name, superclass_name, body, file, line, column):
        super().__init__("ClassDefinition", file, line, column)
        self.name = name
        self.superclass_name = superclass_name
        self.body = body

    def __str__(self):
        super_str = (
            f", superclass='{self.superclass_name}'" if self.superclass_name else ""
        )
        body_str = "[" + ", ".join(str(s) for s in self.body) + "]"
        return f"ClassDefinition(name='{self.name}'{super_str}, body={body_str})"

    def to_dict(self):
        base = super().to_dict()
        base["class_name"] = self.name
        base["superclass"] = self.superclass_name
        base["body"] = _serialize_list(self.body)
        return base


class ObjectDecl(Statement):
    def __init__(self, name, body, file, line, column):
        super().__init__("ObjectDecl", file, line, column)
        self.name = name
        self.body = body

    def __str__(self):
        body_str = "[" + ", ".join(str(s) for s in self.body) + "]"
        return f"ObjectDecl(name='{self.name}', body={body_str})"

    def to_dict(self):
        base = super().to_dict()
        base["name"] = self.name
        base["body"] = _serialize_list(self.body)
        return base


class ProxyDef(Statement):
    def __init__(
        self,
        setter,
        getter,
        name,
        file,
        line,
        column,
    ):
        super().__init__("ProxyDef", file, line, column)
        self.name = name
        self.setter = setter
        self.getter = getter

    def __str__(self):
        return f"ProxyDef({self.name})"

class MethodDefinition(Statement):
    def __init__(
        self,
        name,
        parameters,
        body,
        is_static,
        is_constructor,
        is_private,
        file,
        line,
        column,
    ):
        super().__init__("MethodDefinition", file, line, column)
        self.name = name
        self.parameters = parameters
        self.body = body
        self.is_static = is_static
        self.is_constructor = is_constructor
        self.is_private = is_private

    def __str__(self):
        params = "[" + ", ".join(str(p) for p in self.parameters) + "]"
        body_str = "[" + ", ".join(str(s) for s in self.body) + "]"
        return f"MethodDefinition(name='{self.name}', parameters={params}, body={body_str}, is_static={self.is_static}, is_constructor={self.is_constructor}, is_private={self.is_private})"

    def to_dict(self):
        base = super().to_dict()
        base["name"] = "init" if self.is_constructor else self.name
        base["parameters"] = _serialize_list(self.parameters)
        base["body"] = _serialize_list(self.body)
        base["is_static"] = self.is_static
        base["is_constructor"] = self.is_constructor
        base["is_private"] = self.is_private
        return base


class PropertyDefinition(Statement):
    def __init__(
        self,
        name,
        type_annotation,
        initializer,
        is_static,
        is_private,
        file,
        line,
        column,
    ):
        super().__init__("PropertyDefinition", file, line, column)
        self.name = name
        self.type_annotation = type_annotation
        self.initializer = initializer
        self.is_static = is_static
        self.is_private = is_private

    def __str__(self):
        type_str = (
            f", type_annotation={self.type_annotation}" if self.type_annotation else ""
        )
        init_str = f", initializer={self.initializer}" if self.initializer else ""
        return f"PropertyDefinition(name='{self.name}'{type_str}{init_str}, is_static={self.is_static}, is_private={self.is_private})"

    def to_dict(self):
        base = super().to_dict()
        base["name"] = self.name
        base["type_annotation"] = self.type_annotation
        base["initializer"] = (
            _serialize_node(self.initializer) if self.initializer else None
        )
        base["is_static"] = self.is_static
        base["is_private"] = self.is_private
        return base


class ExpressionStmt(Statement):
    def __init__(self, expression, file, line, column):
        super().__init__("ExpressionStmt", file, line, column)
        self.expression = expression

    def __str__(self):
        return f"ExpressionStmt(expression={self.expression})"

    def to_dict(self):
        base = super().to_dict()
        base["expression"] = _serialize_node(self.expression)
        return base


class BreakStmt(Statement):
    def __init__(self, file, line, column):
        super().__init__("BreakStmt", file, line, column)

    def __str__(self):
        return "BreakStmt()"


class ContinueStmt(Statement):
    def __init__(self, file, line, column):
        super().__init__("ContinueStmt", file, line, column)

    def __str__(self):
        return "ContinueStmt()"


class TryStmt(Statement):
    def __init__(self, try_block, error_var, catch_block, file, line, column):
        super().__init__("TryStmt", file, line, column)
        self.try_block = try_block
        self.error_var = error_var
        self.catch_block = catch_block

    def __str__(self):
        try_str = "[" + ", ".join(str(s) for s in self.try_block) + "]"
        catch_str = "[" + ", ".join(str(s) for s in self.catch_block) + "]"
        return f"TryStmt(try_block={try_str}, error_var='{self.error_var}', catch_block={catch_str})"

    def to_dict(self):
        base = super().to_dict()
        base["try_block"] = _serialize_list(self.try_block)
        base["error_var"] = self.error_var
        base["catch_block"] = _serialize_list(self.catch_block)
        return base


class IfStmt(Statement):
    def __init__(self, condition, then_block, else_block, else_if, file, line, column):
        super().__init__("IfStmt", file, line, column)
        self.condition = condition
        self.then_block = then_block
        self.else_block = else_block
        self.else_if = else_if  # list of {"condition": ..., "body": ...}

    def __str__(self):
        then_str = "[" + ", ".join(str(s) for s in self.then_block) + "]"
        else_if_str = ""
        if self.else_if:
            else_if_str = (
                ", else_if=["
                + ", ".join(
                    f"{{condition={b['condition']}, body=[{', '.join(str(s) for s in b['body'])}]}}"
                    for b in self.else_if
                )
                + "]"
            )
        else_str = ""
        if self.else_block:
            else_str = (
                ", else_block=[" + ", ".join(str(s) for s in self.else_block) + "]"
            )
        return f"IfStmt(condition={self.condition}, then_block={then_str}{else_if_str}{else_str})"

    def to_dict(self):
        base = super().to_dict()
        base["condition"] = _serialize_node(self.condition)
        base["then_block"] = _serialize_list(self.then_block)
        base["else_if"] = [
            {
                "condition": _serialize_node(b["condition"]),
                "body": _serialize_list(b["body"]),
            }
            for b in self.else_if
        ]
        base["else_block"] = (
            _serialize_list(self.else_block) if self.else_block else None
        )
        return base


class WhileStmt(Statement):
    def __init__(self, condition, body, file, line, column):
        super().__init__("WhileStmt", file, line, column)
        self.condition = condition
        self.body = body

    def __str__(self):
        body_str = "[" + ", ".join(str(s) for s in self.body) + "]"
        return f"WhileStmt(condition={self.condition}, body={body_str})"

    def to_dict(self):
        base = super().to_dict()
        base["condition"] = _serialize_node(self.condition)
        base["body"] = _serialize_list(self.body)
        return base


class UntilStmt(Statement):
    def __init__(self, condition, body, file, line, column):
        super().__init__("UntilStmt", file, line, column)
        self.condition = condition
        self.body = body

    def __str__(self):
        body_str = "[" + ", ".join(str(s) for s in self.body) + "]"
        return f"UntilStmt(condition={self.condition}, body={body_str})"

    def to_dict(self):
        base = super().to_dict()
        base["condition"] = _serialize_node(self.condition)
        base["body"] = _serialize_list(self.body)
        return base


class ForEachStmt(Statement):
    def __init__(self, variable, list, body, file, line, column):
        super().__init__("ForEachStmt", file, line, column)
        self.variable = variable
        self.list = list
        self.body = body

    def __str__(self):
        body_str = "[" + ", ".join(str(s) for s in self.body) + "]"
        return f"ForEachStmt(variable='{self.variable}', list={self.list}, body={body_str})"

    def to_dict(self):
        base = super().to_dict()
        base["variable"] = self.variable
        base["list"] = _serialize_node(self.list)
        base["body"] = _serialize_list(self.body)
        return base


class ForStmt(Statement):
    def __init__(self, variable, start, end, step, body, file, line, column):
        super().__init__("ForStmt", file, line, column)
        self.variable = variable
        self.start = start
        self.end = end
        self.step = step
        self.body = body

    def __str__(self):
        step_str = f", step={self.step}" if self.step else ""
        body_str = "[" + ", ".join(str(s) for s in self.body) + "]"
        return f"ForStmt(variable='{self.variable}', start={self.start}, end={self.end}{step_str}, body={body_str})"

    def to_dict(self):
        base = super().to_dict()
        base["variable"] = self.variable
        base["start"] = _serialize_node(self.start)
        base["end"] = _serialize_node(self.end)
        base["step"] = _serialize_node(self.step) if self.step else None
        base["body"] = _serialize_list(self.body)
        return base


class EnumDef(Expression):
    def __init__(self, values, file, line, column):
        super().__init__("EnumDef", file, line, column)
        self.values = values

    def __str__(self):
        return f"EnumDef(values={self.values})"

    def to_dict(self):
        base = super().to_dict()
        base["values"] = self.values
        return base


class ScopeStmt(Statement):
    def __init__(self, name, body, file, line, column):
        super().__init__("ScopeStmt", file, line, column)
        self.name = name
        self.body = body

    def __str__(self):
        body_str = "[" + ", ".join(str(s) for s in self.body) + "]"
        return f"ScopeStmt(name='{self.name}', body={body_str})"

    def to_dict(self):
        base = super().to_dict()
        base["name"] = self.name
        base["body"] = _serialize_list(self.body)
        return base


class SwitchStmt(Statement):
    def __init__(self, expression, cases, strict, file, line, column):
        super().__init__("SwitchStmt", file, line, column)
        self.expression = expression
        self.cases = cases
        self.strict = strict

    def __str__(self):
        cases_str = "[" + ", ".join(str(c) for c in self.cases) + "]"
        return f"SwitchStmt(expression={self.expression}, cases={cases_str}, strict={self.strict})"

    def to_dict(self):
        base = super().to_dict()
        base["expression"] = _serialize_node(self.expression)
        base["cases"] = [c.to_dict() for c in self.cases]
        base["strict"] = self.strict
        return base


class ReturnStmt(Statement):
    def __init__(self, expression, file, line, column):
        super().__init__("ReturnStmt", file, line, column)
        self.expression = expression

    def __str__(self):
        expr_str = f", expression={self.expression}" if self.expression else ""
        return f"ReturnStmt({expr_str})"

    def to_dict(self):
        base = super().to_dict()
        base["expression"] = (
            _serialize_node(self.expression) if self.expression else None
        )
        return base


class LocalFuncDecl(Statement):
    def __init__(self, fn, file, line, column):
        super().__init__("LocalFuncDecl", file, line, column)
        self.fn = fn

    def __str__(self):
        return f"LocalFuncDecl(fn={self.fn})"

    def to_dict(self):
        base = super().to_dict()
        base["function"] = _serialize_node(self.fn)
        return base


class FuncDecl(Statement):
    def __init__(self, name, parameters, body, file, line, column):
        super().__init__("FuncDecl", file, line, column)
        self.name = name
        self.parameters = parameters
        self.body = body

    def __str__(self):
        params = "[" + ", ".join(str(p) for p in self.parameters) + "]"
        body_str = "[" + ", ".join(str(s) for s in self.body) + "]"
        return f"FuncDecl(name='{self.name}', parameters={params}, body={body_str})"

    def to_dict(self):
        base = super().to_dict()
        base["name"] = self.name
        base["parameters"] = _serialize_list(self.parameters)
        base["body"] = _serialize_list(self.body)
        return base


class WithStmt(Statement):
    def __init__(self, expr, alias, body, file, line, column):
        super().__init__("WithStmt", file, line, column)
        self.expr = expr
        self.alias = alias
        self.body = body

    def __str__(self):
        alias_str = f", alias='{self.alias}'" if self.alias else ""
        body_str = "[" + ", ".join(str(s) for s in self.body) + "]"
        return f"WithStmt(expr={self.expr}{alias_str}, body={body_str})"

    def to_dict(self):
        base = super().to_dict()
        base["expression"] = _serialize_node(self.expr)
        base["alias"] = self.alias
        base["body"] = _serialize_list(self.body)
        return base


class LambdaDecl(Expression):
    def __init__(self, parameters, body, file, line, column):
        super().__init__("LambdaDecl", file, line, column)
        self.parameters = parameters
        self.body = body

    def __str__(self):
        params = "[" + ", ".join(str(p) for p in self.parameters) + "]"
        body_str = "[" + ", ".join(str(s) for s in self.body) + "]"
        return f"LambdaDecl(parameters={params}, body={body_str})"

    def to_dict(self):
        base = super().to_dict()
        base["parameters"] = _serialize_list(self.parameters)
        base["body"] = _serialize_list(self.body)
        return base


class UsingStmt(Statement):
    def __init__(self, name, file, line, column):
        super().__init__("UsingStmt", file, line, column)
        self.name = name

    def __str__(self):
        if isinstance(self.name, list):
            names = "[" + ", ".join(f"'{n}'" for n in self.name) + "]"
            return f"UsingStmt(names={names})"
        else:
            return f"UsingStmt(name='{self.name}')"

    def to_dict(self):
        base = super().to_dict()
        if isinstance(self.name, list):
            base["names"] = self.name
        else:
            base["name"] = self.name
        return base
