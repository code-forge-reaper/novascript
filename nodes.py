# --- Control Flow Classes ---
class ControlFlow:
    """Base class for control flow signals"""

    pass


class ReturnFlow(ControlFlow):
    def __init__(self, value):
        self.value = value


class BreakFlow(ControlFlow):
    pass


class ContinueFlow(ControlFlow):
    pass


class NovaError(Exception):
    """Custom error class for NovaScript, including location information."""

    def __init__(self, token, user_message=None):
        # Provide fallback values if token is None or lacks properties
        file = getattr(
            token, "file", "unknown_file") if token else "unknown_file"
        line = getattr(token, "line", 0) if token else 0
        column = getattr(token, "column", 0) if token else 0
        token_value = getattr(token, "value", "N/A") if token else "N/A"

        self.message = (f"{file}:{line}:{column}" +
                        f": {user_message or 'unknown error at token: ' + str(token_value)}")
        super().__init__(self.message)
        self.line = line
        self.column = column
        self.file = file


# Helper for indentation in __str__ methods
INDENT_STEP = "  "


# --- AST Node Base Classes ---
class Token:
    def __init__(self, type, value, file, line, column):
        self.type = type
        self.value = value
        self.file = file
        self.line = line
        self.column = column

    def __str__(self, indent_level=0):
        # Generic representation for debugging, concrete nodes will override for source reconstruction
        return f"Token(type='{self.type}', value={repr(self.value)}, file='{self.file}', line={self.line}, column={self.column})"


class Statement(Token):
    def __init__(self, type, file, line, column):
        super().__init__(type, None, file, line, column)

    def __str__(self, indent_level=0):
        # Generic statement, concrete statements will override
        return f"{INDENT_STEP * indent_level}[{self.__class__.__name__} Statement]"


class Expression(Token):
    def __init__(self, type, file, line, column):
        super().__init__(type, None, file, line, column)

    def __str__(self, indent_level=0):
        # Generic expression, concrete expressions will override
        return f"[{self.__class__.__name__} Expression]"


class ExplodeExpr(Expression):
    def __init__(self, args, file, line, col):
        super().__init__("ExplodeExpr", file, line, col)
        self.args = args

    def __str__(self, indent=0):
        return f"*[{self.args}]"


class DecoratorExpr(Expression):
    def __init__(self, expr, body, file, line, column):
        super().__init__("DecoratorExpr", file, line, column)
        self.expr = expr
        self.body = body

    def __str__(self, indent_level=0):
        current_indent = INDENT_STEP * indent_level
        body_str = self.body.__str__(indent_level)
        return f"{current_indent}@{self.expr.__str__(indent_level)} {body_str}"


# --- Specific AST Node Classes ---
class Literal(Expression):
    def __init__(self, value, file, line, column):
        super().__init__("Literal", file, line, column)
        self.value = value

    def __str__(self, indent_level=0):
        if isinstance(self.value, str):
            return f'"{self.value}"'
        if self.value is None:
            return "null"  # Assuming 'null' for None in NovaScript
        if isinstance(self.value, bool):
            return "true" if self.value else "false"
        return str(self.value)


class Identifier(Expression):
    def __init__(self, name, file, line, column):
        super().__init__("Identifier", file, line, column)
        self.name = name

    def __str__(self, indent_level=0):
        return self.name


class BinaryExpr(Expression):
    def __init__(self, operator, left, right, file, line, column):
        super().__init__("BinaryExpr", file, line, column)
        self.operator = operator
        self.left = left
        self.right = right

    def __str__(self, indent_level=0):
        # Add parentheses to ensure correct precedence during reconstruction
        return f"({self.left.__str__(indent_level)} {self.operator} {self.right.__str__(indent_level)})"


class PipeExpr(BinaryExpr):
    def __init__(self, left, right, file, line, column):
        super().__init__("->", left, right, file, line, column)


class MapExpr(BinaryExpr):
    def __init__(self, left, right, file, line, column):
        super().__init__("=>", left, right, file, line, column)


class UnaryExpr(Expression):
    def __init__(self, operator, right, file, line, column):
        super().__init__("UnaryExpr", file, line, column)
        self.operator = operator
        self.right = right

    def __str__(self, indent_level=0):
        return f"{self.operator}{self.right.__str__(indent_level)}"


class FuncCall(Expression):
    def __init__(self, name, arguments, file, line, column):
        super().__init__("FuncCall", file, line, column)
        self.name = name
        self.arguments = arguments

    def __str__(self, indent_level=0):
        args_str = ", ".join(arg.__str__(indent_level)
                             for arg in self.arguments)
        return f"{self.name}({args_str})"


class MethodCall(Expression):
    def __init__(self, object, method, arguments, file, line, column):
        super().__init__("MethodCall", file, line, column)
        self.object = object
        self.method = method
        self.arguments = arguments

    def __str__(self, indent_level=0):
        args_str = ", ".join(arg.__str__(indent_level)
                             for arg in self.arguments)
        return f"{self.object.__str__(indent_level)}.{self.method}({args_str})"


class PropertyAccess(Expression):
    def __init__(self, object, property, file, line, column):
        super().__init__("PropertyAccess", file, line, column)
        self.object = object
        self.property = property

    def __str__(self, indent_level=0):
        return f"{self.object.__str__(indent_level)}.{self.property}"


class ArrayAccess(Expression):
    def __init__(self, object, index, file, line, column):
        super().__init__("ArrayAccess", file, line, column)
        self.object = object
        self.index = index

    def __str__(self, indent_level=0):
        return (
            f"{self.object.__str__(indent_level)}[{
                self.index.__str__(indent_level)}]"
        )


class ArrayLiteral(Expression):
    def __init__(self, elements, file, line, column):
        super().__init__("ArrayLiteral", file, line, column)
        self.elements = elements

    def __str__(self, indent_level=0):
        elements_str = ", ".join(elem.__str__(indent_level)
                                 for elem in self.elements)
        return f"[{elements_str}]"


class ObjectLiteral(Expression):
    def __init__(self, properties, file, line, column):
        super().__init__("ObjectLiteral", file, line, column)
        self.properties = properties

    def __str__(self, indent_level=0):
        current_indent = INDENT_STEP * indent_level
        next_indent = INDENT_STEP * (indent_level + 1)

        if not self.properties:
            return "{}"

        props_list = []
        for prop in self.properties:
            props_list.append(
                f"{next_indent}{prop['key']}: {
                    prop['value'].__str__(indent_level + 1)}"
            )

        # build the joined string OUTSIDE the f-string expression
        joined = ",\n".join(props_list)

        return f"{{\n{joined}\n{current_indent}}}"


class AssignmentExpr(Expression):
    def __init__(self, target, value, operator, file, line, column):
        super().__init__("AssignmentExpr", file, line, column)
        self.target = target
        self.value = value
        self.operator = operator

    def __str__(self, indent_level=0):
        return f"{self.target.__str__(indent_level)} {self.operator} {self.value.__str__(indent_level)}"


class NewInstance(Expression):
    # Changed class_target_expr back to class_name to hold a string path
    def __init__(self, class_name, arguments, file, line, column):
        super().__init__("NewInstance", file, line, column)
        self.class_name = class_name
        self.arguments = arguments

    def __str__(self, indent_level=0):
        args_str = ", ".join(arg.__str__(indent_level)
                             for arg in self.arguments)
        return f"new {self.class_name}({args_str})"


class Parameter(Token):
    def __init__(self, name, annotation_type, default, file, line, column, type, value):
        super().__init__(type, value, file, line, column)
        self.name = name
        self.annotation_type = annotation_type
        self.default = default

    def __str__(self, indent_level=0):
        param_str = self.name
        if self.annotation_type:
            param_str += f": {self.annotation_type}"
        if self.default:
            param_str += f" = {self.default.__str__(indent_level)}"
        return param_str


class Case:
    def __init__(self, case_expr, body):
        self.case_expr = case_expr
        self.body = body

    def __str__(self, indent_level=0):
        current_indent = INDENT_STEP * indent_level
        body_str = "\n".join(stmt.__str__(indent_level + 1)
                             for stmt in self.body)
        if self.case_expr:
            return f"{current_indent}case {self.case_expr.__str__(indent_level)} do\n{body_str}\n{current_indent}end"
        else:  # Default case
            return f"{current_indent}default do\n{body_str}\n{current_indent}end"


class ExportStmt(Statement):
    def __init__(self, expr, file, line, column):
        super().__init__("ExportStmt", file, line, column)
        self.expr = expr

    def __str__(self, indent_level=0):
        current_indent = INDENT_STEP * indent_level
        expr = self.expr.__str__(indent_level + 1)
        return f"{current_indent}export {expr}"


class DeferStmt(Statement):
    def __init__(self, body, file, line, column):
        super().__init__("DeferStmt", file, line, column)
        self.body = body

    def __str__(self, indent_level=0):
        current_indent = INDENT_STEP * indent_level
        body_str = "\n".join(stmt.__str__(indent_level + 1)
                             for stmt in self.body)
        return f"{current_indent}defer do\n{body_str}\n{current_indent}end"


class VarDeclStmt(Statement):
    def __init__(self, name, type_annotation, initializer, file, line, column):
        super().__init__("VarDecl", file, line, column)
        self.name = name
        self.type_annotation = type_annotation
        self.initializer = initializer

    def __str__(self, indent_level=0):
        current_indent = INDENT_STEP * indent_level
        var_str = f"var {self.name}"
        if self.type_annotation:
            var_str += f": {self.type_annotation}"
        var_str += f" = {self.initializer.__str__(indent_level)}"
        return f"{current_indent}{var_str}"


class ConstDeclStmt(Statement):
    def __init__(self, name, type_annotation, initializer, file, line, column):
        super().__init__("ConstDecl", file, line, column)
        self.name = name
        self.type_annotation = type_annotation
        self.initializer = initializer

    def __str__(self, indent_level=0):
        current_indent = INDENT_STEP * indent_level
        var_str = f"const {self.name}"
        if self.type_annotation:
            var_str += f": {self.type_annotation}"
        var_str += f" = {self.initializer.__str__(indent_level)}"
        return f"{current_indent}{var_str}"


class CustomTypeProperty:
    def __init__(self, name, type):
        self.name = name
        self.type = type

    def __str__(self, indent_level=0):
        return f"{INDENT_STEP * indent_level}{self.name}: {self.type}"


class CustomType:  # This is a runtime representation, not an AST node directly
    def __init__(self, name, properties, file, line, column):
        self.name = name
        self.properties = properties
        self.file = file
        self.line = line
        self.column = column

    def __str__(self, indent_level=0):
        current_indent = INDENT_STEP * indent_level
        props_str = ",\n".join(
            prop.__str__(indent_level + 1) for prop in self.properties
        )
        return (
            f"{current_indent}define {
                self.name} = {{\n{props_str}\n{current_indent}}}"
        )


class CustomTypeDeclStmt(Statement):
    def __init__(self, name, definition, file, line, column):
        super().__init__("CustomTypeDecl", file, line, column)
        self.name = name
        self.definition = definition  # List of CustomTypeProperty objects

    def __str__(self, indent_level=0):
        current_indent = INDENT_STEP * indent_level
        props_str = ",\n".join(
            prop.__str__(indent_level + 1) for prop in self.definition
        )
        return (
            f"{current_indent}define {
                self.name} = {{\n{props_str}\n{current_indent}}}"
        )


class AssertStmt(Statement):
    def __init__(self, expression, message, file, line, column):
        super().__init__("AssertStmt", file, line, column)
        self.expression = expression
        self.message = message

    def __str__(self, indent_level=0):
        current_indent = INDENT_STEP * indent_level
        msg_str = f' "{self.message}"' if self.message else ""
        return (
            f"{current_indent}assert {self.expression.__str__(indent_level)}{
                msg_str}"
        )


class ClassDefinition(Statement):
    def __init__(self, name, superclass_name, body, file, line, column):
        super().__init__("ClassDefinition", file, line, column)
        self.name = name
        self.superclass_name = superclass_name
        self.body = body

    def __str__(self, indent_level=0):
        current_indent = INDENT_STEP * indent_level
        next_indent = INDENT_STEP * (indent_level + 1)
        class_str = f"{current_indent}class {self.name}"
        if self.superclass_name:
            class_str += f" inherits {self.superclass_name}"
        class_str += " do\n"
        for member in self.body:
            class_str += member.__str__(indent_level + 1) + "\n"
        class_str += f"{current_indent}end"
        return class_str


# Add this class along with ClassDecl
class ObjectDecl(Statement):
    def __init__(self, name, body, file, line, column):
        super().__init__("ObjectDecl", file, line, column)
        self.name = name
        self.body = body

    def __str__(self, indent_level=0):
        current_indent = INDENT_STEP * indent_level

        body_str = "\n".join(stmt.__str__(indent_level + 1)
                             for stmt in self.body)
        return f"{current_indent}object {self.name}\n{body_str}\n{current_indent}end"


class MethodDefinition(Statement):
    def __init__(
        self, name, parameters, body, is_static, is_constructor, file, line, column
    ):
        super().__init__("MethodDefinition", file, line, column)
        self.name = name
        self.parameters = parameters
        self.body = body
        self.is_static = is_static
        self.is_constructor = is_constructor

    def __str__(self, indent_level=0):
        current_indent = INDENT_STEP * indent_level
        params_str = ", ".join(p.__str__(indent_level)
                               for p in self.parameters)
        body_str = "\n".join(stmt.__str__(indent_level + 1)
                             for stmt in self.body)
        static_prefix = "static " if self.is_static else ""
        name = "init" if self.is_constructor else self.name
        return f"{current_indent}{static_prefix}func {name}({params_str}) \n{body_str}\n{current_indent}end"


class PropertyDefinition(Statement):
    def __init__(
        self, name, type_annotation, initializer, is_static, file, line, column
    ):
        super().__init__("PropertyDefinition", file, line, column)
        self.name = name
        self.type_annotation = type_annotation
        self.initializer = initializer
        self.is_static = is_static

    def __str__(self, indent_level=0):
        current_indent = INDENT_STEP * indent_level
        static_prefix = "static " if self.is_static else ""
        prop_str = f"{current_indent}{static_prefix}var {self.name}"
        if self.type_annotation:
            prop_str += f": {self.type_annotation}"
        if self.initializer:
            prop_str += f" = {self.initializer.__str__(indent_level)}"
        return prop_str


class ExpressionStmt(Statement):
    def __init__(self, expression, file, line, column):
        super().__init__("ExpressionStmt", file, line, column)
        self.expression = expression

    def __str__(self, indent_level=0):
        current_indent = INDENT_STEP * indent_level
        return f"{current_indent}{self.expression.__str__(indent_level)}"


class BreakStmt(Statement):
    def __init__(self, file, line, column):
        super().__init__("BreakStmt", file, line, column)

    def __str__(self, indent_level=0):
        return f"{INDENT_STEP * indent_level}break"


class ContinueStmt(Statement):
    def __init__(self, file, line, column):
        super().__init__("ContinueStmt", file, line, column)

    def __str__(self, indent_level=0):
        return f"{INDENT_STEP * indent_level}continue"


class TryStmt(Statement):
    def __init__(self, try_block, error_var, catch_block, file, line, column):
        super().__init__("TryStmt", file, line, column)
        self.try_block = try_block
        self.error_var = error_var
        self.catch_block = catch_block

    def __str__(self, indent_level=0):
        current_indent = INDENT_STEP * indent_level
        try_body_str = "\n".join(
            stmt.__str__(indent_level + 1) for stmt in self.try_block
        )
        catch_body_str = "\n".join(
            stmt.__str__(indent_level + 1) for stmt in self.catch_block
        )
        return (
            f"{current_indent}test do\n"
            f"{try_body_str}\n"
            f"{current_indent}failed {self.error_var} do\n"
            f"{catch_body_str}\n"
            f"{current_indent}end"
        )


class IfStmt(Statement):
    def __init__(self, condition, then_block, else_block, else_if, file, line, column):
        super().__init__("IfStmt", file, line, column)
        self.condition = condition
        self.then_block = then_block
        self.else_block = else_block
        self.else_if = else_if

    def __str__(self, indent_level=0):
        current_indent = INDENT_STEP * indent_level
        s = f"{current_indent}if {self.condition.__str__(indent_level)} do\n"
        s += "\n".join(stmt.__str__(indent_level + 1)
                       for stmt in self.then_block)

        if self.else_if:
            for elseif_block in self.else_if:
                s += f"\n{current_indent}elseif {
                    elseif_block['condition'].__str__(indent_level)} do\n"
                s += "\n".join(
                    stmt.__str__(indent_level + 1) for stmt in elseif_block["body"]
                )

        if self.else_block:
            s += f"\n{current_indent}else do\n"
            s += "\n".join(stmt.__str__(indent_level + 1)
                           for stmt in self.else_block)

        s += f"\n{current_indent}end"
        return s


class WhileStmt(Statement):
    def __init__(self, condition, body, file, line, column):
        super().__init__("WhileStmt", file, line, column)
        self.condition = condition
        self.body = body

    def __str__(self, indent_level=0):
        current_indent = INDENT_STEP * indent_level
        body_str = "\n".join(stmt.__str__(indent_level + 1)
                             for stmt in self.body)
        return f"{current_indent}while {self.condition.__str__(indent_level)}\n{body_str}\n{current_indent}end"


class UntilStmt(Statement):
    def __init__(self, condition, body, file, line, column):
        super().__init__("UntilStmt", file, line, column)
        self.condition = condition
        self.body = body

    def __str__(self, indent_level=0):
        current_indent = INDENT_STEP * indent_level
        body_str = "\n".join(stmt.__str__(indent_level + 1)
                             for stmt in self.body)
        return f"{current_indent}until {self.condition.__str__(indent_level)}\n{body_str}\n{current_indent}end"


class ForEachStmt(Statement):
    def __init__(self, variable, list, body, file, line, column):
        super().__init__("ForEachStmt", file, line, column)
        self.variable = variable
        self.list = list
        self.body = body

    def __str__(self, indent_level=0):
        current_indent = INDENT_STEP * indent_level
        body_str = "\n".join(stmt.__str__(indent_level + 1)
                             for stmt in self.body)
        return f"{current_indent}forEach {self.variable} in {self.list.__str__(indent_level)} do\n{body_str}\n{current_indent}end"


# wraps "@thing\ndef _():..."


class ForStmt(Statement):
    def __init__(self, variable, start, end, step, body, file, line, column):
        super().__init__("ForStmt", file, line, column)
        self.variable = variable
        self.start = start
        self.end = end
        self.step = step
        self.body = body

    def __str__(self, indent_level=0):
        current_indent = INDENT_STEP * indent_level
        step_str = f", {self.step.__str__(indent_level)}" if self.step else ""
        body_str = "\n".join(stmt.__str__(indent_level + 1)
                             for stmt in self.body)
        return (
            f"{current_indent}for {self.variable} = {
                self.start.__str__(indent_level)}, "
            f"{self.end.__str__(indent_level)}{step_str} do\n"
            f"{body_str}\n"
            f"{current_indent}end"
        )


class EnumDef(Statement):
    def __init__(self, name, values, file, line, column):
        super().__init__("EnumDef", file, line, column)
        self.values = values
        self.name = name

    def __str__(self, indent_level=0):
        current_indent = INDENT_STEP * indent_level
        next_indent = INDENT_STEP * (indent_level + 1)
        values_str = ",\n".join(
            f"{next_indent}{value}" for value in self.values)
        return f"{current_indent}enum {self.name} {{\n{values_str}\n{current_indent}}}"


class ScopeStmt(Statement):
    def __init__(self, name, body, file, line, column):
        super().__init__("ScopeStmt", file, line, column)
        self.name = name
        self.body = body

    def __str__(self, indent_level=0):
        current_indent = INDENT_STEP * indent_level
        body_str = "\n".join(stmt.__str__(indent_level + 1)
                             for stmt in self.body)
        return f"{current_indent}scope {self.name} do\n{body_str}\n{current_indent}end"


class SwitchStmt(Statement):
    def __init__(self, expression, cases, strict: bool, file, line, column):
        super().__init__("SwitchStmt", file, line, column)
        self.expression = expression
        self.cases = cases
        self.strict = strict

    def __str__(self, indent_level=0):
        current_indent = INDENT_STEP * indent_level
        cases_str = "\n".join(case.__str__(indent_level + 1)
                              for case in self.cases)
        if self.strict:
            return f"{current_indent}switch strict {self.expression.__str__(indent_level)} do\n{cases_str}\n{current_indent}end"
        return f"{current_indent}switch {self.expression.__str__(indent_level)} do\n{cases_str}\n{current_indent}end"


class ReturnStmt(Statement):
    def __init__(self, expression, file, line, column):
        super().__init__("ReturnStmt", file, line, column)
        self.expression = expression

    def __str__(self, indent_level=0):
        current_indent = INDENT_STEP * indent_level
        expr_str = (
            f" {self.expression.__str__(
                indent_level)}" if self.expression else ""
        )
        return f"{current_indent}return{expr_str}"


class LocalFuncDecl(Statement):
    def __init__(self, fn, file, line, column):
        super().__init__("LocalFuncDecl", file, line, column)
        self.fn = fn

    def __str__(self, indent_level=0):
        current_indent = INDENT_STEP * indent_level
        return f"{current_indent}local {self.fn.__str__(indent_level)}"


class FuncDecl(Statement):
    def __init__(self, name, parameters, body, file, line, column):
        super().__init__("FuncDecl", file, line, column)
        self.name = name
        self.parameters = parameters
        self.body = body

    def __str__(self, indent_level=0):
        current_indent = INDENT_STEP * indent_level
        params_str = ", ".join(p.__str__(indent_level)
                               for p in self.parameters)
        body_str = "\n".join(stmt.__str__(indent_level + 1)
                             for stmt in self.body)
        return f"{current_indent}func {self.name}({params_str}) do\n{body_str}\n{current_indent}end"


class WithStmt(Statement):
    def __init__(self, expr, alias, body, file, line, column):
        super().__init__("WithStmt", file, line, column)
        self.expr = expr
        self.alias = alias
        self.body = body

    def __str__(self, indent_level=0):
        current_indent = INDENT_STEP * indent_level
        body_str = "\n".join(stmt.__str__(indent_level + 1)
                             for stmt in self.body)
        return f"{current_indent}with {self.expr.__str__(indent_level)} as {self.alias} do\n{body_str}\n{current_indent}end"


class LambdaDecl(
    Expression
):  # LambdaDecl is an expression that evaluates to a function
    def __init__(self, parameters, body, file, line, column):
        super().__init__("LambdaDecl", file, line, column)
        self.parameters = parameters
        self.body = body

    def __str__(self, indent_level=0):
        params_str = ", ".join(p.__str__(indent_level)
                               for p in self.parameters)
        body_str = "\n".join(stmt.__str__(indent_level + 1)
                             for stmt in self.body)
        # Lambda is an expression, so it doesn't get the outer indent, but its body does.
        return f"def ({params_str}) do\n{body_str}\n{INDENT_STEP * indent_level}end"


class UsingStmt(Statement):
    def __init__(self, name, file, line, column):
        super().__init__("UsingStmt", file, line, column)
        self.name = name

    def __str__(self, indent_level=0):
        current_indent = INDENT_STEP * indent_level
        if isinstance(self.name, list):
            names_str = ", ".join(self.name)
            return f"{current_indent}using [{names_str}]"
        else:
            return f"{current_indent}using {self.name}"
