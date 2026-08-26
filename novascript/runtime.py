#!/usr/bin/env python
from .novaclass import bind_parameters
from .novaenv import FuncWrapp
from .typechecker import CUSTOM_TYPES
from .typechecker import check_type
from .tokenizer import Tokenizer
from .novaenv import Environment
from .novaclass import NovaClass
from .helpers import Proxy, dprint
from .nodes import *
import os
import sys
import pathlib
from . import conf


import uuid
from collections.abc import Iterable, Callable

import copy


__no_variable_set__ = object()


def opToName(op):
    def up(st):
        s = list(st)
        s[0] = s[0].upper()
        return "__" + "".join(s)

    matches = {
        "+": up("add"),
        "-=": up("setDec"),
        "+=": up("setInc"),
        "*=": up("setMul"),
        "-": up("sub"),
        "*": up("mul"),
        "/": up("div"),
        "%": up("mod"),
        "^": up("xor"),
        "<<": up("lshift"),
        ">>": up("rshift"),
        "==": up("eq"),
        "!=": up("ne"),
        "<": up("lt"),
        ">": up("gt"),
        "<=": up("le"),
        ">=": up("ge"),
    }
    return matches.get(op, None)


class Interpreter:
    def __init__(self, source, file_path):
        self.source = source
        c = pathlib.Path(file_path)
        self.file = c.expanduser().resolve()
        # self.file = os.path.abspath(file_path)
        self.tk = Tokenizer(source, file_path)
        self.globals = Environment()

        self.modules_loaded = {}
        self.current_env = None

        self.globals.define(
            "__SCRIPT_PATH__", os.path.dirname(os.path.abspath(self.file))
        )
        self.globals.define("__SCRIPT_NAME__", file_path)
        self.globals.define("__IS_MAIN__", True)
        self.current_class_stack = []
        self.callStack = []
        self.errorStack = []

        # ------------------------------------------------------------------
        # Extensibility registries
        # ------------------------------------------------------------------
        # To add a new statement type:
        #   1. Add the AST node in nodes.py
        #   2. Implement a method `_stmt_YourType(self, stmt, env)`
        #   3. Register it here: self._stmt_handlers["YourType"] = self._stmt_YourType
        # Same pattern for expressions with `_expr_handlers`.
        #
        # The big if/elif chains remain for now for backward compatibility, but
        # new features should prefer the registry path so the core dispatch stays
        # clean and easy to maintain.
        self._stmt_handlers = {}
        self._expr_handlers = {}
        self._register_default_handlers()

    def _register_default_handlers(self):
        """Populate the extensibility registries. Override or extend in subclasses."""
        # Statement handlers (examples – full coverage still lives in execute_stmt)
        # self._stmt_handlers["VarDecl"] = self._stmt_VarDecl
        # ...
        # Expression handlers
        # self._expr_handlers["Literal"] = self._expr_Literal
        pass

    def _create_callable(
        self, parameters, body, env, name=None, node=None, context_name=None
    ):
        """
        Shared factory for both named functions (FuncDecl) and lambdas (LambdaDecl).
        A lambda is simply an unnamed / uuid-named function.

        - Captures the current class context (if any) so nested lambdas can still
          touch private members of the enclosing class.
        - Uses the common bind_parameters helper.
        - Attaches __nova_ast__ so dynamic method binding continues to work.
        """
        captured_class = (
            self.current_class_stack[-1] if self.current_class_stack else None
        )
        display_name = name or "lambda"
        ctx = context_name or display_name

        def func_wrapper(*args, **kwargs):
            pushed = False
            if captured_class is not None:
                self.current_class_stack.append(captured_class)
                pushed = True
            try:
                func_env = Environment(env)
                bound = bind_parameters(
                    parameters,
                    args,
                    kwargs,
                    self,
                    env,
                    context_name=ctx,
                    token=node,
                )
                for pname, pval in bound.items():
                    func_env.define(pname, pval)
                result = self.execute_block(body, func_env)
                if isinstance(result, ReturnFlow):
                    return result.value
                return None
            finally:
                if pushed and self.current_class_stack:
                    self.current_class_stack.pop()

        wrapper = FuncWrapp(
            func_wrapper,
            (node.__repr__ if node else None),
            (node.__str__ if node else None),
            node,
            name=display_name,
        )
        wrapper.__name__ = name if name is not None else str(uuid.uuid4())
        if node is not None:
            wrapper.__nova_ast__ = node
        if captured_class is not None:
            wrapper.__DefiningClass = captured_class
        return wrapper

    def _check_private_access(self, obj, name, expr):
        """Raise NovaError if `name` is a private member of `obj` and access is not from inside its class."""
        if not isinstance(obj, dict) or "__DefiningClass" not in obj:
            return  # not a NovaScript instance
        cls = obj["__DefiningClass"]
        if name in cls._private_properties or name in cls._private_methods:
            # allowed only if the current class (top of stack) is exactly this class
            if not self.current_class_stack or self.current_class_stack[-1] is not cls:
                raise NovaError(
                    expr,
                    f"Private member '{name}' cannot be accessed outside its class",
                )

    # --- Evaluation / Execution ---
    def interpret(self):
        statements = self.tk.parse_block()
        try:
            self.execute_block(statements, self.globals)
        except Exception as err:
            # Ensure deferred statements still execute
            self.globals.execute_deferred(self)
            print("\n".join(reversed(self.errorStack)))
            if isinstance(err, NovaError):
                print(f"{err.message}", file=sys.stderr)
            else:
                print(
                    f"{err.__class__.__name__}:{err}"
                )  # trying to print the name and what caused it
            exit(1)

    def execute_block(self, statements, env):
        previous_env = self.current_env
        self.current_env = env
        try:
            for stmt in statements:
                result = self.execute_stmt(stmt, env)
                if isinstance(result, ControlFlow):
                    return result
            return None
        finally:
            # Execute deferred statements in reverse order
            env.execute_deferred(self)
            self.current_env = previous_env

    def get_current_context(self):
        return self.current_env

    def execute_stmt(self, stmt, env):
        dprint("stmt: ", stmt)

        # Prefer registered handler when present (makes new features easy to plug in)
        handler = self._stmt_handlers.get(stmt.type)
        if handler is not None:
            return handler(stmt, env)

        if stmt.type in ["VarDecl", "ConstDecl"]:
            value = self.evaluate_expr(stmt.initializer, env)
            if stmt.type_annotation:
                check_type(stmt.type_annotation, value, stmt)
            env.define(stmt.name, value, stmt.type == "ConstDecl", stmt.type_annotation)

        elif stmt.type == "ExportStmt":
            # Execute the inner statement first
            self.execute_stmt(stmt.expr, env)

            # Ensure exports dictionary exists (it should in modules, but create if missing)
            if not env.has("exports"):
                env.define("exports", {})
            exports_dict = env.get("exports").value

            inner = stmt.expr
            if inner.type in ("VarDecl", "ConstDecl", "FuncDecl", "ClassDefinition"):
                # For declarations, export the declared name
                name = inner.name
                if name in exports_dict:
                    raise NovaError(stmt, "cannot re-export: {}".format(name))
                value = env.get(name).value
                exports_dict[name] = value

            elif inner.type == "ExpressionStmt":
                expr = inner.expression
                if expr.type == "ObjectLiteral":
                    # export { a, b, c: d } → evaluate each property
                    for prop in expr.properties:
                        key = prop["key"]
                        val_expr = prop["value"]
                        if key in exports_dict:
                            raise NovaError(stmt, "cannot re-export: {}".format(key))
                        if val_expr.type == "Identifier":
                            val = env.get(val_expr.name).value
                        else:
                            val = self.evaluate_expr(val_expr, env)
                        exports_dict[key] = val
                elif expr.type == "Identifier":
                    # export a → export the variable 'a'
                    name = expr.name
                    if name in exports_dict:
                        raise NovaError(stmt, "cannot re-export: {}".format(name))
                    val = env.get(name).value
                    exports_dict[name] = val
                else:
                    raise NovaError(
                        stmt, f"Cannot export expression of type {expr.type}"
                    )

            else:
                raise NovaError(stmt, f"Cannot export statement of type {inner.type}")
        elif stmt.type == "DeferStmt":
            # To match TypeScript's behavior: statements within a single defer block
            # are executed in FIFO order. Since `execute_deferred` pops from the end
            # of the list, we must add them in reverse order here.
            reversed_stmts = list(reversed(stmt.body))
            for s in reversed_stmts:
                env.add_deferred(s)

        elif stmt.type == "ExpressionStmt":
            self.evaluate_expr(stmt.expression, env)

        elif stmt.type == "BreakStmt":
            return BreakFlow()

        elif stmt.type == "ContinueStmt":
            return ContinueFlow()

        elif stmt.type == "TryStmt":
            try:
                result = self.execute_block(stmt.try_block, env)
                if isinstance(result, ControlFlow) and not isinstance(
                    result, ReturnFlow
                ):
                    return result
            except Exception as e:  # Catch all other Python exceptions
                catch_env = Environment(env)
                # If e is a NovaError, use it directly. Otherwise, wrap it.
                catch_env.define(stmt.error_var, e, True)
                result = self.execute_block(stmt.catch_block, catch_env)
                if isinstance(result, ControlFlow):
                    return result
        elif stmt.type == "WithStmt":
            nenv = Environment(env)
            expression_val = self.evaluate_expr(stmt.expr, env)
            if getattr(expression_val, "__enter__", None):
                var = expression_val.__enter__()
            elif isinstance(expression_val, dict) and "__enter__" in expression_val:
                var = expression_val["__enter__"]()
            else:
                var = expression_val

            nenv.define(stmt.alias, var)
            try:
                result = self.execute_block(stmt.body, nenv)
                if isinstance(result, ControlFlow):
                    return result
            except Exception as e:
                handled = False
                if getattr(expression_val, "__exit__", None):
                    handled = expression_val.__exit__(type(e), e, e.__traceback__)
                elif isinstance(expression_val, dict) and "__exit__" in expression_val:
                    handled = expression_val["__exit__"](type(e), e, e.__traceback__)

                if not handled:
                    raise
            else:
                if getattr(expression_val, "__exit__", None):
                    expression_val.__exit__(None, None, None)
                elif isinstance(expression_val, dict) and "__exit__" in expression_val:
                    expression_val["__exit__"](None, None, None)

        elif stmt.type == "IfStmt":
            condition = self.evaluate_expr(stmt.condition, env)
            if condition:
                result = self.execute_block(stmt.then_block, Environment(env))
                if isinstance(result, ControlFlow):
                    return result
            elif stmt.else_if:
                matched = False
                for elseif_block in stmt.else_if:
                    elseif_condition = self.evaluate_expr(
                        elseif_block["condition"], env
                    )
                    if elseif_condition:
                        result = self.execute_block(
                            elseif_block["body"], Environment(env)
                        )
                        if isinstance(result, ControlFlow):
                            return result
                        matched = True
                        break
                if not matched and stmt.else_block:
                    result = self.execute_block(stmt.else_block, Environment(env))
                    if isinstance(result, ControlFlow):
                        return result
            elif stmt.else_block:
                result = self.execute_block(stmt.else_block, Environment(env))
                if isinstance(result, ControlFlow):
                    return result

        elif stmt.type == "ObjectDecl":
            temp_class = NovaClass(f"__anon_{stmt.name}", None, self, env)

            for member in stmt.body:
                if member.type == "FuncDecl":
                    if member.name == "init":
                        method_def = MethodDefinition(
                            member.name,
                            member.parameters,
                            member.body,
                            False,
                            True,
                            False,
                            member.file,
                            member.line,
                            member.column,
                        )
                        temp_class.constructor_def = method_def

                    else:
                        method_def = MethodDefinition(
                            member.name,
                            member.parameters,
                            member.body,
                            False,
                            False,
                            False,
                            member.file,
                            member.line,
                            member.column,
                        )
                        temp_class.instance_methods[member.name] = method_def
                elif member.type == "VarDecl":
                    # Treat 'var' as instance properties
                    prop_def = PropertyDefinition(
                        member.name,
                        member.type_annotation,
                        member.initializer,
                        False,  # is_static
                        False,  # is_private
                        False,  # is_const
                        member.file,
                        member.line,
                        member.column,
                    )
                    temp_class.instance_properties[member.name] = prop_def

                elif member.type == "PropertyDefinition":
                    # Use member directly or create a new one with forced flags
                    prop_def = PropertyDefinition(
                        member.name,
                        member.type_annotation,
                        member.initializer,
                        False,  # non‑static
                        False,  # non‑private
                        getattr(member, "is_const", False),
                        member.file,
                        member.line,
                        member.column,
                    )
                    temp_class.instance_properties[member.name] = prop_def

                elif member.type == "PropertyHandler":
                    # Add getter and setter as methods
                    temp_class.instance_methods[member.getter.name] = member.getter
                    temp_class.instance_methods[member.setter.name] = member.setter

                else:
                    raise NovaError(member, "not supported")

            # 4. Instantiate and assign to the variable name provided
            instance = temp_class.instantiate([])
            env.define(stmt.name, instance)
        elif stmt.type == "WhileStmt":
            while self.evaluate_expr(stmt.condition, env):
                result = self.execute_block(stmt.body, Environment(env))
                if isinstance(result, BreakFlow):
                    break
                elif isinstance(result, ReturnFlow):
                    return result
                elif isinstance(result, ContinueFlow):
                    continue
        elif stmt.type == "UntilStmt":
            while not self.evaluate_expr(stmt.condition, env):
                result = self.execute_block(stmt.body, Environment(env))
                if isinstance(result, BreakFlow):
                    break
                elif isinstance(result, ReturnFlow):
                    return result
                elif isinstance(result, ContinueFlow):
                    continue
        elif stmt.type == "ForEachStmt":
            list_val = self.evaluate_expr(stmt.list, env)
            if not isinstance(list_val, (list, dict)) and not hasattr(
                list_val, "__iter__"
            ):
                raise NovaError(
                    stmt,
                    f"Cannot iterate over non-array type for forEach loop. Got: {type(list_val).__name__}",
                )
            for item in list_val:
                loop_env = Environment(env)
                loop_env.define(stmt.variable, item)
                result = self.execute_block(stmt.body, loop_env)
                if isinstance(result, BreakFlow):
                    break
                elif isinstance(result, ReturnFlow):
                    return result
                elif isinstance(result, ContinueFlow):
                    continue
        elif stmt.type == "ForStmt":
            start = self.evaluate_expr(stmt.start, env)
            end = self.evaluate_expr(stmt.end, env)
            step = self.evaluate_expr(stmt.step, env) if stmt.step else 1

            if not all(isinstance(val, (int, float)) for val in [start, end, step]):
                raise NovaError(
                    stmt,
                    f"For loop bounds and step must be numbers. Got start: {type(start).__name__}, end: {type(end).__name__}, step: {type(step).__name__}",
                )

            # Python's range handles step correctly. For float steps, manual loop.
            # Assuming integer steps for now, as floats can lead to precision issues.
            # If NovaScript intends float steps, this will need adjustment.
            if isinstance(step, float):
                current_val = start
                while (step > 0 and current_val <= end) or (
                    step < 0 and current_val >= end
                ):
                    loop_env = Environment(env)
                    loop_env.define(stmt.variable, current_val)
                    result = self.execute_block(stmt.body, loop_env)
                    if isinstance(result, BreakFlow):
                        break
                    elif isinstance(result, ReturnFlow):
                        return result
                    elif isinstance(result, ContinueFlow):
                        current_val += step
                        continue
                    current_val += step
            else:  # Integer step
                for i in range(start, end + (1 if step > 0 else -1), step):
                    loop_env = Environment(env)
                    loop_env.define(stmt.variable, i)
                    result = self.execute_block(stmt.body, loop_env)
                    if isinstance(result, BreakFlow):
                        break
                    elif isinstance(result, ReturnFlow):
                        return result
                    elif isinstance(result, ContinueFlow):
                        continue
        elif stmt.type == "ScopeStmt":
            n_env = Environment(env)
            n_env.localsOnly = True
            result = self.execute_block(stmt.body, n_env)
            env.define(stmt.name, n_env)
            if isinstance(result, ControlFlow):
                return result

        elif stmt.type == "SwitchStmt":
            value = self.evaluate_expr(stmt.expression, env)
            matched = False
            default_case_body = None
            for case in stmt.cases:
                if case.case_expr is None:  # This is the default case
                    default_case_body = case.body
                else:
                    case_val = self.evaluate_expr(case.case_expr, env)
                    if value == case_val:
                        result = self.execute_block(case.body, Environment(env))
                        if isinstance(result, ControlFlow):
                            return result
                        matched = True
                        break  # Exit switch after first match

            if not matched and default_case_body and not stmt.strict:
                result = self.execute_block(default_case_body, Environment(env))
                if isinstance(result, ControlFlow):
                    return result

            if stmt.strict and not matched:
                raise NovaError(
                    stmt, f"Switch statement did not match any case for value: {value}"
                )

        elif stmt.type == "ReturnStmt":
            if not stmt.expression:
                return ReturnFlow(None)
            value = self.evaluate_expr(stmt.expression, env)
            return ReturnFlow(value)
        elif stmt.type == "LocalFuncDecl":
            _env = Environment(env)
            _env.localsOnly = True
            self.execute_stmt(stmt.fn, _env)
            env.define(
                stmt.fn.name, _env.get(stmt.fn.name).value, _env.get(stmt.fn.name).const
            )
        elif stmt.type == "FuncDecl":
            # Named function – same machinery as a lambda, just with a name
            # and stored in the environment.
            func_wrapper = self._create_callable(
                stmt.parameters,
                stmt.body,
                env,
                name=stmt.name,
                node=stmt,
                context_name=f"function '{stmt.name}'",
            )
            env.define(stmt.name, func_wrapper)

        elif stmt.type == "ClassDefinition":
            class_def = stmt
            super_class = None
            if class_def.superclass_name:
                # Resolve the fully qualified superclass name
                name_parts = class_def.superclass_name.split(".")
                current_resolved_object = env
                for i, part in enumerate(name_parts):
                    if isinstance(current_resolved_object, Environment):
                        # FIX: Get value from var
                        var_obj = current_resolved_object.get(part, class_def)
                        next_resolved_part = var_obj.value
                    elif isinstance(current_resolved_object, dict):
                        next_resolved_part = current_resolved_object.get(part)
                    elif hasattr(current_resolved_object, part):
                        next_resolved_part = getattr(current_resolved_object, part)
                    else:
                        raise NovaError(
                            class_def,
                            f"Cannot resolve part '{part}' in superclass path '{class_def.superclass_name}'.",
                        )

                    if next_resolved_part is None:
                        raise NovaError(
                            class_def,
                            f"Superclass '{class_def.superclass_name}' part '{part}' not found.",
                        )

                    current_resolved_object = next_resolved_part

                super_class = current_resolved_object

                # Allow NovaClass or Python type/class as superclass
                if not isinstance(super_class, (NovaClass, type)):
                    raise NovaError(
                        class_def,
                        f"Superclass '{class_def.superclass_name}' resolved to type {type(super_class).__name__}, which is not a class.",
                    )

            # Pass the resolved super_class to NovaClass
            nova_class = NovaClass(class_def.name, super_class, self, env)

            # Populate static members, instance properties/methods
            for member in class_def.body:
                if member.type == "PropertyDefinition":
                    if member.is_private:
                        nova_class._private_properties.add(member.name)
                    if getattr(member, "is_const", False):
                        nova_class._const_properties.add(member.name)
                    if member.is_static:
                        prop_value = None
                        if member.initializer:
                            prop_value = self.evaluate_expr(member.initializer, env)
                        nova_class.static_members[member.name] = prop_value
                        if getattr(member, "is_const", False):
                            nova_class._static_const.add(member.name)
                    else:
                        nova_class.instance_properties[member.name] = member
                elif member.type == "PropertyHandler":
                    nova_class.instance_properties[member.name] = member

                elif member.type == "MethodDefinition":
                    if member.is_private:
                        nova_class._private_methods.add(member.name)
                    if member.is_constructor:
                        nova_class.constructor_def = member
                    elif member.is_static:
                        # Wrap static methods using shared bind helper
                        def static_method_wrapper(*args, _member=member, **kwargs):
                            method_env = Environment(env)
                            method_env.define("self", nova_class.static_members)
                            bound = bind_parameters(
                                _member.parameters,
                                args,
                                kwargs,
                                self,
                                env,
                                context_name=f"static method '{_member.name}'",
                                token=_member,
                            )
                            for pname, pval in bound.items():
                                method_env.define(pname, pval)
                            result = self.execute_block(_member.body, method_env)
                            if isinstance(result, ReturnFlow):
                                return result.value
                            return None

                        nova_class.static_members[member.name] = static_method_wrapper
                    else:
                        nova_class.instance_methods[member.name] = member
                else:
                    raise NovaError(member, f"not supported: {member}")

            env.define(class_def.name, nova_class)
        elif stmt.type == "AssertStmt":
            e = self.evaluate_expr(stmt.expression, env)
            if not e:
                raise NovaError(stmt, f"{stmt.message}")
        elif stmt.type == "UsingStmt":
            # Helper to import members from a dict/Environment/module into env
            def import_members(value):
                if isinstance(value, NovaClass):
                    # Import public static members only
                    for key, val in value.get_public_static_members().items():
                        env.define(key, val)
                elif isinstance(value, dict) and "__DefiningClass" in value:
                    # NovaScript instance: import public instance members
                    cls = value["__DefiningClass"]
                    for key, val in cls.get_public_instance_members(value).items():
                        env.define(key, val)
                elif isinstance(value, Environment):
                    for key, var_obj in value.values.items():
                        # Environment has no private concept; import everything
                        env.define(
                            key, var_obj.value, var_obj.const, var_obj.type_annotation
                        )
                elif isinstance(value, dict):
                    # Plain dict – import all keys (no privacy)
                    for key, val in value.items():
                        env.define(key, val)
                elif hasattr(value, "__dict__"):
                    # Python object – skip names starting with '_' (by convention)
                    for key, val in value.__dict__.items():
                        if not key.startswith("_"):
                            env.define(key, val)
                else:
                    raise NovaError(
                        stmt,
                        f"Cannot 'use' value of type {type(value).__name__}. "
                        "Expected a namespace, dict, class, instance, or Python object.",
                    )

            if isinstance(stmt.name, list):
                # List of string paths: resolve each and import members
                for path_str in stmt.name:
                    parts = path_str.split(".")
                    current = env
                    for part in parts:
                        if isinstance(current, Environment):
                            var_obj = current.get(part, stmt)
                            current = var_obj.value
                        elif isinstance(current, dict):
                            current = current.get(part)
                        elif hasattr(current, part):
                            current = getattr(current, part)
                        else:
                            raise NovaError(
                                stmt,
                                f"Cannot resolve part '{part}' in path '{path_str}'.",
                            )
                        if current is None:
                            raise NovaError(
                                stmt,
                                f"Name '{path_str}' part '{part}' not found.",
                            )
                    import_members(current)
            else:
                # Single expression: evaluate it, then import members from the result
                value = self.evaluate_expr(stmt.name, env)
                import_members(value)
        else:
            raise NovaError(stmt, f"Unknown statement type: {stmt.type}")

    # --- NEW HELPER: Resolves the base object and final key/index for assignment ---
    def resolve_assignment_target(self, target_expr, env):
        if isinstance(target_expr, Identifier):
            # For simple identifiers, the base is the environment and the key is the identifier name
            return {"base": env, "final_key": target_expr.name}
        elif isinstance(target_expr, ArrayAccess):
            # Recursively evaluate the object part to get the actual array/object
            base_object = self.evaluate_expr(target_expr.object, env)
            index = self.evaluate_expr(target_expr.index, env)

            if base_object is None:
                raise NovaError(target_expr, "Cannot assign to index of None value.")
            if not isinstance(base_object, (list, dict)) and not hasattr(
                base_object, "__setitem__"
            ):  # Python lists/dicts for arrays/objects
                # print(dir(base_object))
                raise NovaError(
                    target_expr,
                    f"Cannot assign to index of non-list/dict: {type(base_object).__name__}",
                )
            if not isinstance(index, (int, str)):
                raise NovaError(
                    target_expr,
                    f"List/dict index must be a number or string for assignment. Got: {type(index).__name__}",
                )
            return {"base": base_object, "final_key": index}
        elif isinstance(target_expr, PropertyAccess):
            # Recursively evaluate the object part to get the actual object
            base_object = self.evaluate_expr(target_expr.object, env)
            key = target_expr.property  # Property name is a string

            if base_object is None:
                raise NovaError(
                    target_expr, f"Cannot assign property '{key}' of None value."
                )

            # Special case: if the base object is an Environment, we assign to it directly
            if isinstance(base_object, Environment):
                return {"base": base_object, "final_key": key}
            # If the base object is a NovaScript instance (a Python dict), assign to its key.
            elif isinstance(base_object, dict):
                return {"base": base_object, "final_key": key}
            # For other Python objects, assume it's a regular attribute access.
            else:
                return {"base": base_object, "final_key": key}
        else:
            raise NovaError(
                target_expr, "Invalid assignment target type: " + target_expr.type
            )

    def get_target_type(self, target, env):
        """Resolve the expected type for an assignment target (Identifier or nested PropertyAccess)."""
        if isinstance(target, Identifier):
            var = env.get(target.name, target)
            return var.type_annotation

        if isinstance(target, PropertyAccess):
            # Recurse to get the type of the parent object
            parent_type = self.get_target_type(target.object, env)
            if not parent_type or parent_type not in CUSTOM_TYPES:
                return None
            custom_def = CUSTOM_TYPES[parent_type]
            prop_def = next(
                (p for p in custom_def.properties if p.name == target.property), None
            )
            return prop_def.type if prop_def else None

        # ArrayAccess or anything else → no static type check for now
        return None

    def evaluate_expr(self, expr, env):
        dprint("expr: ", expr)

        # Prefer registered handler when present (extension point for new expr types)
        handler = self._expr_handlers.get(expr.type)
        if handler is not None:
            return handler(expr, env)

        if expr.type == "Literal":
            return expr.value
        elif expr.type == "Identifier":
            return env.get(expr.name, expr).value

        elif expr.type == "AssignmentExpr":
            target = expr.target
            op = expr.operator

            # Resolve where we are assigning to
            resolved = self.resolve_assignment_target(target, env)
            base = resolved["base"]
            final_key = resolved["final_key"]

            assigned_value = self.evaluate_expr(expr.value, env)

            # === Compound assignment handling ===
            if op != "=":
                if isinstance(base, Environment):
                    current_value = base.get(final_key, target).value
                elif isinstance(base, dict):
                    current_value = base.get(final_key)
                else:
                    current_value = getattr(base, final_key, None)
                if isinstance(current_value, Proxy):
                    if not current_value.get:
                        raise NovaError(
                            target, f"Property '{final_key}' has no getter."
                        )
                    current_value = current_value.get()

                if op == "+=":
                    final_value_to_assign = current_value + assigned_value
                elif op == "-=":
                    final_value_to_assign = current_value - assigned_value
                elif op == "*=":
                    if isinstance(current_value, (int, float)) and not isinstance(
                        assigned_value, (int, float)
                    ):
                        raise NovaError(
                            expr,
                            f"If you wanted to repeat '{assigned_value}' {current_value} times, "
                            f"you'd do '\"{assigned_value}\" * {current_value}'",
                        )
                    final_value_to_assign = current_value * assigned_value
                elif op == "/=":
                    if assigned_value == 0:
                        raise NovaError(
                            expr, "Division by zero in compound assignment."
                        )
                    final_value_to_assign = current_value / assigned_value
                elif op == "%=":
                    final_value_to_assign = current_value % assigned_value
                else:
                    raise NovaError(expr, f"Unknown compound assignment operator: {op}")
            else:
                final_value_to_assign = assigned_value

            # === Perform the actual assignment + type checking ===
            if isinstance(target, ArrayLiteral):  # destructuring
                source_array = final_value_to_assign
                if not isinstance(source_array, list):
                    raise NovaError(
                        target,
                        f"Cannot destructure non-list value. Expected list, got {type(source_array).__name__}.",
                    )
                for i, target_element in enumerate(target.elements):
                    source_value = source_array[i] if i < len(source_array) else None
                    temp_assignment = AssignmentExpr(
                        target_element,
                        Literal(source_value, expr.file, expr.line, expr.column),
                        "=",
                        expr.file,
                        expr.line,
                        expr.column,
                    )
                    self.evaluate_expr(temp_assignment, env)

            else:  # normal / property / nested assignment
                if isinstance(base, Environment):
                    var = base.get(final_key, target)
                    if var.type_annotation:
                        check_type(var.type_annotation, final_value_to_assign, target)
                    base.assign(final_key, final_value_to_assign, target)

                elif isinstance(base, dict):
                    # Enforce const properties on Nova instances
                    const_set = base.get("__const__")
                    if const_set and final_key in const_set:
                        raise NovaError(
                            target,
                            f"Cannot re-assign constant property '{final_key}'",
                        )
                    # Resolve expected type for (possibly nested) property using root variable's type
                    if final_key in base and isinstance(base[final_key], Proxy):
                        proxy = base[final_key]
                        if not proxy.set:
                            raise NovaError(
                                target, f"Property '{final_key}' has no setter."
                            )
                        cls = base.get("__DefiningClass")
                        if cls:
                            try:
                                self.current_class_stack.append(cls)
                                proxy.set(final_value_to_assign)
                            finally:
                                self.current_class_stack.pop()
                        else:
                            proxy.set(final_value_to_assign)
                    elif "__DefiningClass" in base and isinstance(
                        assigned_value, Callable
                    ):
                        # === Dynamic method binding ===
                        # Makes it easy to do: instance.foo = def() print(self.name) end
                        # We prefer the original AST (LambdaDecl) when available so we
                        # can reconstruct a proper MethodDefinition that receives `self`.
                        # Extension point: if you add new callable AST nodes, just attach
                        # .__nova_ast__ on the wrapper (see LambdaDecl evaluation).
                        ast_node = getattr(assigned_value, "__nova_ast__", None)
                        if (
                            ast_node is not None
                            and hasattr(ast_node, "parameters")
                            and hasattr(ast_node, "body")
                        ):
                            meth = MethodDefinition(
                                final_key,
                                ast_node.parameters,
                                ast_node.body,
                                False,  # is_static
                                False,  # is_constructor
                                False,  # is_private
                                expr.file,
                                expr.line,
                                expr.column,
                            )
                            bound = base["__DefiningClass"]._create_method(meth, base)
                            base[final_key] = bound
                        elif hasattr(expr.value, "parameters") and hasattr(
                            expr.value, "body"
                        ):
                            # Fallback: RHS was still the raw AST (should not normally happen)
                            meth = MethodDefinition(
                                final_key,
                                expr.value.parameters,
                                expr.value.body,
                                False,
                                False,
                                False,
                                expr.file,
                                expr.line,
                                expr.column,
                            )
                            bound = base["__DefiningClass"]._create_method(meth, base)
                            base[final_key] = bound
                        else:
                            # Plain Python callable or already-bound function – store as-is
                            base[final_key] = final_value_to_assign

                    else:
                        expected_type = self.get_target_type(target, env)
                        if expected_type:
                            check_type(expected_type, final_value_to_assign, target)
                        base[final_key] = final_value_to_assign

                else:
                    # Python object
                    try:
                        setattr(base, final_key, final_value_to_assign)
                    except (AttributeError, TypeError):
                        if isinstance(base, (dict, list)) or hasattr(
                            base, "__setitem__"
                        ):
                            base[final_key] = final_value_to_assign
                        else:
                            raise NovaError(
                                target,
                                f"Cannot assign to property '{final_key}' of object of type {type(base).__name__}.",
                            )

            return final_value_to_assign

        elif expr.type == "BinaryExpr":
            dprint("BinOp", expr)
            if expr.operator == "&&":
                f = self.evaluate_expr(expr.left, env)
                if not f:
                    return f
                return self.evaluate_expr(expr.right, env)
            elif expr.operator == "||":
                f = self.evaluate_expr(expr.left, env)
                if f:
                    return f
                return self.evaluate_expr(expr.right, env)
            left = self.evaluate_expr(expr.left, env)
            right = self.evaluate_expr(expr.right, env)

            # Handle operator overloading for custom objects (dicts)
            if isinstance(left, dict):
                op = opToName(expr.operator)
                if op and op in left and callable(left[op]):
                    try:
                        return left[op](right)
                    except Exception as e:
                        raise NovaError(
                            expr, f"Error calling overloaded operator '{op}': {e}"
                        )
                # If no operator overloading found, fall through to normal operations

            # Normal binary operations
            if expr.operator == "+":
                return left + right
            elif expr.operator == "|":
                return left | right
            elif expr.operator == "&":
                return left & right
            elif expr.operator == "%":
                if isinstance(left, str):
                    if isinstance(right, list):
                        left = left.format(*right)
                    elif isinstance(right, dict):
                        left = left.format(**right)
                    else:
                        raise NovaError(
                            expr,
                            "'%' formatting is only available when right side is an array or object",
                        )
                    return left
                return left % right
            elif expr.operator == "^":
                return left ^ right
            elif expr.operator == "-":
                return left - right
            elif expr.operator == "*":
                if isinstance(left, (int, float)) and not isinstance(
                    right, (int, float)
                ):
                    raise NovaError(
                        expr,
                        f"If you wanted to repeat '{right}' {left} times, you'd do '\"{right}\" * {left}'",
                    )
                return left * right
            elif expr.operator == "**":
                return left**right
            elif expr.operator == "/":
                if right == 0:
                    raise NovaError(expr, "Division by zero is not allowed.")
                return left / right
            elif expr.operator == "==":
                return left == right
            elif expr.operator == "!=":
                return left != right
            elif expr.operator == "<":
                return left < right
            elif expr.operator == ">":
                return left > right
            elif expr.operator == ">>":
                return left >> right
            elif expr.operator == "<<":
                return left << right
            elif expr.operator == "<=":
                return left <= right
            elif expr.operator == "between":
                if not isinstance(right, (list, tuple)) or len(right) != 2:
                    raise NovaError(expr, "'between' requires a range [low, high]")
                return left >= right[0] and left <= right[1]
            elif expr.operator == ">=":
                return left >= right
            elif expr.operator == "->":
                if not callable(right):
                    raise NovaError(expr, f"right side of pipe expr must be a function")
                return right(left)
            elif expr.operator == "=>":
                if not callable(right):
                    raise NovaError(expr, f"right side of pipe expr must be a function")
                clone = copy.deepcopy(left)
                if isinstance(clone, dict):
                    for k, v in clone.items():
                        clone[k] = right(v, k)
                elif isinstance(clone, list):
                    for i, v in enumerate(clone):
                        clone[i] = right(v, i)
                else:
                    raise NovaError(expr, "=> only works on arrays or objects")
                return clone
            else:
                raise NovaError(expr, f"Unknown binary operator: {expr.operator}")

        elif expr.type == "UnaryExpr":
            right = self.evaluate_expr(expr.right, env)
            if expr.operator == "-":
                if not isinstance(right, (int, float)):
                    raise NovaError(
                        expr,
                        f"Unary '-' operator can only be applied to numbers. Got: {type(right).__name__}",
                    )
                return -right
            elif expr.operator == "!":
                return not right
            elif expr.operator == "#":
                if isinstance(right, (list, str, dict)):
                    return len(right)
                elif hasattr(right, "__len__"):
                    return len(right)
                else:
                    raise NovaError(
                        expr, f"Unary '#' cannot be applied to {type(right).__name__}"
                    )

            else:
                raise NovaError(expr, f"Unknown unary operator: {expr.operator}")

        elif expr.type == "FuncCall":
            self.callStack.append(expr.name)
            try:
                func = env.get(expr.name, expr).value
                if not callable(func):
                    raise NovaError(expr, f"{expr.name} is not a function")
                args = []
                kwargs = {}
                for arg in expr.arguments:
                    if arg.type == "ExplodeExpr":
                        v = self.evaluate_expr(arg.args, env)
                        if isinstance(v, dict):
                            for name in v.keys():
                                kwargs[name] = v[name]
                        elif isinstance(v, Iterable) and not isinstance(
                            v, (str, bytes)
                        ):
                            args.extend(v)
                        else:
                            args.append(v)
                    elif arg.type == "NamedArg":
                        kwargs[arg.name] = self.evaluate_expr(arg.value, env)
                    else:
                        v = self.evaluate_expr(arg, env)
                        args.append(v)
                return func(*args, **kwargs)
            except Exception as E:
                self.errorStack.append(
                    " " * len(self.callStack)
                    + f"- {expr.file}:{expr.line}:{expr.column}: "
                    + "Error while executing: "
                    + expr.name
                )
                raise E
            finally:
                self.callStack.pop()

        elif expr.type == "MethodCall":
            obj = self.evaluate_expr(expr.object, env)
            self._check_private_access(obj, expr.method, expr)
            if obj is None:
                raise NovaError(expr, f"Cannot call method '{expr.method}' on None.")

            args = []
            kwargs = {}
            for arg in expr.arguments:
                if arg.type == "ExplodeExpr":
                    v = self.evaluate_expr(arg.args, env)
                    if isinstance(v, dict):
                        for name in v.keys():
                            kwargs[name] = v[name]
                    elif isinstance(v, Iterable) and not isinstance(v, (str, bytes)):
                        args.extend(v)
                    else:
                        args.append(v)
                elif arg.type == "NamedArg":
                    kwargs[arg.name] = self.evaluate_expr(arg.value, env)
                else:
                    v = self.evaluate_expr(arg, env)
                    args.append(v)

            # Handle NovaClass static methods
            if isinstance(obj, NovaClass):
                static_method = obj.static_members.get(expr.method)
                if callable(static_method):
                    return static_method(*args, **kwargs)
                raise NovaError(
                    expr,
                    f"Static method '{expr.method}' not found or is not a function on class '{obj.name}'.",
                )

            fn = None
            # If obj is a NovaScript instance (represented as a Python dictionary)
            if isinstance(obj, dict) and expr.method in obj:
                fn = obj[expr.method]
            # If obj is an Environment (e.g., 'self' within a NovaScript method)
            elif isinstance(obj, Environment):
                # FIX: Get value from var
                fn = obj.get(expr.method, expr).value
            # For regular Python objects exposed to NovaScript
            else:
                fn = getattr(obj, expr.method, None)

            if not callable(fn):
                raise NovaError(
                    expr, f"{expr.method} is not a function or method on this object"
                )

            # Push class context if the method has a defining clas attribute (NovaScript bound method)
            if isinstance(obj, dict) and "__DefiningClass" in obj:
                # Push the defining class before calling the method
                self.current_class_stack.append(obj["__DefiningClass"])
                try:
                    result = fn(*args, **kwargs)
                finally:
                    self.current_class_stack.pop()
                return result
            else:
                try:
                    if hasattr(fn, "__DefiningClass"):
                        self.current_class_stack.append(fn.__DefiningClass)
                    result = fn(*args, **kwargs)
                finally:
                    if hasattr(fn, "__DefiningClass"):
                        self.current_class_stack.pop()
                return result

        elif expr.type == "ArrayAccess":
            arr = self.evaluate_expr(expr.object, env)
            index = self.evaluate_expr(expr.index, env)
            if not isinstance(arr, (list, dict, str, tuple)) and not hasattr(
                arr, "__getitem__"
            ):
                raise NovaError(
                    expr,
                    f"Cannot access index of non-list/dict/indexible: {type(arr).__name__}",
                )
            if not isinstance(index, (int, str)):
                raise NovaError(
                    expr,
                    f"List/dict index must be a number or string. Got: {type(index).__name__}",
                )
            try:
                return arr[index]
            except (IndexError, KeyError):
                raise NovaError(
                    expr, f"Index/key '{index}' out of bounds or not found for object."
                )

        elif expr.type == "PropertyAccess":
            obj = self.evaluate_expr(expr.object, env)
            self._check_private_access(obj, expr.property, expr)

            if obj is None:
                raise NovaError(
                    expr, f"Cannot access property '{expr.property}' of None."
                )

            # Handle NovaClass static properties
            if isinstance(obj, NovaClass):
                if expr.property in obj.static_members:
                    return obj.static_members[expr.property]
                raise NovaError(
                    expr,
                    f"Static property '{expr.property}' not found on class '{obj.name}'.",
                )
            if isinstance(obj, dict) and expr.property in obj:
                val = obj[expr.property]
                if isinstance(val, Proxy):
                    if not val.get:
                        raise NovaError(
                            expr, f"Property '{expr.property}' has no getter."
                        )
                    cls = obj.get("__DefiningClass")
                    if cls:
                        try:
                            self.current_class_stack.append(cls)
                            return val.get()
                        finally:
                            self.current_class_stack.pop()
                    else:
                        return val.get()
                return val
            # If obj is a NovaScript instance (represented as a Python dictionary)
            # if isinstance(obj, dict) and expr.property in obj:
            #    return obj[expr.property]
            # If obj is an Environment (e.g., 'self' within a NovaScript method)
            elif isinstance(obj, Environment):
                # FIX: Get value from var
                return obj.get(expr.property, expr).value
            # For regular Python objects (e.g., exposed modules, native types)
            elif hasattr(obj, expr.property):
                return getattr(obj, expr.property)

            raise NovaError(expr, f"Property '{expr.property}' not found on object.")

        elif expr.type == "ArrayLiteral":
            return [self.evaluate_expr(element, env) for element in expr.elements]

        elif expr.type == "ObjectLiteral":
            obj = {}
            for prop in expr.properties:
                obj[prop["key"]] = self.evaluate_expr(prop["value"], env)
            return obj

        elif expr.type == "NewInstance":
            # Resolve the fully qualified class name string
            name_parts = expr.class_name.split(".")
            current_resolved_object = env  # Start resolution from current environment

            for i, part in enumerate(name_parts):
                if isinstance(current_resolved_object, Environment):
                    # FIX: Get value from var
                    var_obj = current_resolved_object.get(part, expr)
                    next_resolved_part = var_obj.value
                elif isinstance(current_resolved_object, dict):
                    next_resolved_part = current_resolved_object.get(part)
                elif hasattr(current_resolved_object, part):
                    next_resolved_part = getattr(current_resolved_object, part)
                else:
                    raise NovaError(
                        expr,
                        f"Cannot resolve part '{part}' in class path '{expr.class_name}'. Previous part was type {type(current_resolved_object).__name__}.",
                    )

                if next_resolved_part is None:
                    raise NovaError(
                        expr, f"Class '{expr.class_name}' part '{part}' not found."
                    )

                current_resolved_object = next_resolved_part

            target_class = current_resolved_object
            args = [self.evaluate_expr(arg, env) for arg in expr.arguments]
            c = None
            if isinstance(target_class, NovaClass):
                c = target_class.instantiate(args)
            elif isinstance(target_class, type) and hasattr(
                target_class, "__init__"
            ):  # It's a Python class
                # For Python classes, we instantiate them directly.
                try:
                    c = target_class(*args)
                except Exception as e:
                    raise NovaError(
                        expr,
                        f"Error instantiating Python class '{expr.class_name}' (resolved to {target_class}): {e}",
                    )
            else:
                raise NovaError(
                    expr,
                    f"'{expr.class_name}' (resolved to {target_class}) is not a constructible class.",
                )
            if CUSTOM_TYPES.get(expr.class_name):
                check_type(
                    expr.class_name,
                    c,
                    expr,
                )
            return c
        elif expr.type == "LambdaDecl":
            # A lambda is just an unnamed / uuid-named function.
            # Same creation path as FuncDecl; class context is captured
            # automatically by _create_callable.
            return self._create_callable(
                expr.parameters,
                expr.body,
                env,
                name=None,  # → random uuid
                node=expr,
                context_name="lambda",
            )
        elif expr.type == "DecoratorExpr":
            # print(expr.__dict__)
            value = self.evaluate_expr(expr.expr, env)
            if not callable(value):
                raise NovaError(expr.expr, "Expected to return a function")
            # print(f"{value = }")
            body = self.evaluate_expr(expr.body, env)

            if not callable(body):
                raise NovaError(expr.body, "Expected to be a function")
            # print(f"{body = }")
            val = value(body)
            # print(f"{val = }")
            return val
        elif expr.type == "EnumDef":
            v = Environment()  # no sense copying env here
            v.values = {}
            for i in range(len(expr.values)):
                v.define(expr.values[i], i)

            v.lock()
            return v
        else:
            raise NovaError(expr, f"Unknown expression type: {expr}")
