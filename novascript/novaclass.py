from typing import Any

from .nodes import ReturnFlow, Token
from .helpers import Proxy
from .helpers import dprint
from .novaenv import Environment
from .nodes import PropertyHandler
from .typechecker import check_type
from .nodes import NovaError

def bind_parameters(
    parameters:list[Any], args:list[str], kwargs: dict[str, Any], interpreter: "Interpreter", env: Environment, context_name:str="function", token:Token|None=None
) -> dict[Any, Any]:
    """
    Common helper to bind positional + keyword + default + compact parameters.
    Returns a dict of {param_name: value} after type checks.
    Raises NovaError on missing args, duplicates, unexpected kwargs, type errors.
    """
    compact_param = None
    normal_params = []
    for p in parameters:
        if getattr(p, "is_compact", False):
            compact_param = p
            break
        normal_params.append(p)

    num_normal = len(normal_params)
    values = {}

    # 1. Positional → normal params
    for i, param in enumerate(normal_params):
        if i < len(args):
            values[param.name] = args[i]

    # 2. Remaining positional → compact
    if compact_param:
        if len(args) > num_normal:
            values[compact_param.name] = list(args[num_normal:])
        else:
            values[compact_param.name] = []

    # 3. Keyword arguments
    param_names = {p.name for p in parameters}
    for kw_name, kw_val in kwargs.items():
        if kw_name not in param_names:
            raise NovaError(
                token or None,
                f"Unexpected keyword argument '{kw_name}' in {context_name}",
            )
        if kw_name in values:
            raise NovaError(
                token or None,
                f"Parameter '{kw_name}' given both positionally and by keyword in {context_name}",
            )
        values[kw_name] = kw_val

    # 4. Defaults + type checks
    result = {}
    for param in parameters:
        if param.name in values:
            arg_val = values[param.name]
        elif getattr(param, "is_compact", False):
            continue  # already set to []
        else:
            if param.default is not None:
                arg_val = interpreter.evaluate_expr(param.default, env)
            else:
                raise NovaError(
                    param,
                    f"Missing argument for parameter '{param.name}' in {context_name}",
                )
        if param.annotation_type:
            check_type(param.annotation_type, arg_val, param)
        result[param.name] = arg_val

    return result


class NovaClass:
    """
    Redesigned class system with:
    - Support for `const` properties (instance and static)
    - Proper encapsulation via private tracking + current_class_stack
    - Cleaner method/constructor binding using shared bind_parameters helper
    - Scope-aware environments for methods
    """

    def __init__(self, name, super_class, interpreter, env):
        self.name = name
        self.super_class = super_class  # NovaClass or Python type
        self.interpreter = interpreter
        self.env = env  # definition environment (lexical parent)
        self.static_members = {"name": name}  # runtime static values
        self._static_const = set()  # names of const static members
        self._private_properties = set()
        self._private_methods = set()
        self._const_properties = set()  # instance const property names
        self.instance_properties = {}  # name -> PropertyDefinition AST
        self.instance_methods = {}  # name -> MethodDefinition AST
        self.constructor_def = None

        # Determine the top-most Python root, if any
        self._python_root = self._find_python_root()

    def get_public_instance_members(self, instance):
        result = {}
        # Properties (skip private)
        for name, prop_def in self.instance_properties.items():
            if name not in self._private_properties:
                value = (
                    instance[name]
                    if isinstance(instance, dict)
                    else getattr(instance, name, None)
                )
                result[name] = value
        # Methods (skip private)
        for name, method_def in self.instance_methods.items():
            if name not in self._private_methods:
                value = (
                    instance[name]
                    if isinstance(instance, dict)
                    else getattr(instance, name, None)
                )
                result[name] = value
        return result

    def get_public_static_members(self):
        # Return a shallow copy; callers should not mutate const ones
        return dict(self.static_members)

    def _find_python_root(self):
        """Return the most foundational Python superclass, or None."""
        cls = self
        root = None
        while cls is not None:
            if isinstance(cls.super_class, type):
                root = cls.super_class
            cls = cls.super_class if isinstance(cls.super_class, NovaClass) else None
        return root

    def __call__(self, *args):
        return self.instantiate(args)

    # ------------------------------------------------------------------
    #  Unified instance creation
    # ------------------------------------------------------------------
    def instantiate(self, args):
        if self._python_root is not None:
            instance = self._python_root.__new__(self._python_root)
            setattr(instance, "__DefiningClass", self)
        else:
            instance = {}
            instance["__DefiningClass"] = self

        # Walk the NovaClass hierarchy from the root Python subclass down,
        # adding all Nova properties and methods.
        self._build_instance_layout(instance)

        # Run constructor chain (most derived first, super calls move up)
        self._run_constructor_chain(args, instance)
        return instance

    # ------------------------------------------------------------------
    #  Build layout: properties + methods in order (top-down)
    # ------------------------------------------------------------------
    def _build_instance_layout(self, instance):
        # Recursively build from superclass first (proper inheritance chain)
        if isinstance(self.super_class, NovaClass):
            self.super_class._build_instance_layout(instance)

        # Track const/private on the instance for runtime checks (dict instances)
        if isinstance(instance, dict):
            if "__const__" not in instance:
                instance["__const__"] = set()
            if "__private__" not in instance:
                instance["__private__"] = set()

        # Add properties
        for prop_name, prop_def in self.instance_properties.items():
            value = None
            if isinstance(prop_def, PropertyHandler):
                _env = Environment(self.env)
                _env.define("self", instance)
                _env.define("__DefiningClass", self)
                self.interpreter.execute_stmt(prop_def.setter, _env)
                self.interpreter.execute_stmt(prop_def.getter, _env)
                set_func = _env.get("set").value
                get_func = _env.get("get").value
                dprint("PropertyHandler:", prop_def)
                value = Proxy(set_func, get_func, instance, self.interpreter, self)
            elif prop_def.initializer:
                value = self.interpreter.evaluate_expr(prop_def.initializer, self.env)

            self._set_instance_attr(instance, prop_name, value)

            # Mark const / private for later assignment checks
            # PropertyHandler has neither is_const nor is_private; use getattr
            if isinstance(instance, dict):
                if getattr(prop_def, "is_const", False):
                    instance["__const__"].add(prop_name)
                    self._const_properties.add(prop_name)
                if getattr(prop_def, "is_private", False):
                    instance["__private__"].add(prop_name)

        # Add methods
        for method_name, method_def in self.instance_methods.items():
            bound = self._create_method(method_def, instance)
            self._set_instance_attr(instance, method_name, bound)
            if isinstance(instance, dict) and method_def.is_private:
                instance["__private__"].add(method_name)

    @staticmethod
    def _set_instance_attr(instance, name, value):
        if isinstance(instance, dict):
            instance[name] = value
        else:
            setattr(instance, name, value)

    # ------------------------------------------------------------------
    #  Method binding (with super support)
    # ------------------------------------------------------------------
    def _create_method(self, method_def, instance):
        interpreter = self.interpreter
        env = self.env
        super_class = self.super_class
        defining_cls = self  # capture for nested lambdas / private checks

        def bound_method(*method_args, **method_kwargs):
            # Push onto the *defining* interpreter's stack so that:
            # 1. private access checks inside the method body succeed, and
            # 2. any lambda created inside the method captures this class
            #    (critical when the method is invoked from a different Interpreter,
            #     e.g. after load()).
            interpreter.current_class_stack.append(defining_cls)
            try:
                method_env = Environment(env)
                method_env.define("self", instance)

                # Build 'super' object for method calls
                if super_class:
                    super_obj = {}
                    if isinstance(super_class, NovaClass):
                        for sm_name, sm_def in super_class.instance_methods.items():

                            def make_super_method(mdef=sm_def, sname=sm_name):
                                def super_call(*super_args, **super_kwargs):
                                    super_env = Environment(super_class.env)
                                    super_env.define("self", instance)
                                    # Shared binding helper
                                    bound = bind_parameters(
                                        mdef.parameters,
                                        super_args,
                                        super_kwargs,
                                        interpreter,
                                        super_class.env,
                                        context_name=f"super method '{sname}'",
                                        token=mdef,
                                    )
                                    for pname, pval in bound.items():
                                        super_env.define(pname, pval)
                                    result = interpreter.execute_block(
                                        mdef.body, super_env
                                    )
                                    if isinstance(result, ReturnFlow):
                                        return result.value
                                    return None

                                return super_call

                            super_obj[sm_name] = make_super_method()
                    elif isinstance(super_class, type):
                        # Expose Python superclass methods
                        for attr_name in dir(super_class):
                            if not attr_name.startswith("_"):
                                attr = getattr(super_class, attr_name, None)
                                if callable(attr):

                                    def python_super_call(
                                        *args, aname=attr_name, **kwargs
                                    ):
                                        method = getattr(super_class, aname)
                                        try:
                                            return method(instance, *args, **kwargs)
                                        except Exception as e:
                                            raise NovaError(
                                                None,
                                                f"Error calling super Python method '{aname}': {e}",
                                            )

                                    super_obj[attr_name] = python_super_call
                    method_env.define("Base", super_obj)

                # Bind method parameters using shared helper
                bound_values = bind_parameters(
                    method_def.parameters,
                    method_args,
                    method_kwargs,
                    interpreter,
                    env,
                    context_name=f"method '{method_def.name}'",
                    token=method_def,
                )
                for pname, pval in bound_values.items():
                    method_env.define(pname, pval)

                result = interpreter.execute_block(method_def.body, method_env)
                if isinstance(result, ReturnFlow):
                    return result.value
                return None
            finally:
                if interpreter.current_class_stack:
                    interpreter.current_class_stack.pop()

        bound_method.__DefiningClass = self  # store the class for access checks
        if method_def.is_private:
            bound_method.__is_private__ = True
        return bound_method

    # ------------------------------------------------------------------
    #  Constructor chain
    # ------------------------------------------------------------------
    def _run_constructor_chain(self, args, instance):
        """Start the chain from the most derived class."""
        if self.constructor_def:
            self._execute_constructor(self.constructor_def, args, instance)

    def _execute_constructor(self, constructor_def, args, instance):
        constructor_env = Environment(self.env)
        constructor_env.define("self", instance)

        # Define 'super' for constructor
        if self.super_class:
            if isinstance(self.super_class, NovaClass):
                parent = self.super_class

                def super_constructor_call(*super_args):
                    if parent.constructor_def:
                        parent._execute_constructor(
                            parent.constructor_def, super_args, instance
                        )

                constructor_env.define("Base", super_constructor_call)
            elif isinstance(self.super_class, type):
                parent_type = self.super_class

                def super_constructor_call(*super_args):
                    try:
                        parent_type.__init__(instance, *super_args)
                    except Exception as e:
                        raise ValueError(
                            f"Error calling superclass Python constructor: {e}",
                        )

                constructor_env.define("Base", super_constructor_call)
            else:
                raise ValueError(
                    f"Invalid superclass type: {type(self.super_class).__name__}",
                )

        # Bind constructor parameters via shared helper
        bound = bind_parameters(
            constructor_def.parameters,
            args,
            {},  # constructors currently positional-only from call site
            self.interpreter,
            self.env,
            context_name=f"constructor of '{self.name}'",
            token=constructor_def,
        )
        for pname, pval in bound.items():
            constructor_env.define(pname, pval)

        self.interpreter.execute_block(constructor_def.body, constructor_env)
