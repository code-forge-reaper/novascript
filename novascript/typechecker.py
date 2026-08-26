

from collections.abc import Callable
from .nodes import NovaError
BUILTIN_VAR_TYPES: dict[str, object] = {
    "string": str,
    "number": (int, float),
    "bool": bool,
    "function": Callable,
    "list": list,
    "int": int,
    "float": float,
}

BUILTIN_VAR_TYPES_INFER: dict[str, str] = {
    "str": "string",
    "bool": "bool",
    "function": "function",
    "list": "list",
    "int": "int",
    "float": "float",
}

CUSTOM_TYPES = {}  # Dictionary for custom types


def check_type(expected, value, token):
    if expected in ["any", None]:
        return
    # Handle array types: int[], string[][], list[], etc.
    if isinstance(expected, str) and expected.endswith("[]"):
        base_type = expected[:-2]
        if not isinstance(value, list):
            raise NovaError(
                token,
                f"Type mismatch: expected array type '{expected}', got {type(value).__name__}",
            )
        for elem in value:
            check_type(base_type, elem, token)
        return
    custom_type_definition = BUILTIN_VAR_TYPES.get(expected, None)
    if not custom_type_definition:
        custom_type_definition = CUSTOM_TYPES.get(expected)
        if custom_type_definition is None:
            raise NovaError(token, f"unknown type definition: {expected}")
        check_custom_type(expected, custom_type_definition, value, token)
    else:
        if not isinstance(value, custom_type_definition):
            raise NovaError(
                token,
                f"Type mismatch: expected {custom_type_definition}, got {type(value).__name__}",
            )


def check_custom_type(expected, custom_type_definition, value, token):
    if custom_type_definition:
        if not isinstance(value, dict):
            raise NovaError(
                token,
                f"Type mismatch: expected custom type '{expected}', got {type(value).__name__}",
            )
        missing = []

        # Collect missing properties
        for prop_def in custom_type_definition.properties:
            if prop_def.name not in value:
                missing.append(prop_def)

        if missing:
            missing_list = ""
            for f in missing:
                missing_list += f"\n- {f.name} ({f.type})"
            raise NovaError(
                token,
                f"Type mismatch: custom type '{expected}' is missing properties: {missing_list}",
            )

        # Now check each property type
        for prop_def in custom_type_definition.properties:
            check_type(prop_def.type, value[prop_def.name], token)
