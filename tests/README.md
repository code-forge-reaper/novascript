# NovaScript tests

These are **automated tests**, not language tutorials.

- Examples for humans live in `../examples/`.
- Tests here use `assert` and should stay quiet on success (they only print a single OK line).

## Running

From the repository root:

```bash
python tests/run_tests.py
```

Or run a single file:

```bash
python nova.py tests/test_basics.nova
```

## Layout

| File | Covers |
|------|--------|
| `test_basics.nova` | Literals, vars, consts, arithmetic, comparison, logical, bitwise, compound on scalars |
| `test_arrays_objects.nova` | Arrays, objects/dicts, indexing, nested structures, compound on indexes |
| `test_index_compound.nova` | Regression for `$name[i] +=` / `-=` / etc. |
| `test_control.nova` | if/elseif/else, while, for, for-in, break, continue |
| `test_functions.nova` | Named funcs, recursion, short funcs, closures, higher-order |
| `test_classes.nova` | class, record, methods, properties |
| `test_errors.nova` | assert, test/failed, raise |
| `test_misc.nova` | between, enum, matches, map (`=>`), defer |
