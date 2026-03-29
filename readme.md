# NovaScript

**A fun, expressive, Python-hosted scripting language** with Lua-inspired syntax, seamless Python interop, and a focus on developer joy.

Built from the ground up because I wanted something that felt *right* — simple syntax, powerful features, and zero bullshit.

---

## Features

- **Clean, readable syntax** — `do ... end` blocks, short functions, and expressive control flow
- **Modern module system** — `load("module")` with priority for `.nova` files + fallback to Python
- **`export` keyword** for clean, declarative modules
- **Static-ish type checking** with runtime enforcement (`var x number`, custom `define` types)
- **Operator overloading** (`__Add`, `__Str`, `__Mul`, etc.)
- **Compound assignments** and chained assignments (`z *= y *= x *= 42`)
- **`test ... failed` blocks** — expressive error handling
- **Pipe operators** `->` and `=>` (map-style on arrays/objects)
- **Full Python interop** via `load("py:module")`
- **Built-in standard library** (`mathlib`, `BoxBuilder`, `Fs`, `Runtime`, `Convert`, etc.)
- **Sublime Text syntax highlighting** included

---

## Quick Start

```bash
# Run an example
./nova.py examples/example.nova

# Or make it even shorter
chmod +x nova.py
./nova.py examples/server.nova
```

### Requirements
- Python 3.8+
- No external dependencies (pure Python interpreter)

---

## Project Structure

| Folder / File         | Purpose |
|-----------------------|-------|
| `novascript.py`       | Core interpreter (parser + runtime) |
| `nodes.py`            | AST node definitions |
| `nova.py`             | CLI entry point |
| `examples/`           | Demo scripts (HTTP server, game, BoxBuilder, etc.) |
| `libs/`               | Standard library (mathlib, BoxBuilder, etc.) |
| `legacy/`             | Old JavaScript/TypeScript version |
| `nova.sublime-syntax` | Syntax highlighting for Sublime Text |

---

## Example

```nova
const mmath = load("mathlib")
const sock = load("socket")

var v1 = new mmath.Vec2(20, 30)
var v2 = new mmath.Vec2(50, 10)

print(v1 + v2)                    // Vector2(x: 70, y: 40)
print("hello from NovaScript!")

test
    var s string = "hello"
    s = 42                        // will trigger type error
failed err
    print("expected error:", err)
end
```

Check out `examples/example.nova` and `examples/server.nova` for more.

---

## Why NovaScript?

This is my longest-running personal project. I started it because I was tired of languages that were either too verbose or too magical. NovaScript sits in a sweet spot:

- Expressive like Lua/Python
- Safe-ish with optional types
- Extremely easy to extend with Python
- Actually fun to write

(Yes, some commits were just `git commit -m "$(date)"`. We’ve all been there.)

---

## Syntax Highlighting

Sublime Text support is included (`nova.sublime-syntax`). Just copy it to your Sublime packages folder.

---

## Legacy

The original implementation was written in TypeScript/Node.js (see the `legacy/` folder). The current Python version is significantly more mature and performant for what it is.

---

## License

This is my personal passion project. Feel free to explore, fork, or use pieces of it. Just don’t call it “enterprise ready” — we both know better.

---

**Made with love (and occasional existential crises) by cross**

Star it if you enjoy it. Issues and PRs welcome.
