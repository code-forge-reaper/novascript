# NovaScript

NovaScript is a dynamically-typed, Lua-inspired scripting language with first-class functions, object-oriented features, and familiar C-style syntax. It's implemented in TypeScript and can run in both Node.js and browser environments.

## Features

- **Lua-like syntax** with familiar control structures
- **First-class functions** and closures
- **Object-oriented** programming with prototypes
- **Modules** and namespacing
- **Exception handling** with try/catch
- **Deferred execution** with `defer`
- **Standard library** with common utilities
- **JSON** support built-in
- **Type annotations** (optional)

## Installation

### Node.js Runtime
```bash
# Clone the repository
git clone https://github.com/yourusername/novascript.git
cd novascript

# Install dependencies
npm install

# Make the CLI executable
chmod +x main.ts
```

### Python Runtime
NovaScript also includes a Python implementation that can be used as an alternative to the Node.js version.

```bash
# Clone the repository
git clone https://github.com/yourusername/novascript.git
cd novascript

# Run a NovaScript file
python3 nova.py example.nova
```

The Python implementation includes all the core features of NovaScript and can be easily extended with Python modules.

## Usage

Run a NovaScript file:
```bash
./main.ts path/to/script.nova
```

## Language Syntax

### Variables and Types
```nova
// Variable declarations with and without type annotations
var greeting string = "Hello, NovaScript!"
var count = 10
var isActive bool = true
var price number = 99.99

// Null, Undefined, NaN
var nothing = null
var notDefined = undefined
var notANumber = NaN

// String formatting and concatenation
var formattedString = "The value is %d and the name is %s.".format(count, "Item")
var concatenatedString = "Part1 ".concat("Part2 ", "Part3")
```

### Control Flow
```nova
// If-Elseif-Else
var temperature = 25
if (temperature > 30)
    print("It's hot!")
elseif (temperature > 20)
    print("It's warm.")
else
    print("It's cold.")
end

// While loop
var i = 0
while (i < 3)
    print("While loop iteration:", i)
    i += 1
end

// For loop (numeric range)
for j = 1, 5 do
    print("For loop iteration:", j)
end

// ForEach loop (array iteration)
var fruits = ["apple", "banana", "cherry"]
forEach fruit in fruits do
    print("Fruit:", fruit)
end

// Break and Continue
for m = 0, 5 do
    if m == 2
        print("Skipping 2 (continue)")
        continue
    end
    if m == 4
        print("Breaking at 4")
        break
    end
    print("Current m:", m)
end
```

### Functions
```nova
// Function declaration with parameters and return
func add(a number, b number)
    return a + b
end

// Function with default parameters
func greet(name = "Guest", age = 30)
    print("Hello, %s! You are %d years old.".format(name, age))
end

// Lambda function (anonymous function)
var multiply = def (x, y)
    return x * y
end

// Using the functions
var sumResult = add(5, 7)
greet("Alice")
greet()
greet("Bob", 25)
print("Multiply (lambda):", multiply(4, 6))
```

### Objects and Classes
```nova
// Object literals
var person = {
    name: "John Doe",
    age: 30,
    isStudent: false,
    address: {
        street: "123 Main St",
        city: "Anytown"
    },
    greet: func()
        return "Hello, my name is " + this.name
    end
}

// Class definition
class Animal
    var name: string
    var sound: string

    func init(name string, sound string)
        self.name = name
        self.sound = sound
        print("Animal created: %s".format(self.name))
    end

    func makeSound()
        print("%s says %s!".format(self.name, self.sound))
    end
end

// Using the class
var dog = new Animal("Rex", "Woof")
dog.makeSound()
```

### Error Handling
```nova
// Test-Failed (Try-Catch)
print("Test-Failed example:")
test
    print("Inside test block.")
    // This will trigger the 'failed' block
    Runtime.throw("Simulated error!")
    print("This line will not be reached if error occurs.")
failed err
    Logger.error("Caught an error:", err.message)
end

// Defer statement
print("Defer example:")
test
    print("Entering defer block scope.")
    defer
        print("Deferred statement 1 (executed last).")
    end
    defer
        print("Deferred statement 2 (executed first).")
    end
    print("Exiting defer block scope.")
failed err
    Logger.error("Error in defer scope test:", err.message)
end
```

### Custom Types and Modules
```nova
// Define a custom type 'Point'
type Point = {
    x: number,
    y: number
}

// Using the custom type
var p1 Point = { x: 10, y: 20 }
print("Point p1:", p1.x, p1.y)

// Importing modules
import "os:@r1tsuu/raylib" as rl
using rl

// Using imported module
InitWindow(800, 600, "NovaScript Window")
```

## Python Runtime Features

The Python implementation of NovaScript includes several powerful features:

- **Full compatibility** with the Node.js implementation
- **Python module integration** - Import and use Python modules directly
- **Built-in Python types** - Access Python's standard library and data types
- **Exception handling** - Python exceptions are properly propagated to NovaScript
- **Performance** - Leverages Python's optimized execution

### Python Integration Example
```nova
// Import and use Python modules (Python runtime only)
var os = require('os')
print("System: " + os.uname())

// Working with lists
var myList = [1, 2, 3, 4, 5]
table.insert(myList, 6)  // Add item to list
print("List: " + myList)

// Using math functions
var math = require('math')
print("Square root of 16: " + math.sqrt(16))
```

## Built-in Functions

- `print(...values)`: Prints values to the console
- `len(value)`: Returns the length of a string or array
- `type(value)`: Returns the type of a value as a string
- `toNumber(value)`: Converts a value to a number
- `toString(value)`: Converts a value to a string
- `range(start, end, step)`: Generates a sequence of numbers
- `map(collection, callback)`: Maps over a collection
- `filter(collection, callback)`: Filters a collection
- `reduce(collection, callback, initial)`: Reduces a collection

### File Processing
```nova
// Note: File operations are environment specific
// This example shows the concept, actual implementation may vary
func readFileSync(path)
    try
        // Implementation depends on the host environment
        // This is a conceptual example
        var content = "File content would be here"
        return content
    catch error
        print("Error reading file: " + error)
        return null
    end
try
    var content = readFileSync("example.txt")
    if content != null then
        print("File content: " + content)
    end
catch error
    print("Error: " + error)
end
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

MIT
