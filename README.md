# novascript

nova is a simple language with lua-like syntax,
the interpreter is entirely made in javascript, which means, you could potentialy run it on web,
if you remove some of the node-specific stuff, like `path` and `fs`, but you'd need to define a object that holds the data manualy


## simple test
```nova
namespace test
    var testing = false
    func x(x) // surprisingly, this function works :lol:
        print(isTesting(),isTesting)
        if isTesting()
            return x * 2
        end
        return x * 35
    end
end

var x = 52
print(x)
// scope test
for x=0, 20 do // sometimes, i ask myself, "why the fuck do i like lua-like syntax?"
    print(x+"|"+test.x(x))
end
print(x)

forEach thing in object.keys(test) do
    print(thing, test[thing])
end

var x = {"some name":"some value"}
var y = x["some name"]

print(x, y)
```
