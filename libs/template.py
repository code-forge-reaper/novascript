#!/usr/bin/env python
# This is a simple template engine
# you'd normally use this in like
# a server or for simple stuff like the example bellow

# don't even think about bitching about exec eval
# you wrote/downloaded the template, it is your responsibility
# to know what it does

from dataclasses import dataclass
import re


"""
example:

@doc: sample.html
@title: sample format testing

<?this is like Latex, basically?>
${import numpy as np}

${mat = np.array([[52,12],
                 [24,41]])
}

This is a simple test

<ul>
${
for i in range(20):
    write(f"<li key='{i}'>{mat * i}</li>")    
}
</ul>

"""


@dataclass
class Ctx:
    source: str
    index: int
    doc = {
        "header": {"name": "", "title": ""},
        "content": "",
        "state": {},  # where eval can just dump stuff into
    }

    def write(self, s):
        self.doc["content"] += s

    def __post_init__(self):
        self.doc = {"header": {"name": "", "title": ""}, "content": "", "state": {}}
        self.doc["state"]["write"] = self.write

        self.doc["state"]["load"] = load
        self.doc["state"]["genHtml"] = genHtml

    def moveIndex(self, offset: int):
        self.index += offset

    def consumeNextChar(self):
        char = self.source[self.index]
        if char == "\\":
            v = self.nextChar(1)
            self.moveIndex(2)
            return v
        else:
            self.moveIndex(1)
            return char

    def nextChar(self, i=0):
        return self.source[self.index + i]

    def skipSpace(self):
        while self.isWhiteSpace():
            self.moveIndex(1)

    def isWhiteSpace(self):
        return self.source[self.index].isspace()

    def isEnd(self):
        return self.index >= len(self.source)

    def isNum(self):
        return self.source[self.index].isdigit()

    def isAlpha(self):
        return self.source[self.index].isalpha()

    def isChar(self, c):
        return self.source[self.index] == c

    def getString(self):
        self.moveIndex(1)

        output = ""

        while not self.isChar('"'):
            output += self.source[self.index]
            self.moveIndex(1)

        self.moveIndex(1)

        return output


# @doc: sample.html
def handle_at(context: Ctx):
    context.moveIndex(1)

    name = ""
    output = ""
    while not context.isChar(":"):
        name += context.source[context.index]
        context.moveIndex(1)

    context.moveIndex(1)  # skip ":"
    context.skipSpace()
    while not context.isChar("\n"):
        output += context.source[context.index]
        context.moveIndex(1)

    context.doc["header"][name] = output


def handle_text(context: Ctx):
    output = ""

    while (
        not context.isEnd()
        and not context.isChar("\n")
        and not (context.isChar("!") and context.nextChar(1) == "{")
        and not (context.isChar("$") and context.nextChar(1) == "{")
    ):
        e = context.consumeNextChar()
        output += e

    context.write(output)


def handle_embedded_stmt(context: Ctx):
    "${...}"
    braceCount = 1
    context.moveIndex(2)
    string = ""
    while braceCount > 0:
        if context.isChar("{"):
            braceCount += 1
        elif context.isChar("}"):
            braceCount -= 1
        if braceCount > 0:
            string += context.source[context.index]
        context.moveIndex(1)

    exec(string, context.doc["state"])


def handle_embedded_expr(context: Ctx):
    "!{...}"
    braceCount = 1  # Why? because this allows this: "!{c = {'age': 42}}"
    context.moveIndex(2)
    string = ""
    while braceCount > 0:
        if context.isChar("{"):
            braceCount += 1
        elif context.isChar("}"):
            braceCount -= 1
        if braceCount > 0:
            string += context.source[context.index]
        context.moveIndex(1)

    c = eval(string, context.doc["state"])
    if c is not None:
        context.write(str(c))


# you can use this for like, profiles
# you don't need the whole "@doc" "@title" thing
# if you just need one specific template to be rendered
"""
<div>
    <h1>name: !{name}</h1>
    <h2>age: !{age}</h2>
</div>
"""


def load(content, state=None):
    if state == None:
        state = {}
    context = Ctx(re.sub(r"<\?(.*?)\?>", "", content, flags=re.DOTALL), 0)
    context.doc["state"].update(state)
    # print(context)
    while not context.isEnd():
        # print("current: ", repr(context.nextChar()))
        if context.isChar("\n"):
            if not context.doc["content"].endswith(
                "\n"
            ):  # try to avoid spamming newlines
                context.doc["content"] += "\n"
            context.moveIndex(1)
        elif context.isWhiteSpace():
            context.write(context.nextChar())
            context.moveIndex(1)
        elif context.isChar("@"):
            handle_at(context)
        elif context.isChar("!") and context.nextChar(1) == "{":
            handle_embedded_expr(context)
        elif context.isChar("$") and context.nextChar(1) == "{":
            handle_embedded_stmt(context)
        else:
            handle_text(context)
    return context.doc


def genHtml(doc):
    content = "<!DOCTYPE html>\n"
    content += "<html>\n"
    content += "<head>\n"
    content += f"<title>{doc['header']['title']}</title>\n"
    content += "</head>\n"
    content += "<body>\n"
    content += doc["content"]
    content += "</body>\n"
    content += "</html>\n"
    return content


if __name__ == "__main__":
    import sys

    if len(sys.argv) != 2:
        print("Usage: python main.py <file>")
        sys.exit(1)
    with open(sys.argv[1], "r") as f:
        content = f.read()
        if not content.endswith("\n"):
            content += "\n"
        doc = load(content)

    with open(doc["header"]["doc"], "w") as f:
        f.write(genHtml(doc))

    print("Wrote", doc["header"]["doc"])
