#!/usr/bin/env node
/**
 * NovaScript 
 **/
import fs from "fs"
import path from "path"
// this will propagate to nova, since it uses js strings as well
/*
String.prototype.isNumber = function () {
    return !isNaN(this) && !isNaN(parseFloat(this));
};
*/

Object.defineProperty(String.prototype, 'isNumber', {
    value: function () {
        return !isNaN(this) && !isNaN(parseFloat(this));
    },
    writable: false,
    configurable: false,
    enumerable: false
});

function initGlobals(globals) {
    globals.define('print', console.log)
    const runtimeVersion = {
        major: 0,
        minor: 4,
        patch: 6
    }
    globals.define("isArray", Array.isArray)
    const args = process.argv.slice(2)
    globals.define("json", JSON) // don't know why js wants json to be upcased
    globals.define("parse", {
        int: parseInt,
        float: parseFloat,
        str: String
    })
    globals.define("void", undefined)
    globals.define('Runtime', {
        dump: {
            keys: Object.keys,
            values: Object.values
        },
        version: runtimeVersion,
        versionString: `v${runtimeVersion.major}.${runtimeVersion.minor}.${runtimeVersion.patch}`,
        currentDirectory: process.cwd,
        regex: (strPatt) => {
            return new RegExp(strPatt)
        },
        args: (id = undefined) => {
            if (id) return args[id]
            return args
        },
        exit: function (code = 0) {
            process.exit(code)
        },
        versionAtLeast: function (maj, min, pat) {
            if (runtimeVersion.major > maj) return true
            if (runtimeVersion.major < maj) return false

            if (runtimeVersion.minor > min) return true
            if (runtimeVersion.minor < min) return false

            return runtimeVersion.patch >= pat
        },
        env: (key = undefined) => {
            if (key) return process.env[key]
            return process.env
        },

        fs: { // instead of exposing the entire fs module, we expose just enough to not cause havoc
            read: function (path) {
                return fs.readFileSync(path, 'utf8')
            },
            write: function (path, contents) {
                fs.writeFileSync(path, contents, 'utf8')
            },
            exists: function (path) {
                return fs.existsSync(path)
            }
        },
        URI: {
            decode: (str) => decodeURIComponent(str),
            encode: (str) => encodeURIComponent(str)
        },
        time: {
            now: () => Date.now(),
            str: () => new Date(),
            hrtime: () => process.hrtime.bigint().toString()
        }
    })
}

/* A special exception used to implement returning values from functions */
class ReturnException extends Error {
    constructor(value) {
        super("Return");
        this.value = value;
    }
}
/* Special exceptions to implement break/continue */
class BreakException extends Error {
    constructor() { super("Break"); }
}
class ContinueException extends Error {
    constructor() { super("Continue"); }
}

/* Helper function for type checking */
function checkType(expected, value) {
    switch (expected) {
        case "number":
            if (typeof value !== "number") throw new Error(`Type mismatch: expected number, got ${typeof value}`);
            break;
        case "string":
            if (typeof value !== "string") throw new Error(`Type mismatch: expected string, got ${typeof value}`);
            break;
        case "boolean":
            if (typeof value !== "boolean") throw new Error(`Type mismatch: expected boolean, got ${typeof value}`);
            break;
        case "void":
            if (value !== undefined) throw new Error(`Type mismatch: expected void, got ${typeof value}`);
            break;
        default:
            throw new Error(`Unknown type: ${expected}`);
    }
    return value;
}

/* A simple environment for variable scoping */
class Environment {
    constructor(parent = null) {
        this.values = {};
        this.parent = parent;
    }
    define(name, value) {
        this.values[name] = value;
    }
    assign(name, value) {
        if (name in this.values) {
            this.values[name] = value;
        } else if (this.parent) {
            this.parent.assign(name, value);
        } else {
            throw new Error(`Undefined variable ${name}`);
        }
    }
    has(name) {
        if (name in this.values) {
            return true;
        } else if (this.parent) {
            return this.parent.has(name);
        } else {
            return false;
        }
    }
    get(name) {
        if (name in this.values) {
            return this.values[name];
        } else if (this.parent) {
            return this.parent.get(name);
        } else {
            throw new Error(`Undefined variable ${name}`);
        }
    }
}

export class Interpreter {
    keywords = [
        "var", "if", "else", "end", "break", "continue", "func",
        "return", "import", "as", "namespace", "while", "forEach", "for",
        "do", "in", "try", "errored",
        "switch", "case", "default", "using"
    ]
    constructor(filePath) {
        const source = fs.readFileSync(filePath,"utf8")
        this.source = source;
        this.file = filePath
        this.importedFiles = new Set();

        // Tokenize the source code into an array of tokens.
        this.tokens = this.tokenize(source);
        this.current = 0;
        // Global environment for variables and platform functions.
        this.globals = new Environment();
        // Storage for NovaScript function definitions.
        this.functions = {};
        initGlobals(this.globals);
    }
    currentEnv = null;


    // ----------------------
    // Tokenization (with comment support, booleans, logical operators, etc.)
    // ----------------------
    tokenize(source) {
        const tokens = [];
        let i = 0;
        const length = source.length;

        while (i < length) {
            let char = source[i];

            // Skip whitespace.
            if (/\s/.test(char)) {
                i++;
                continue;
            }

            // --- Comment support ---
            if (char === "/" && i + 1 < length && source[i + 1] === "/") {
                while (i < length && source[i] !== "\n") {
                    i++;
                }
                continue;
            }
            if (char === "/" && i + 1 < length && source[i + 1] === "*") {
                i += 2;
                while (i < length && !(source[i] === "*" && i + 1 < length && source[i + 1] === "/")) {
                    i++;
                }
                i += 2;
                continue;
            }

            // --- Logical operators (&& and ||) ---
            if (char === "&" && i + 1 < length && source[i + 1] === "&") {
                tokens.push({ type: "operator", value: "&&" });
                i += 2;
                continue;
            }
            if (char === "|" && i + 1 < length && source[i + 1] === "|") {
                tokens.push({ type: "operator", value: "||" });
                i += 2;
                continue;
            }

            // Numbers (supporting decimals)
            if (/[0-9]/.test(char)) {
                let num = "";
                while (i < length && /[0-9\.]/.test(source[i])) {
                    num += source[i];
                    i++;
                }
                tokens.push({ type: "number", value: parseFloat(num) });
                continue;
            }

            // Strings: delimited by double quotes.
            if (char === '"') {
                i++;
                let str = "";
                while (i < length && source[i] !== '"') {
                    if (source[i] === "\\" && i + 1 < length) {
                        i++;
                        str += source[i];
                    } else {
                        str += source[i];
                    }
                    i++;
                }
                i++;
                tokens.push({ type: "string", value: str });
                continue;
            }

            // Identifiers, keywords, booleans.
            if (/[A-Za-z_]/.test(char)) {
                let id = "";
                while (i < length && /[A-Za-z0-9_]/.test(source[i])) {
                    id += source[i];
                    i++;
                }
                if (id === "true" || id === "false") {
                    tokens.push({ type: "boolean", value: id === "true" });
                } else if (this.keywords.includes(id)) {
                    tokens.push({ type: "keyword", value: id });
                } else {
                    tokens.push({ type: "identifier", value: id });
                }
                continue;
            }

            // Multi-character operators: ==, !=, >=, <=.
            if (char === "=" || char === "!" || char === "<" || char === ">") {
                let op = char;
                if (i + 1 < length && source[i + 1] === "=") {
                    op += "=";
                    i += 2;
                } else {
                    i++;
                }
                tokens.push({ type: "operator", value: op });
                continue;
            }

            // Dot operator for property access.
            if (char === ".") {
                tokens.push({ type: "operator", value: "." });
                i++;
                continue;
            }

            // Single-character operators/punctuation.
            if ("#+-*/(),{}[]:".includes(char)) {
                tokens.push({ type: "operator", value: char });
                i++;
                continue;
            }

            throw new Error(`Unexpected character: ${char}`);
        }
        return tokens;
    }

    // ----------------------
    // Token Parser Helpers
    // ----------------------
    getNextToken() {
        return this.tokens[this.current] || null;
    }

    consumeToken() {
        return this.tokens[this.current++];
    }

    expectType(type) {
        const token = this.getNextToken();
        if (!token || token.type !== type) {
            throw new Error(`Expected token type ${type}, got ${token ? token.type : "EOF"}`);
        }
        return token;
    }

    expectToken(value) {
        const token = this.getNextToken();
        if (!token || token.value !== value) {
            throw new Error(`Expected token '${value}', got ${token ? token.value : "EOF"}`);
        }
        return token;
    }

    // ----------------------
    // Parsing Helpers
    // ----------------------
    parseBlockUntil(terminators = []) {
        const statements = [];
        while (this.current < this.tokens.length) {
            const token = this.getNextToken();
            if (
                (token.type === "keyword" && terminators.includes(token.value)) ||
                (token.type === "operator" && terminators.includes(token.value))
            ) {
                break;
            }
            statements.push(this.parseStatement());
        }
        return statements;
    }

    parseBlock() {
        return this.parseBlockUntil();
    }

    // ----------------------
    // Parsing Statements and Expressions
    // ----------------------
    parseStatement() {
        const token = this.getNextToken();

        // --- Label statement ---
        if (token.type === "keyword" && token.value === "label") {
            this.consumeToken();
            const nameToken = this.expectType("identifier");
            const name = nameToken.value;
            this.consumeToken();
            return { type: "LabelStmt", name };
        }

        // --- Variable declaration with optional type annotation ---
        if (token.type === "keyword" && token.value === "var") {
            this.consumeToken();
            const nameToken = this.expectType("identifier");
            const name = nameToken.value;
            this.consumeToken();

            // Optional type annotation (must be one of the allowed types)
            let typeAnnotation = null;
            if (
                this.getNextToken() &&
                this.getNextToken().type === "identifier" &&
                ["string", "number", "boolean", "void"].includes(this.getNextToken().value)
            ) {
                typeAnnotation = this.getNextToken().value;
                this.consumeToken();
            }

            // Optional modifier (e.g., #global)
            let modifier = null;
            if (
                this.getNextToken() &&
                this.getNextToken().type === "operator" &&
                this.getNextToken().value === "#"
            ) {
                this.consumeToken();
                const modToken = this.expectType("identifier");
                modifier = modToken.value;
                this.consumeToken();
            }

            this.expectToken("=");
            this.consumeToken();
            const initializer = this.parseExpression();
            return { type: "VarDecl", name, typeAnnotation, initializer, modifier };
        }

        // --- Switch statement ---
        if (token.type === "keyword" && token.value === "switch") {
            this.consumeToken();
            const expression = this.parseExpression();
            const cases = [];
            while (this.getNextToken().type === "keyword" && this.getNextToken().value === "case") {
                this.consumeToken();
                const caseExpr = this.parseExpression();
                this.expectToken("do");
                this.consumeToken();
                const body = this.parseBlockUntil(["end"]);
                this.expectToken("end");
                this.consumeToken();
                cases.push({ caseExpr, body });
            }
            if (this.getNextToken().type === "keyword" && this.getNextToken().value === "default") {
                this.consumeToken();
                this.expectToken("do");
                this.consumeToken();
                const body = this.parseBlockUntil(["end"]);
                this.expectToken("end");
                this.consumeToken();
                cases.push({ caseExpr: null, body });
            }

            this.expectToken("end");
            this.consumeToken();

            return { type: "SwitchStmt", expression, cases };
        }

        // --- using statement ---
        if (token.type === "keyword" && token.value === "using") {
            //this should take everything from a namespace or module, and push them into the current context
            this.consumeToken();
            const nameToken = this.expectType("identifier");
            this.consumeToken();

            return { type: "UsingStmt", name:nameToken.value };
        }

        // --- Try/Catch statement ---
        if (token.type === "keyword" && token.value === "try") {
            this.consumeToken();
            const tryBlock = this.parseBlockUntil(["errored"]);
            this.expectToken("errored");
            this.consumeToken();
            const errorVarToken = this.expectType("identifier");
            const errorVar = errorVarToken.value;
            this.consumeToken();
            const catchBlock = this.parseBlockUntil(["end"]);
            this.expectToken("end");
            this.consumeToken();
            return { type: "TryStmt", tryBlock, errorVar, catchBlock };
        }

        // --- ForEach loop ---
        if (token.type === "keyword" && token.value === "forEach") {
            this.consumeToken();
            const varToken = this.expectType("identifier");
            const variable = varToken.value;
            this.consumeToken();
            this.expectToken("in");
            this.consumeToken();
            const listExpr = this.parseExpression();
            this.expectToken("do");
            this.consumeToken();
            const body = this.parseBlockUntil(["end"]);
            this.expectToken("end");
            this.consumeToken();
            return { type: "ForEachStmt", variable, list: listExpr, body };
        }

        // --- For loop ---
        if (token.type === "keyword" && token.value === "for") {
            this.consumeToken();
            const varToken = this.expectType("identifier");
            const variable = varToken.value;
            this.consumeToken();
            this.expectToken("=");
            this.consumeToken();
            const startExpr = this.parseExpression();
            this.expectToken(",");
            this.consumeToken();
            const endExpr = this.parseExpression();
            let stepExpr = undefined;
            if (this.getNextToken() && this.getNextToken().value === ",") {
                this.consumeToken();
                stepExpr = this.parseExpression();
            }
            this.expectToken("do");
            this.consumeToken();
            const body = this.parseBlockUntil(["end"]);
            this.expectToken("end");
            this.consumeToken();
            return { type: "ForStmt", variable, start: startExpr, end: endExpr, step: stepExpr, body };
        }

        // --- While loop ---
        if (token.type === "keyword" && token.value === "while") {
            this.consumeToken();
            const condition = this.parseExpression();
            const body = this.parseBlockUntil(["end"]);
            this.expectToken("end");
            this.consumeToken();
            return { type: "WhileStmt", condition, body };
        }

        // --- Break statement ---
        if (token.type === "keyword" && token.value === "break") {
            this.consumeToken();
            return { type: "BreakStmt" };
        }
        // --- Continue statement ---
        if (token.type === "keyword" && token.value === "continue") {
            this.consumeToken();
            return { type: "ContinueStmt" };
        }


        // --- If statement ---
        if (token.type === "keyword" && token.value === "if") {
            this.consumeToken();
            const condition = this.parseExpression();
            const thenBlock = this.parseBlockUntil(["else", "end"]);
            let elseBlock = null;
            if (this.getNextToken() && this.getNextToken().type === "keyword" && this.getNextToken().value === "else") {
                this.consumeToken();
                elseBlock = this.parseBlockUntil(["end"]);
            }
            this.expectToken("end");
            this.consumeToken();
            return { type: "IfStmt", condition, thenBlock, elseBlock };
        }

        // --- namespace ---
        if (token.type === "keyword" && token.value === "namespace") {
            this.consumeToken();
            const nameToken = this.expectType("identifier");
            const name = nameToken.value;
            this.consumeToken();
            const body = this.parseBlockUntil(["end"]);
            this.expectToken("end");
            this.consumeToken();
            return { type: "NamespaceStmt", name, body };
        }
        
        if (token.type === "keyword" && token.value === "func") {
            this.consumeToken();
            const nameToken = this.expectType("identifier");
            const name = nameToken.value;
            this.consumeToken();
            this.expectToken("(");
            this.consumeToken();

            const parameters = [];
            if (this.getNextToken() && this.getNextToken().value !== ")") {
                while (true) {
                    const paramToken = this.expectType("identifier");
                    const paramName = paramToken.value;
                    this.consumeToken();

                    let paramType = null;
                    let defaultExpr = undefined;

                    // optional type annotation
                    if (this.getNextToken()?.type === "identifier") {
                        const typeToken = this.getNextToken();
                        // simple heuristic: if it's a known type name, treat it as a type
                        if (["string", "number", "bool"].includes(typeToken.value)) {
                            paramType = typeToken.value;
                            this.consumeToken();
                        }
                    }

                    // optional default value
                    if (this.getNextToken()?.value === "=") {
                        this.consumeToken();
                        defaultExpr = this.parseExpression();
                        if(paramType){
                            throw new Error("cannot have both type and default value, as that prevents type infering");
                        }
                        paramType = defaultExpr.value.type;

                    }

                    parameters.push({ name: paramName, type: paramType, default: defaultExpr });

                    if (this.getNextToken()?.value === ",") {
                        this.consumeToken();
                    } else {
                        break;
                    }
                }
            }

            this.expectToken(")");
            this.consumeToken();
            const body = this.parseBlockUntil(["end"]);
            this.expectToken("end");
            this.consumeToken();

            return {
                type: "FuncDecl",
                name,
                parameters,
                body
            };
        }

        // --- Return statement ---
        if (token.type === "keyword" && token.value === "return") {
            this.consumeToken();
            let expression = null;
            if (this.getNextToken() && this.getNextToken().value !== "\n") {
                expression = this.parseExpression();
            }
            return { type: "ReturnStmt", expression };
        }

        // --- Import statement ---
        if (token.type === "keyword" && token.value === "import") {
            this.consumeToken();
            const fileToken = this.expectType("string");
            const filename = fileToken.value;
            this.consumeToken();
            let alias = null;
            if (this.getNextToken() && this.getNextToken().value === "as") {
                this.consumeToken();
                const aliasToken = this.expectType("identifier");
                alias = aliasToken.value;
                this.consumeToken();
            }
            return { type: "ImportStmt", filename, alias };
        }

        // --- Expression statement (fallback) ---
        const expr = this.parseExpression();
        return { type: "ExpressionStmt", expression: expr };
    }

    // --- Expression Parsing (Recursive Descent) ---
    parseExpression() {
        return this.parseAssignment();
    }

    parseAssignment() {
        let expr = this.parseLogicalOr();
        if (
            this.getNextToken() &&
            this.getNextToken().type === "operator" &&
            this.getNextToken().value === "="
        ) {
            this.consumeToken();
            const valueExpr = this.parseAssignment();
            expr = { type: "AssignmentExpr", target: expr, value: valueExpr };
        }
        return expr;
    }

    parseLogicalOr() {
        let expr = this.parseLogicalAnd();
        while (
            this.getNextToken() &&
            this.getNextToken().type === "operator" &&
            this.getNextToken().value === "||"
        ) {
            const operator = this.consumeToken().value;
            const right = this.parseLogicalAnd();
            expr = { type: "BinaryExpr", operator, left: expr, right };
        }
        return expr;
    }

    parseLogicalAnd() {
        let expr = this.parseEquality();
        while (
            this.getNextToken() &&
            this.getNextToken().type === "operator" &&
            this.getNextToken().value === "&&"
        ) {
            const operator = this.consumeToken().value;
            const right = this.parseEquality();
            expr = { type: "BinaryExpr", operator, left: expr, right };
        }
        return expr;
    }

    parseEquality() {
        let expr = this.parseComparison();
        while (
            this.getNextToken() &&
            this.getNextToken().type === "operator" &&
            (this.getNextToken().value === "==" || this.getNextToken().value === "!=")
        ) {
            const operator = this.consumeToken().value;
            const right = this.parseComparison();
            expr = { type: "BinaryExpr", operator, left: expr, right };
        }
        return expr;
    }

    parseComparison() {
        let expr = this.parseTerm();
        while (
            this.getNextToken() &&
            this.getNextToken().type === "operator" &&
            ["<", "<=", ">", ">="].includes(this.getNextToken().value)
        ) {
            const operator = this.consumeToken().value;
            const right = this.parseTerm();
            expr = { type: "BinaryExpr", operator, left: expr, right };
        }
        return expr;
    }

    parseTerm() {
        let expr = this.parseFactor();
        while (
            this.getNextToken() &&
            this.getNextToken().type === "operator" &&
            (this.getNextToken().value === "+" || this.getNextToken().value === "-")
        ) {
            const operator = this.consumeToken().value;
            const right = this.parseFactor();
            expr = { type: "BinaryExpr", operator, left: expr, right };
        }
        return expr;
    }

    parseFactor() {
        let expr = this.parseUnary();
        while (
            this.getNextToken() &&
            this.getNextToken().type === "operator" &&
            (this.getNextToken().value === "*" || this.getNextToken().value === "/")
        ) {
            const operator = this.consumeToken().value;
            const right = this.parseUnary();
            expr = { type: "BinaryExpr", operator, left: expr, right };
        }
        return expr;
    }

    parseUnary() {
        if (
            this.getNextToken() &&
            this.getNextToken().type === "operator" &&
            (this.getNextToken().value === "-" || this.getNextToken().value === "!")
        ) {
            const operator = this.consumeToken().value;
            const right = this.parseUnary();
            return { type: "UnaryExpr", operator, right };
        }
        return this.parsePrimary();
    }

    parsePrimary() {
        let node;
        const token = this.getNextToken();
        if (!token) {
            throw new Error("Unexpected end of input");
        }

        if (token.type === "boolean") {
            this.consumeToken();
            node = { type: "Literal", value: token.value };
        }
        else if (token.type === "number" || token.type === "string") {
            this.consumeToken();
            node = { type: "Literal", value: token.value };
        }
        else if (token.value === "[") {
            this.consumeToken();
            const elements = [];
            if (this.getNextToken() && this.getNextToken().value !== "]") {
                while (true) {
                    elements.push(this.parseExpression());
                    if (this.getNextToken() && this.getNextToken().value === ",") {
                        this.consumeToken();
                    } else {
                        break;
                    }
                }
            }
            this.expectToken("]");
            this.consumeToken();
            node = { type: "ArrayLiteral", elements };
        }
        else if (token.value === "{") {
            this.consumeToken();
            const properties = [];
            if (this.getNextToken() && this.getNextToken().value !== "}") {
                while (true) {
                    let keyToken = this.getNextToken();
                    if (keyToken.type !== "identifier" && keyToken.type !== "string") {
                        throw new Error("Expected identifier or string as object key");
                    }
                    const key = keyToken.value;
                    this.consumeToken();
                    this.expectToken(":");
                    this.consumeToken();
                    const value = this.parseExpression();
                    properties.push({ key, value });
                    if (this.getNextToken() && this.getNextToken().value === ",") {
                        this.consumeToken();
                    } else {
                        break;
                    }
                }
            }
            this.expectToken("}");
            this.consumeToken();
            node = { type: "ObjectLiteral", properties };
        }
        else if (token.type === "identifier") {
            this.consumeToken();
            if (this.getNextToken() && this.getNextToken().value === "[") {
                // array access
                this.consumeToken();
                const index = this.parseExpression();
                this.expectToken("]");
                this.consumeToken();
                node = { type: "ArrayAccess", name: token.value, index };
            }
            else if (this.getNextToken() && this.getNextToken().value === "(") {
                this.consumeToken();
                const args = [];
                if (this.getNextToken() && this.getNextToken().value !== ")") {
                    while (true) {
                        args.push(this.parseExpression());
                        if (this.getNextToken() && this.getNextToken().value === ",") {
                            this.consumeToken();
                        } else {
                            break;
                        }
                    }
                }
                this.expectToken(")");
                this.consumeToken();
                node = { type: "FuncCall", name: token.value, arguments: args };
            } else {
                node = { type: "Identifier", name: token.value };
            }
        }
        else if (token.value === "(") {
            this.consumeToken();
            node = this.parseExpression();
            this.expectToken(")");
            this.consumeToken();
        }
        else {
            throw new Error(`Unexpected token: ${JSON.stringify(token)}`);
        }

        while (this.getNextToken() && this.getNextToken().value === ".") {
            this.consumeToken();                             // consume "."
            const propToken = this.expectType("identifier");
            const propName = propToken.value;
            this.consumeToken();                             // consume identifier

            // method-call?
            if (this.getNextToken() && this.getNextToken().value === "(") {
                this.consumeToken();                           // consume "("
                const args = [];
                while (this.getNextToken() && this.getNextToken().value !== ")") {
                    args.push(this.parseExpression());
                    if (this.getNextToken().value === ",") this.consumeToken();
                    else break;
                }
                this.expectToken(")");
                this.consumeToken();                           // consume ")"

                node = {
                    type: "MethodCall",
                    object: node,        // the `x` part of `x.foo(...)`
                    method: propName,    // the `"foo"`
                    arguments: args
                };
            } else {
                // plain property access
                node = {
                    type: "PropertyAccess",
                    object: node,
                    property: propName
                };
            }
        }

        return node;
    }

    // ----------------------
    // Evaluation / Execution
    // ----------------------
    interpret() {
        const statements = this.parseBlock();
        try {
            this.executeBlock(statements, this.globals);
        } catch (err) {
            console.error("Uncaught error:", err);
        }
    }

    executeBlock(statements, env) {
        const previousEnv = this.currentEnv;
        this.currentEnv = env;

        for (const stmt of statements) {
            this.executeStmt(stmt, env);
        }

        this.currentEnv = previousEnv;
    }

    getCurrentContext() {
        return this.currentEnv;
    }
    executeStmt(stmt, env) {
        switch (stmt.type) {
            case "VarDecl": {
                const value = this.evaluateExpr(stmt.initializer, env);
                if (stmt.typeAnnotation) {
                    checkType(stmt.typeAnnotation, value);
                }
                env.define(stmt.name, value);
                break;
            }
            case "ExpressionStmt": {
                this.evaluateExpr(stmt.expression, env);
                break;
            }
            case "BreakStmt":
                throw new BreakException();

            case "ContinueStmt":
                throw new ContinueException();

            case "TryStmt": {
                try {
                    this.executeBlock(stmt.tryBlock, env);
                } catch (e) {
                    const catchEnv = new Environment(env);
                    catchEnv.define(stmt.errorVar, e);
                    this.executeBlock(stmt.catchBlock, catchEnv);
                }
                break;
            }
            case "IfStmt": {
                const condition = this.evaluateExpr(stmt.condition, env);
                if (condition) {
                    this.executeBlock(stmt.thenBlock, new Environment(env));
                } else if (stmt.elseBlock) {
                    this.executeBlock(stmt.elseBlock, new Environment(env));
                }
                break;
            }
            case "WhileStmt": {
                while (this.evaluateExpr(stmt.condition, env)) {
                    try {
                        this.executeBlock(stmt.body, new Environment(env));
                    }
                    catch (e) {
                        if (e instanceof BreakException) { break; }
                        if (e instanceof ContinueException) { continue; }
                        throw e;
                    }
                }
                break;
            }
            case "ForEachStmt": {
                const list = this.evaluateExpr(stmt.list, env);
                for (const item of list) {
                    const loopEnv = new Environment(env);
                    loopEnv.define(stmt.variable, item);
                    try {
                        this.executeBlock(stmt.body, loopEnv);
                    }
                    catch (e) {
                        if (e instanceof BreakException) { break; }
                        if (e instanceof ContinueException) { continue; }
                        throw e;
                    }
                }
                break;
            }

            case "ForStmt": {
                for (let i = start; i <= end; i += step) {
                    const loopEnv = new Environment(env);
                    loopEnv.define(stmt.variable, i);
                    try {
                        this.executeBlock(stmt.body, loopEnv);
                    }
                    catch (e) {
                        if (e instanceof BreakException) { break; }
                        if (e instanceof ContinueException) { continue; }
                        throw e;
                    }
                }
                break;
            }

            case "ImportStmt": {
                let filePath = stmt.filename;
                if (stmt.filename.startsWith("js:")) {
                    filePath = filePath.substring(3);
                    if (!env.has("js-import-handler")) {
                        throw new Error("js-import-handler is not defined, your runtime should define it, interpreter.globals.define('js-import-handler', handler)");
                    }

                    const handler = env.get("js-import-handler");
                    const result = handler(filePath);
                    let name = stmt.alias || filePath
                    //this.globals.define(name, result);
                    env.define(name, result);
                    break;
                }
                filePath+=".nova";
                const fileDir = path.dirname(this.file)

                if (this.importedFiles.has(filePath)) break;
                this.importedFiles.add(filePath);

                const fullPath = path.resolve(fileDir,filePath);
                // Evaluate in isolated environment
                const importedInterpreter = new Interpreter(fullPath);
                const importedEnv = new Environment(this.globals);
                importedInterpreter.globals = importedEnv;

                importedInterpreter.functions = this.functions;
                importedInterpreter.importedFiles = this.importedFiles;

                importedInterpreter.interpret();

                // Create namespace object
                const namespace = {};
                for (const key in importedEnv.values) {
                    namespace[key] = importedEnv.values[key];
                }

                // Use filename (minus extension) as variable name
                const pathObj = path.parse(filePath);
                const moduleName = pathObj.name; // e.g., "math" from "math.nova"
                let name = stmt.alias || moduleName
                //this.globals.define(name, namespace);
                env.define(name, namespace);
                //this.globals.define(moduleName, namespace);
                break;
            }

            case "NamespaceStmt": {
                const nenv = new Environment(env);
                this.executeBlock(stmt.body, nenv);

                // Store the actual env, not a plain object
                env.define(stmt.name, nenv);
                break;
            }
            case "SwitchStmt": {
                const value = this.evaluateExpr(stmt.expression, env);
                let matched = false;

                for (const c of stmt.cases) {
                    // Default case
                    if (c.caseExpr === null) {
                        if (!matched) {
                            this.executeBlock(c.body, new Environment(env));
                            break;
                        }
                    } else {
                        const caseVal = this.evaluateExpr(c.caseExpr, env);
                        if (value === caseVal) {
                            this.executeBlock(c.body, new Environment(env));
                            matched = true;
                            break;
                        }
                    }
                }
                break;
            }


            case "ReturnStmt": {
                const value = stmt.expression ? this.evaluateExpr(stmt.expression, env) : undefined;
                throw new ReturnException(value);
            }

            case "FuncDecl": {
                const func = (...args) => {
                    const funcEnv = new Environment(env);

                    for (let i = 0; i < stmt.parameters.length; i++) {
                        const param = stmt.parameters[i];
                        let argVal = args[i];

                        // apply default if missing
                        if (argVal === undefined && param.default !== undefined) {
                            argVal = this.evaluateExpr(param.default, env);
                        }

                        // soft type check
                        if (param.type) {
                            const type = param.type;
                            const actualType = typeof argVal;

                            if (type === "number" && actualType !== "number") {
                                throw new Error(`Type mismatch in function '${stmt.name}': parameter '${param.name}' expected number, got ${actualType}`);
                            } else if (type === "string" && actualType !== "string") {
                                throw new Error(`Type mismatch in function '${stmt.name}': parameter '${param.name}' expected string, got ${actualType}`);
                            } else if (type === "bool" && actualType !== "boolean") {
                                throw new Error(`Type mismatch in function '${stmt.name}': parameter '${param.name}' expected bool, got ${actualType}`);
                            }
                            // more types? add here
                        }

                        funcEnv.define(param.name, argVal);
                    }

                    try {
                        this.executeBlock(stmt.body, funcEnv);
                    } catch (e) {
                        if (e instanceof ReturnException) {
                            return e.value;
                        } else {
                            throw e;
                        }
                    }
                };

                env.define(stmt.name, func);
                break;
            }

            case "UsingStmt":{
                const namespace = env.get(stmt.name)
                for(const [key,value] of Object.entries(namespace)){
                    env.define(key,value)
                }
            } break

            default:
                throw new Error(`Unknown statement type: ${stmt.type}\n${JSON.stringify(stmt, null, 2)}`);
        }
    }

    expandArrayTarget(expr, env) {
        // expr: { type: "ArrayAccess", name: "roomsVisited", index: {...} }

        const arrayName = expr.name;
        const indexExpr = expr.index;

        const arr = env.get(arrayName);
        const index = this.evaluateExpr(indexExpr, env);

        if (!Array.isArray(arr) && typeof arr !== "object") {
            throw new Error(`Cannot index into non-array: ${typeof arr}`);
        }

        return { arr, index };
    }

    expandPropTarget(expr, env) {
        let current = expr;

        // Walk down the left side to resolve the full object chain
        const chain = [];

        while (current.type === "PropertyAccess") {
            chain.unshift(current.property); // collect property names in reverse
            current = current.object;
        }

        if (current.type !== "Identifier") {
            throw new Error("Invalid base for property access: " + current.type);
        }

        // Resolve the base object from the environment
        let obj = env.get(current.name);

        // Walk through all but the last key to get to the parent
        for (let i = 0; i < chain.length - 1; i++) {
            const key = chain[i];
            if (obj == null || typeof obj !== 'object') {
                throw new Error("Cannot access property '" + key + "' on non-object");
            }
            obj = obj[key];
        }

        return {
            obj,
            key: chain[chain.length - 1] // the last key is what we’re assigning to
        };
    }


    evaluateExpr(expr, env) {
        //console.log(expr)
        switch (expr.type) {
            case "Literal":
                return expr.value;
            case "Identifier":
                return env.get(expr.name);
            case "AssignmentExpr": {
                const value = this.evaluateExpr(expr.value, env);
                /*
                ArrayAccess({
                  "type": "ArrayAccess",
                  "name": "roomsVisited",
                  "index": {
                    "type": "Identifier",
                    "name": "name"
                  }
                })*/
                if(expr.target.type == "ArrayAccess"){
                    const { arr, index } = this.expandArrayTarget(expr.target, env);
                    arr[index] = value;
                }else if (expr.target.type === "PropertyAccess") {
                    const { obj, key } = this.expandPropTarget(expr.target, env);
                    obj[key] = value;
                } else if (expr.target.type === "Identifier") {
                    env.assign(expr.target.name, value);
                } else {
                    throw new Error("Unsupported assignment target: "+`${expr.target.type}(${JSON.stringify(expr.target, null, 2)})`);
                }

                return value;
            }

            case "BinaryExpr": {
                const left = this.evaluateExpr(expr.left, env);
                const right = this.evaluateExpr(expr.right, env);
                switch (expr.operator) {
                    case "+": return left + right;
                    case "-": return left - right;
                    case "*": return left * right;
                    case "/": return left / right;
                    case "==": return left === right;
                    case "!=": return left !== right;
                    case "<": return left < right;
                    case "<=": return left <= right;
                    case ">": return left > right;
                    case ">=": return left >= right;
                    case "&&": return left && right;
                    case "||": return left || right;
                    default: throw new Error(`Unknown binary operator: ${expr.operator}`);
                }
            }
            case "UnaryExpr": {
                const right = this.evaluateExpr(expr.right, env);
                switch (expr.operator) {
                    case "-": return -right;
                    case "!": return !right;
                    default: throw new Error(`Unknown unary operator: ${expr.operator}`);
                }
            }
            case "FuncCall": {
                const func = env.get(expr.name);
                if (typeof func !== "function") {
                    throw new Error(`${expr.name} is not a function`);
                }
                const args = expr.arguments.map(arg => this.evaluateExpr(arg, env));
                return func(...args);
            }
            case "MethodCall": {
                const obj = this.evaluateExpr(expr.object, env);
                const fn = (obj instanceof Environment) ? obj.get(expr.method) : obj[expr.method];
                
                if (typeof fn !== "function") {
                    throw new Error(`${expr.method} is not a function`);
                }

                const argVals = expr.arguments.map(arg => this.evaluateExpr(arg, env));
                return fn.apply(obj, argVals);
            }

            case "ArrayAccess": {
                const arr = env.get(expr.name)
                const index = this.evaluateExpr(expr.index, env);
                //console.log(arr, expr.index, index, arr[index]);
                return arr[index];
            }
            case "PropertyAccess": {
                const obj = this.evaluateExpr(expr.object, env);
                
                if (obj instanceof Environment) {
                    return obj.get(expr.property);
                }
                
                return obj[expr.property];
            }

            case "ArrayLiteral": {
                return expr.elements.map(element => this.evaluateExpr(element, env));
            }
            case "ObjectLiteral": {
                const obj = {};
                for (const prop of expr.properties) {
                    obj[prop.key] = this.evaluateExpr(prop.value, env);
                }
                return obj;
            }
            default:
                throw new Error(`Unknown expression type: ${expr.type}`);
        }
    }
}
