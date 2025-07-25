#!/usr/bin/env node
/**
 * NovaScript
 **/
import fs from "node:fs";
import path from "node:path";
import util from "node:util";
interface Token {
    type: string;
    // @ts-ignore
    value: any;
    file: string
    line: number
    column: number
}

interface RuntimeVersion {
    major: number;
    minor: number;
    patch: number;
}

// Statement and Expression now extend Token to carry location information
interface Statement extends Token {
    type: string;
    [key: string]: any;
}

interface Expression extends Token {
    type: string;
    [key: string]: any;
}

// MODIFIED: Parameter interface to avoid 'type' conflict
interface Parameter extends Token {
    name: string;
    annotationType: string | null; // Renamed from 'type' to avoid conflict with Token.type
    default?: Expression;
}

interface Case {
    caseExpr: any;
    body: Statement[];
}

interface FunctionCall extends Expression {
    type: "FuncCall";
    name: string;
    arguments: Expression[];
}

interface MethodCall extends Expression {
    type: "MethodCall";
    object: Expression;
    method: string;
    arguments: Expression[];
}

interface PropertyAccess extends Expression {
    type: "PropertyAccess";
    object: Expression;
    property: string;
}

// MODIFIED: ArrayAccess now takes an Expression for 'object' instead of 'name: string'
interface ArrayAccess extends Expression {
    type: "ArrayAccess";
    object: Expression; // Changed from 'name: string'
    index: Expression;
}

interface Literal extends Expression {
    type: "Literal";
    value: any;
}
interface DeferStmt extends Statement {
    type: "DeferStmt";
    body: Statement[];
}

// NEW: VarDeclStmt interface for clearer type annotation property
interface VarDeclStmt extends Statement {
    type: "VarDecl";
    name: string;
    typeAnnotation: string | null;
    initializer: Expression;
}

// NEW: Interfaces for Custom Types
interface CustomTypeProperty {
    name: string;
    type: string; // The NovaScript type name (e.g., "number", "string", "rect")
}

interface CustomType {
    name: string;
    properties: CustomTypeProperty[];
    file: string;
    line: number;
    column: number;
}

// NEW: Statement for Custom Type Declaration
interface CustomTypeDeclStmt extends Statement {
    type: "CustomTypeDecl";
    name: string;
    definition: CustomTypeProperty[];
}

// MODIFIED: AssignmentExpr interface to include 'operator' for compound assignments
interface AssignmentExpr extends Expression {
    type: "AssignmentExpr";
    target: Expression;
    value: Expression;
    operator: string; // Added for compound assignments (e.g., "+=", "-=", or "=" for simple assignment)
}

// Existing ArrayLiteral interface
interface ArrayLiteral extends Expression {
    type: "ArrayLiteral";
    elements: Expression[];
}

// Existing Identifier interface (for clarity in assignment target resolution)
interface Identifier extends Expression {
    type: "Identifier";
    name: string;
}


/* A special exception used to implement returning values from functions */
export class ReturnException extends Error {
    value: any;
    constructor(value: any) {
        super("Return");
        this.value = value;
    }
}

/* Special exceptions to implement break/continue */
export class BreakException extends Error {
    constructor() { super("Break"); }
}

export class ContinueException extends Error {
    constructor() { super("Continue"); }
}

// NovaError now handles potential null/undefined tokens more robustly
export class NovaError extends Error {
    line: number
    column: number
    file: string // Added file property
    constructor(token: Token | null | undefined, userMessage?: string) {
        // Provide fallback values if token is null/undefined or lacks properties
        const file = token?.file || "unknown_file";
        const line = token?.line || 0;
        const column = token?.column || 0;
        const tokenValue = token?.value !== undefined ? token.value : 'N/A';

        const message = `${file}:${line}:${column} ${userMessage || 'unknown error at token: ' + tokenValue}`;
        super(message);
        this.line = line;
        this.column = column;
        this.file = file; // Initialize file
    }
}

/* A simple environment for variable scoping */
export class Environment {
    values: Record<string, any>;
    parent: Environment | null;
    deferred: Statement[];

    constructor(parent: Environment | null = null) {
        this.values = {};
        this.parent = parent;
        this.deferred = [];
    }

    addDeferred(stmt: Statement) {
        this.deferred.push(stmt);
    }

    executeDeferred(interpreter: Interpreter): void {
        while (this.deferred.length > 0) {
            const stmt = this.deferred.pop()!;
            interpreter.executeStmt(stmt, this);
        }
    }

    define(name: string, value: any): void {
        this.values[name] = value;
    }

    has(name: string): boolean {
        if (name in this.values) {
            return true;
        } else if (this.parent) {
            return this.parent.has(name);
        } else {
            return false;
        }
    }

    assign(name: string, value: any, tok: Token): void {
        if (name in this.values) {
            this.values[name] = value;
        } else if (this.parent) {
            this.parent.assign(name, value, tok);
        } else {
            throw new NovaError(tok, `Undefined variable ${name}`);
        }
    }

    // Modified get method to accept a Token for error reporting
    get(name: string, tok?: Token): any {
        if (name in this.values) {
            return this.values[name];
        } else if (this.parent) {
            return this.parent.get(name, tok);
        } else {
            if (tok)
                throw new NovaError(tok, `Undefined variable ${name}`);
            throw new Error(`Undefined variable ${name}`);
        }
    }
}

function initGlobals(globals: Environment): void {
    globals.define('print', console.log);
    const runtimeVersion: RuntimeVersion = {
        major: 0,
        minor: 7,
        patch: 0
    };

    // Create a dummy token for internal/bootstrap errors in initGlobals
    const internalToken: Token = { type: "internal", value: "init", file: "internal_init.ts", line: 1, column: 1 };

    globals.define("isArray", Array.isArray);
    globals.define("new", (className: any, ...args: any[]) => new className(...args));

    globals.define("Logger", {
        info: (...args: any[]) => console.log("[info %s| at: %i:%i:%i]:", globals.get("Runtime", internalToken).versionString, // Pass internalToken
            new Date().getHours(),
            new Date().getMinutes(),
            new Date().getSeconds(),
            ...args
        ),
        warn: (...args: any[]) => console.warn("[warn %s| at: %i:%i:%i]:", globals.get("Runtime", internalToken).versionString, // Pass internalToken
            new Date().getHours(),
            new Date().getMinutes(),
            new Date().getSeconds(),
            ...args
        ),
        error: (...args: any[]) => console.error("[error %s| at: %i:%i:%i]:", globals.get("Runtime", internalToken).versionString, // Pass internalToken
            new Date().getHours(),
            new Date().getMinutes(),
            new Date().getSeconds(),
            ...args
        )
    });

    globals.define("is", {
        string: (str: any) => typeof str === "string",
        number: (num: any) => typeof num === "number",
        boolean: (bool: any) => typeof bool === "boolean",
    });

    const args = process.argv.slice(2);
    globals.define("json", JSON);
    globals.define("parse", {
        int: parseInt,
        float: parseFloat,
        str: String,
        bool: Boolean
    });

    globals.define("math", Math);
    globals.define("null", null);
    globals.define("undefined", undefined);
    globals.define("void", undefined); // 90% of js bindings have functions that return void, return undefined when in js
    globals.define("NaN", NaN);

    globals.define('Runtime', {
        dump: {
            keys: Object.keys,
            values: Object.values
        },
        version: runtimeVersion,
        versionString: `v${runtimeVersion.major}.${runtimeVersion.minor}.${runtimeVersion.patch}`,
        currentDirectory: process.cwd,
        regex: (strPatt: string, options: string = "") => new RegExp(strPatt, options),
        args,
        exit: function (code: number = 0) {
            process.exit(code);
        },
        versionAtLeast: function (maj: number, min: number, pat: number): boolean {
            if (runtimeVersion.major > maj) return true;
            if (runtimeVersion.major < maj) return false;

            if (runtimeVersion.minor > min) return true;
            if (runtimeVersion.minor < min) return false;

            return runtimeVersion.patch >= pat;
        },
        env: (key?: string): any => {
            if (key) return process.env[key];
            return process.env;
        },
        // This throw is for user-facing `Runtime.throw` in NovaScript, so it should use NovaError
        throw: (reason?: any, ...rest: any[]) => {
            if (rest) {
                let s = util.format(reason, ...rest);
                reason = s;
            }
            // For Runtime.throw, we don't have a direct token from the script.
            // We can create a dummy one or use a generic "runtime" token.
            const runtimeThrowToken: Token = { type: "runtime", value: "throw", file: "runtime_internal.ts", line: 0, column: 0 };
            throw new NovaError(runtimeThrowToken, reason);
        },
        fs: {
            read: function (path: string) {
                return fs.readFileSync(path, 'utf8');
            },
            write: function (path: string, contents: string) {
                fs.writeFileSync(path, contents, 'utf8');
            },
            exists: function (path: string) {
                return fs.existsSync(path);
            }
        },
        URI: {
            decode: (str: string) => decodeURIComponent(str),
            encode: (str: string) => encodeURIComponent(str)
        },
        time: {
            now: () => Date.now(),
            str: () => new Date(),
            hrtime: () => process.hrtime.bigint().toString()
        }
    });
}

/* Helper function for type checking */
// MODIFIED: Added interpreter parameter
function checkType(expected: string, value: any, token: Token, interpreter: Interpreter): any {
    let actualExpected = expected;
    if (expected === "bool") {
        actualExpected = "boolean"; // Map 'bool' from parser to 'boolean' for JS type checking
    }

    switch (actualExpected) {
        case "number":
            if (typeof value !== "number") throw new NovaError(token, `Type mismatch: expected number, got ${typeof value}`);
            break;
        case "string":
            if (typeof value !== "string") throw new NovaError(token, `Type mismatch: expected string, got ${typeof value}`);
            break;
        case "boolean":
            if (typeof value !== "boolean") throw new NovaError(token, `Type mismatch: expected boolean, got ${typeof value}`);
            break;
        case "function":
            if (typeof value !== "function") throw new NovaError(token, `Type mismatch: expected function, got ${typeof value}`);
            break;
        case "void":
            // Changed to NovaError
            if (value !== undefined) throw new NovaError(token, `Type mismatch: expected void, got ${typeof value}`);
            break;
        default:
            // NEW: Check for custom types
            const customTypeDefinition = interpreter.customTypes.get(expected); // Use original 'expected' for lookup
            if (customTypeDefinition) {
                if (typeof value !== "object" || value === null) {
                    throw new NovaError(token, `Type mismatch: expected custom type '${expected}', got ${typeof value}`);
                }
                for (const propDef of customTypeDefinition.properties) {
                    if (!(propDef.name in value)) {
                        throw new NovaError(token, `Type mismatch: custom type '${expected}' is missing property '${propDef.name}'`);
                    }
                    // Recursively check property type
                    checkType(propDef.type, value[propDef.name], token, interpreter); // Pass interpreter
                }
            } else {
                // Changed to NovaError
                throw new NovaError(token, `Unknown type: ${expected}`);
            }
    }
    return value;
}

export class Interpreter {
    keywords: string[] = [
        "var", "if", "else", "elseif", "end", "break", "continue", "func",
        "return", "import", "as", "namespace", "while", "forEach", "for",
        "do", "in", "try", "errored", "defer",
        "switch", "case", "default", "using",
        // "def" = "(...)=>{...}"
        // def (...) ... end
        "def",
        // type rect = {x : number,y: number, width:number, height: number}
        "type"
    ];

    source: string;
    file: string;
    tokens: Token[];
    current: number;
    globals: Environment;
    functions: Record<string, Function>;
    importedFiles: Set<string>;
    currentEnv: Environment | null = null;
    customTypes: Map<string, CustomType>; // NEW: Custom types map

    constructor(filePath: string) {
        const source = fs.readFileSync(filePath, "utf8");
        this.source = source;
        this.file = filePath;
        this.importedFiles = new Set();

        this.tokens = this.tokenize(source, filePath); // Pass filePath to tokenize
        this.current = 0;
        this.globals = new Environment();
        this.functions = {};
        this.customTypes = new Map<string, CustomType>(); // NEW: Initialize customTypes
        initGlobals(this.globals);
        this.globals.define("__SCRIPT_PATH__", path.dirname(this.file));
    }

    // ----------------------
    // Tokenization
    // ----------------------
    tokenize(source: string, file: string): Token[] { // Corrected type of file parameter
        const tokens: Token[] = [];
        let i = 0;
        let line = 1;
        let col = 1;

        const length = source.length;

        while (i < length) {
            let char = source[i];
            const startCol = col;
            const nextChar = source[i + 1];

            // Skip whitespace.
            if (/\s/.test(char)) {
                if (char === "\n") {
                    line++;
                    col = 1;
                } else {
                    col++;
                }
                i++;
                continue;
            }

            // --- Comment support ---
            if (char === "/" && nextChar === "/") { // Single-line comment
                while (i < length && source[i] !== "\n") {
                    i++;
                }
                // Don't continue here, let the loop handle the newline
                continue;
            }
            if (char === "/" && nextChar === "*") { // Multi-line comment
                i += 2; // Consume "/*"
                col += 2;
                while (i < length && !(source[i] === "*" && i + 1 < length && source[i + 1] === "/")) {
                    if (source[i] === "\n") {
                        line++;
                        col = 1;
                    } else {
                        col++;
                    }
                    i++;
                }
                if (i + 1 < length && source[i] === "*" && source[i + 1] === "/") {
                    i += 2; // Consume "*/"
                    col += 2;
                } else {
                    // Changed to NovaError
                    throw new NovaError({ type: "error", value: "Unterminated comment", file: file, line: line, column: col }, "Unterminated multi-line comment");
                }
                continue;
            }

            // --- Operators (prioritize longer matches) ---
            let matchedOperator = false;
            const twoCharOps = ["+=", "-=", "*=", "/=", "%=", "==", "!=", "<=", ">=", "&&", "||"];
            const currentTwoChar = char + (nextChar || ''); // Handle end of string safely

            if (i + 1 < length && twoCharOps.includes(currentTwoChar)) {
                tokens.push({ type: "operator", value: currentTwoChar, line, column: startCol, file });
                i += 2;
                col += 2;
                matchedOperator = true;
            } else {
                // Single-character operators that might be part of two-char ops, or stand alone
                const potentialSingleOps = "=+-*/%<>!."; // Added '.' here
                if (potentialSingleOps.includes(char)) {
                    tokens.push({ type: "operator", value: char, line, column: startCol, file });
                    i++;
                    col++;
                    matchedOperator = true;
                } else {
                    // Other single-character punctuation/operators
                    const otherSingleOps = "()[]{},:#";
                    if (otherSingleOps.includes(char)) {
                        tokens.push({ type: "operator", value: char, line, column: startCol, file });
                        i++;
                        col++;
                        matchedOperator = true;
                    }
                }
            }

            if (matchedOperator) {
                continue;
            }

            // Numbers (supporting decimals)
            if (/[0-9]/.test(char)) {
                let num = "";
                while (i < length && /[0-9\.]/.test(source[i])) {
                    num += source[i];
                    i++;
                    col++;
                }
                tokens.push({ type: "number", value: parseFloat(num), line, column: startCol, file: file });
                continue;
            }

            // Strings: delimited by double quotes.
            if (char === '"') {
                i++;
                let str = "";
                while (i < length && source[i] !== '"') {
                    if (source[i] === "\\" && i + 1 < length) {
                        i++;
                        switch (source[i]) {
                            case "n": str += "\n"; break;
                            case "t": str += "\t"; break;
                            case "r": str += "\r"; break;
                            case "\\": str += "\\"; break;
                            case "\"": str += "\""; break;
                            default:
                                str += source[i];
                                break;
                        }
                    } else {
                        str += source[i];
                    }
                    i++;
                    col++;
                }
                if (i < length && source[i] === '"') { // Consume closing quote
                    i++;
                    col++;
                } else {
                    // Changed to NovaError
                    throw new NovaError({ type: "error", value: "Unterminated string", file: file, line: line, column: col }, "Unterminated string literal");
                }
                tokens.push({ type: "string", value: str, line, column: startCol, file: file });
                continue;
            }

            // Identifiers, keywords, booleans.
            if (/[A-Za-z_]/.test(char)) {
                let id = "";
                while (i < length && /[A-Za-z0-9_]/.test(source[i])) {
                    id += source[i];
                    i++;
                    col++;
                }
                if (id === "true" || id === "false") {
                    tokens.push({ type: "boolean", value: id === "true", line, column: startCol, file });
                } else if (this.keywords.includes(id)) {
                    tokens.push({ type: "keyword", value: id, line, column: startCol, file });
                } else {
                    tokens.push({ type: "identifier", value: id, line, column: startCol, file });
                }
                continue;
            }

            // Changed to NovaError
            throw new NovaError({ type: "error", value: char, file: file, line: line, column: col }, `Unexpected character: ${char}`);
        }
        return tokens;
    }

    // ----------------------
    // Token Parser Helpers
    // ----------------------
    getNextToken(): Token | null {
        return this.tokens[this.current] || null;
    }

    consumeToken(): Token {
        return this.tokens[this.current++];
    }

    expectType(type: string): Token {
        const token = this.getNextToken();
        if (!token || token.type !== type) {
            throw new NovaError(token, `Expected token type ${type}, got ${token ? token.type : "EOF"}`);
        }
        return token;
    }

    // Changed to NovaError
    expectToken(value: string): Token {
        const token = this.getNextToken();
        if (!token) {
            // Create a dummy token for EOF, or pass the last valid token for context
            const lastToken = this.tokens[this.current - 1] || { type: "EOF", value: "EOF", file: this.file, line: 1, column: 1 };
            throw new NovaError(lastToken, `Expected token '${value}', got EOF`);
        }
        if (token.value !== value) {
            throw new NovaError(token, `Expected token '${value}', got ${token.value}`);
        }
        return token;
    }

    // ----------------------
    // Parsing Helpers
    // ----------------------
    parseBlockUntil(terminators: string[] = []): Statement[] {
        const statements: Statement[] = [];
        while (this.current < this.tokens.length) {
            const token = this.getNextToken();
            if (!token) {
                // Changed to NovaError
                const lastToken = this.tokens[this.current - 1] || { type: "EOF", value: "EOF", file: this.file, line: 1, column: 1 };
                throw new NovaError(lastToken, "Unexpected end of input");
            }
            if (
                (token.type === "keyword" && terminators.includes(token.value)) ||
                (token.type === "operator" && terminators.includes(token.value))
            ) {
                break;
            }
            const t = this.parseStatement()
            if(t)
                statements.push(t);
        }
        return statements;
    }

    parseBlock(): Statement[] {
        return this.parseBlockUntil();
    }
    consumeExpected(type: string): Token {
        this.expectType(type);
        return this.consumeToken();
    }

    // ----------------------
    // Parsing Statements and Expressions
    // ----------------------
    parseStatement(): Statement | null {
        const token = this.getNextToken();
        if (!token) {
            // Changed to NovaError
            const lastToken = this.tokens[this.current - 1] || { type: "EOF", value: "EOF", file: this.file, line: 1, column: 1 };
            throw new NovaError(lastToken, "Unexpected end of input");
        }

        // --- Label statement ---
        if (token.type === "keyword" && token.value === "label") {
            this.consumeToken();
            const nameToken = this.expectType("identifier");
            const name = nameToken.value;
            this.consumeToken();
            return { type: "LabelStmt", name, line: token.line, column: token.column, file: token.file };
        }
        // --- Defer statement ---
        if (token.type === "keyword" && token.value === "defer") {
            this.consumeToken();
            const body = this.parseBlockUntil(["end"]);
            this.expectToken("end");
            this.consumeToken();
            return { type: "DeferStmt", body, line: token.line, column: token.column, file: token.file };
        }
        // NEW: Custom Type Declaration
        if (token.type === "keyword" && token.value === "type") {
            this.consumeToken(); // consume 'type'
            const nameToken = this.expectType("identifier");
            const name = nameToken.value;
            this.consumeToken(); // consume type name

            this.expectToken("=");
            this.consumeToken(); // consume '='

            this.expectToken("{");
            this.consumeToken(); // consume '{'

            const properties: CustomTypeProperty[] = [];
            while (this.getNextToken() && this.getNextToken()!.value !== "}") {
                const propNameToken = this.expectType("identifier");
                const propName = propNameToken.value;
                this.consumeToken(); // consume property name

                this.expectToken(":");
                this.consumeToken(); // consume ':'

                // Expect an identifier for the type name (e.g., "number", "string", "MyCustomType")
                const propTypeToken = this.expectType("identifier");
                const propType = propTypeToken.value;
                this.consumeToken(); // consume property type

                properties.push({ name: propName, type: propType });

                if (this.getNextToken() && this.getNextToken()!.value === ",") {
                    this.consumeToken(); // consume ','
                } else {
                    break;
                }
            }

            this.expectToken("}");
            this.consumeToken(); // consume '}'
            const customTypeStmt = {
                type: "CustomTypeDecl",
                name,
                definition: properties,
                file: token.file,
                line: token.line,
                column: token.column
            };
            this.customTypes.set(customTypeStmt.name, {
                name: customTypeStmt.name,
                properties: customTypeStmt.definition,
                file: customTypeStmt.file,
                line: customTypeStmt.line,
                column: customTypeStmt.column
            });
            return// the statement is not handled at runtime, don't return it
        }
        // --- Variable declaration with optional type annotation ---
        if (token.type === "keyword" && token.value === "var") {
            this.consumeToken();
            const nameToken = this.expectType("identifier");
            const name = nameToken.value;
            this.consumeToken();

            // Optional type annotation
            let annotationType: string | null = null; // MODIFIED: Renamed to annotationType
            if (
                this.getNextToken() &&
                this.getNextToken()!.type === "identifier" && // Added ! for non-null assertion
                ["string", "number", "boolean", "void", "bool", ...this.customTypes.keys()].includes(this.getNextToken()!.value) // Added 'bool' for consistency with parser
            ) {
                annotationType = this.getNextToken()!.value; // Added !
                this.consumeToken();
            }
            //console.log(this.customTypes.keys())

            this.expectToken("=");
            this.consumeToken();
            const initializer = this.parseExpression();
            return { type: "VarDecl", name, typeAnnotation: annotationType, initializer, line: token.line, column: token.column, file: token.file }; // MODIFIED: typeAnnotation
        }

        // --- Switch statement ---
        if (token.type === "keyword" && token.value === "switch") {
            this.consumeToken();
            const expression = this.parseExpression();
            const cases: Case[] = [];
            while (this.getNextToken()?.type === "keyword" && this.getNextToken()!.value === "case") { // Added !
                this.consumeToken();
                const caseExpr = this.parseExpression();
                this.expectToken("do");
                this.consumeToken();
                const body = this.parseBlockUntil(["end"]);
                this.expectToken("end");
                this.consumeToken();
                cases.push({ caseExpr, body });
            }
            if (this.getNextToken()?.type === "keyword" && this.getNextToken()!.value === "default") { // Added !
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

            return { type: "SwitchStmt", expression, cases, line: token.line, column: token.column, file: token.file };
        }

        // --- using statement ---
        if (token.type === "keyword" && token.value === "using") {
            this.consumeToken();
            const nameToken = this.expectType("identifier");
            this.consumeToken();

            return { type: "UsingStmt", name: nameToken.value, line: token.line, column: token.column, file: token.file };
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
            return { type: "TryStmt", tryBlock, errorVar, catchBlock, line: token.line, column: token.column, file: token.file };
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
            return { type: "ForEachStmt", variable, list: listExpr, body, line: token.line, column: token.column, file: token.file };
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
            let stepExpr: Expression | undefined;
            if (this.getNextToken()?.value === ",") {
                this.consumeToken();
                stepExpr = this.parseExpression();
            }
            this.expectToken("do");
            this.consumeToken();
            const body = this.parseBlockUntil(["end"]);
            this.expectToken("end");
            this.consumeToken();
            return { type: "ForStmt", variable, start: startExpr, end: endExpr, step: stepExpr, body, line: token.line, column: token.column, file: token.file };
        }

        // --- While loop ---
        if (token.type === "keyword" && token.value === "while") {
            this.consumeToken();
            const condition = this.parseExpression();
            const body = this.parseBlockUntil(["end"]);
            this.expectToken("end");
            this.consumeToken();
            return { type: "WhileStmt", condition, body, line: token.line, column: token.column, file: token.file };
        }

        // --- Break statement ---
        if (token.type === "keyword" && token.value === "break") {
            this.consumeToken();
            return { type: "BreakStmt", line: token.line, column: token.column, file: token.file };
        }
        // --- Continue statement ---
        if (token.type === "keyword" && token.value === "continue") {
            this.consumeToken();
            return { type: "ContinueStmt", line: token.line, column: token.column, file: token.file };
        }

        if (token.type === "keyword" && token.value === "if") {
            this.consumeToken();
            const condition = this.parseExpression();
            const thenBlock = this.parseBlockUntil(["else", "elseif", "end"]);

            const elseIfBlocks: { condition: Expression, body: Statement[] }[] = [];
            let elseBlock: Statement[] | null = null;

            // Parse elseif chains
            while (this.getNextToken()?.type === "keyword" && this.getNextToken()!.value === "elseif") { // Added !
                this.consumeToken(); // consume 'elseif'
                const elseifCondition = this.parseExpression();
                const elseifBody = this.parseBlockUntil(["else", "elseif", "end"]);
                elseIfBlocks.push({ condition: elseifCondition, body: elseifBody });
            }

            // Parse else block if present
            if (this.getNextToken()?.type === "keyword" && this.getNextToken()!.value === "else") { // Added !
                this.consumeToken();
                elseBlock = this.parseBlockUntil(["end"]);
            }

            this.expectToken("end");
            this.consumeToken();

            return {
                type: "IfStmt",
                condition,
                thenBlock,
                elseBlock,
                elseIf: elseIfBlocks.length > 0 ? elseIfBlocks : null,
                line: token.line, column: token.column, file: token.file
            };
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
            return { type: "NamespaceStmt", name, body, line: token.line, column: token.column, file: token.file };
        }

        if (token.type === "keyword" && token.value === "func") {
            this.consumeToken();
            const nameToken = this.expectType("identifier");
            const name = nameToken.value;
            this.consumeToken();
            this.expectToken("(");
            this.consumeToken();

            const parameters: Parameter[] = [];
            if (this.getNextToken() && this.getNextToken()!.value !== ")") { // Added !
                while (true) {
                    const paramToken = this.expectType("identifier");
                    const paramName = paramToken.value;
                    this.consumeToken();

                    let annotationType: string | null = null; // MODIFIED: Renamed
                    let defaultExpr: Expression | undefined;

                    // optional type annotation
                    if (this.getNextToken()?.type === "identifier") {
                        const typeToken = this.getNextToken()!; // Added !
                        if (["string", "number", "bool", "boolean"].includes(typeToken.value)) { // Added "boolean"
                            annotationType = typeToken.value; // MODIFIED: Renamed
                            this.consumeToken();
                        }
                    }

                    // optional default value
                    if (this.getNextToken()?.value === "=") {
                        this.consumeToken();
                        defaultExpr = this.parseExpression();
                        if (annotationType) { // MODIFIED: Logic for explicit type + default
                            throw new NovaError(token, "Cannot have both explicit type annotation and a default value. Consider removing the type annotation to allow type inference from the default value.");
                        }

                        // Infer type from default value if no explicit annotation
                        if (defaultExpr.type === "Literal") {
                            const inferredType = typeof (defaultExpr as Literal).value;
                            if (inferredType === "string") annotationType = "string";
                            else if (inferredType === "number") annotationType = "number";
                            else if (inferredType === "boolean") annotationType = "bool"; // Use "bool" for consistency with parser
                            else annotationType = null; // Cannot infer
                        } else {
                            annotationType = null; // For complex expressions, inference at parse time is difficult.
                        }
                    }

                    // Ensure Parameter also has token info
                    parameters.push({
                        name: paramName,
                        annotationType: annotationType, // MODIFIED: Renamed
                        default: defaultExpr,
                        file: paramToken.file,
                        line: paramToken.line,
                        column: paramToken.column,
                        type: paramToken.type, // Token type (e.g., "identifier")
                        value: paramToken.value // Token value (the identifier name)
                    });

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
                body,
                file: token.file, line: token.line, column: token.column
            };
        }

        if (token.type === "keyword" && token.value === "def") {
            this.consumeToken();
            this.expectToken("(");
            this.consumeToken();

            const parameters: Parameter[] = [];
            if (this.getNextToken() && this.getNextToken()!.value !== ")") { // Added !
                while (true) {
                    const paramToken = this.expectType("identifier");
                    const paramName = paramToken.value;
                    this.consumeToken();

                    let annotationType: string | null = null; // MODIFIED: Renamed
                    let defaultExpr: Expression | undefined;

                    // optional type annotation
                    if (this.getNextToken()?.type === "identifier") {
                        const typeToken = this.getNextToken()!; // Added !
                        if (["string", "number", "bool", "boolean"].includes(typeToken.value)) { // Added "boolean"
                            annotationType = typeToken.value; // MODIFIED: Renamed
                            this.consumeToken();
                        }
                    }

                    // optional default value
                    if (this.getNextToken()?.value === "=") {
                        this.consumeToken();
                        defaultExpr = this.parseExpression();
                        if (annotationType) { // MODIFIED: Logic for explicit type + default
                            throw new NovaError(token, "Cannot have both explicit type annotation and a default value. Consider removing the type annotation to allow type inference from the default value.");
                        }

                        // Infer type from default value if no explicit annotation
                        if (defaultExpr.type === "Literal") {
                            const inferredType = typeof (defaultExpr as Literal).value;
                            if (inferredType === "string") annotationType = "string";
                            else if (inferredType === "number") annotationType = "number";
                            else if (inferredType === "boolean") annotationType = "bool"; // Use "bool" for consistency with parser
                            else annotationType = null; // Cannot infer
                        } else {
                            annotationType = null; // For complex expressions, inference at parse time is difficult.
                        }
                    }

                    // Ensure Parameter also has token info
                    parameters.push({
                        name: paramName,
                        annotationType: annotationType, // MODIFIED: Renamed
                        default: defaultExpr,
                        file: paramToken.file,
                        line: paramToken.line,
                        column: paramToken.column,
                        type: paramToken.type, // Token type (e.g., "identifier")
                        value: paramToken.value // Token value (the identifier name)
                    });

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
                type: "LambdaDecl",
                parameters,
                body,
                file: token.file, line: token.line, column: token.column
            };
        }

        // --- Return statement ---
        if (token.type === "keyword" && token.value === "return") {
            this.consumeToken();
            let expression: Expression | null = null;
            if (this.getNextToken() && !this.keywords.includes(this.getNextToken()!.value)) { // Added !
                expression = this.parseExpression();
            }
            return { type: "ReturnStmt", expression, file: token.file, line: token.line, column: token.column };
        }

        // --- Import statement ---
        if (token.type === "keyword" && token.value === "import") {
            this.consumeToken();
            const fileToken = this.expectType("string");
            const filename = fileToken.value;
            this.consumeToken();
            let alias: string | null = null;
            if (this.getNextToken() && this.getNextToken()!.value === "as") { // Added !
                this.consumeToken();
                const aliasToken = this.expectType("identifier");
                alias = aliasToken.value;
                this.consumeToken();
            }
            return { type: "ImportStmt", filename, alias, file: token.file, line: token.line, column: token.column };
        }

        // --- Expression statement (fallback) ---
        const expr = this.parseExpression();
        return { type: "ExpressionStmt", expression: expr, file: token.file, line: token.line, column: token.column };
    }

    // --- Expression Parsing (Recursive Descent) ---
    parseExpression(): Expression {
        return this.parseAssignment();
    }

    parseAssignment(): Expression {
        let expr = this.parseLogicalOr(); // This is the left-hand side of the assignment
        const nextToken = this.getNextToken();

        // Check if the next token is an assignment operator (simple or compound)
        if (
            nextToken &&
            nextToken.type === "operator" &&
            ["=", "+=", "-=", "*=", "/=", "%="].includes(nextToken.value)
        ) {
            const assignmentOpToken = this.consumeToken(); // Consume the assignment operator token
            const valueExpr = this.parseAssignment(); // Recursively parse the right-hand side (for chained assignments like a = b = 5)

            // Ensure the target is something assignable
            if (expr.type !== "Identifier" && expr.type !== "PropertyAccess" && expr.type !== "ArrayAccess" && expr.type !== "ArrayLiteral") {
                throw new NovaError(expr, `Invalid assignment target: Cannot assign to ${expr.type}`);
            }

            // Create the AssignmentExpr AST node
            return {
                type: "AssignmentExpr",
                target: expr,
                value: valueExpr,
                operator: assignmentOpToken.value, // Store the specific operator (e.g., "=", "+=")
                file: assignmentOpToken.file,
                line: assignmentOpToken.line,
                column: assignmentOpToken.column
            };
        }
        return expr; // If no assignment operator, it's just a logical OR expression
    }

    parseLogicalOr(): Expression {
        let expr = this.parseLogicalAnd();
        while (
            this.getNextToken() &&
            this.getNextToken()!.type === "operator" && // Added !
            this.getNextToken()!.value === "||" // Added !
        ) {
            const operatorToken = this.consumeToken(); // Capture the operator token
            const operator = operatorToken.value;
            const right = this.parseLogicalAnd();
            // Added file, line, column
            expr = { type: "BinaryExpr", operator, left: expr, right, file: operatorToken.file, line: operatorToken.line, column: operatorToken.column };
        }
        return expr;
    }

    parseLogicalAnd(): Expression {
        let expr = this.parseEquality();
        while (
            this.getNextToken() &&
            this.getNextToken()!.type === "operator" && // Added !
            this.getNextToken()!.value === "&&" // Added !
        ) {
            const operatorToken = this.consumeToken(); // Capture the operator token
            const operator = operatorToken.value;
            const right = this.parseEquality();
            // Added file, line, column
            expr = { type: "BinaryExpr", operator, left: expr, right, file: operatorToken.file, line: operatorToken.line, column: operatorToken.column };
        }
        return expr;
    }

    parseEquality(): Expression {
        let expr = this.parseComparison();
        while (
            this.getNextToken() &&
            this.getNextToken()!.type === "operator" && // Added !
            (this.getNextToken()!.value === "==" || this.getNextToken()!.value === "!=") // Added !
        ) {
            const operatorToken = this.consumeToken(); // Capture the operator token
            const operator = operatorToken.value;
            const right = this.parseComparison();
            // Added file, line, column
            expr = { type: "BinaryExpr", operator, left: expr, right, file: operatorToken.file, line: operatorToken.line, column: operatorToken.column };
        }
        return expr;
    }

    parseComparison(): Expression {
        let expr = this.parseTerm();
        while (
            this.getNextToken() &&
            this.getNextToken()!.type === "operator" && // Added !
            ["<", "<=", ">", ">="].includes(this.getNextToken()!.value) // Added !
        ) {
            const operatorToken = this.consumeToken(); // Capture the operator token
            const operator = operatorToken.value;
            const right = this.parseTerm();
            // Added file, line, column
            expr = { type: "BinaryExpr", operator, left: expr, right, file: operatorToken.file, line: operatorToken.line, column: operatorToken.column };
        }
        return expr;
    }

    parseTerm(): Expression {
        let expr = this.parseFactor();
        while (
            this.getNextToken() &&
            this.getNextToken()!.type === "operator" && // Added !
            (this.getNextToken()!.value === "+" || this.getNextToken()!.value === "-") // Added !
        ) {
            const operatorToken = this.consumeToken(); // Capture the operator token
            const operator = operatorToken.value;
            const right = this.parseFactor();
            // Added file, line, column
            expr = { type: "BinaryExpr", operator, left: expr, right, file: operatorToken.file, line: operatorToken.line, column: operatorToken.column };
        }
        return expr;
    }

    parseFactor(): Expression {
        let expr = this.parseUnary();
        while (
            this.getNextToken() &&
            this.getNextToken()!.type === "operator" && // Added !
            (this.getNextToken()!.value === "*" || this.getNextToken()!.value === "/" || this.getNextToken()!.value === "%") // Added !
        ) {
            const operatorToken = this.consumeToken(); // Capture the operator token
            const operator = operatorToken.value;
            const right = this.parseUnary();
            // Added file, line, column
            expr = { type: "BinaryExpr", operator, left: expr, right, file: operatorToken.file, line: operatorToken.line, column: operatorToken.column };
        }
        return expr;
    }

    parseUnary(): Expression {
        if (
            this.getNextToken() &&
            this.getNextToken()!.type === "operator" && // Added !
            (this.getNextToken()!.value === "-" || this.getNextToken()!.value === "!") // Added !
        ) {
            const operatorToken = this.consumeToken(); // Capture the operator token
            const operator = operatorToken.value;
            const right = this.parseUnary();
            // Added file, line, column
            return { type: "UnaryExpr", operator, right, file: operatorToken.file, line: operatorToken.line, column: operatorToken.column };
        }
        // MODIFIED: Call parseCallMemberExpression instead of parsePrimary directly
        return this.parseCallMemberExpression();
    }

    // NEW FUNCTION: Handles chained property access, array access, and function/method calls
    parseCallMemberExpression(): Expression {
        let expr = this.parsePrimary(); // Start with a primary expression

        while (true) {
            const nextToken = this.getNextToken();
            if (!nextToken) break;

            if (nextToken.value === ".") {
                const dotToken = this.consumeToken(); // consume "."
                const propToken = this.expectType("identifier");
                const propName = propToken.value;
                this.consumeToken(); // consume identifier

                // Check for method call
                if (this.getNextToken() && this.getNextToken()!.value === "(") {
                    this.consumeToken(); // consume "("
                    const args: Expression[] = [];
                    if (this.getNextToken() && this.getNextToken()!.value !== ")") {
                        while (true) {
                            args.push(this.parseExpression());
                            if (this.getNextToken() && this.getNextToken()!.value === ",") this.consumeToken();
                            else break;
                        }
                    }
                    this.expectToken(")");
                    this.consumeToken(); // consume ")"
                    expr = {
                        type: "MethodCall",
                        object: expr, // The expression parsed so far is the object
                        method: propName,
                        arguments: args,
                        file: propToken.file, line: propToken.line, column: propToken.column
                    };
                } else {
                    // Plain property access
                    expr = {
                        type: "PropertyAccess",
                        object: expr, // The expression parsed so far is the object
                        property: propName,
                        file: propToken.file, line: propToken.line, column: propToken.column
                    };
                }
            } else if (nextToken.value === "[") {
                const bracketToken = this.consumeToken(); // consume "["
                const indexExpr = this.parseExpression();
                this.expectToken("]");
                this.consumeToken(); // consume "]"
                expr = {
                    type: "ArrayAccess",
                    object: expr, // The expression parsed so far is the array/object
                    index: indexExpr,
                    file: bracketToken.file, line: bracketToken.line, column: bracketToken.column
                };
            } else if (nextToken.value === "(" && expr.type === "Identifier") {
                // This handles direct function calls like `myFunc(arg)`
                // It should only apply if the current 'expr' is an Identifier.
                // Method calls are handled above with the '.' operator.
                this.consumeToken(); // consume "("
                const args: Expression[] = [];
                if (this.getNextToken() && this.getNextToken()!.value !== ")") {
                    while (true) {
                        args.push(this.parseExpression());
                        if (this.getNextToken() && this.getNextToken()!.value === ",") {
                            this.consumeToken();
                        } else {
                            break;
                        }
                    }
                }
                this.expectToken(")");
                this.consumeToken(); // consume ")"
                expr = {
                    type: "FuncCall",
                    name: (expr as Identifier).name, // Use the name from the Identifier
                    arguments: args,
                    file: expr.file, line: expr.line, column: expr.column
                };
            }
            else {
                break; // No more chained access/calls
            }
        }
        return expr;
    }


    parsePrimary(): Expression {
        let node: Expression;
        const token = this.getNextToken();
        if (!token) {
            // Changed to NovaError
            const lastToken = this.tokens[this.current - 1] || { type: "EOF", value: "EOF", file: this.file, line: 1, column: 1 };
            throw new NovaError(lastToken, "Unexpected end of input");
        }

        if (token.type === "boolean") {
            this.consumeToken();
            // Added file, line, column
            node = { type: "Literal", value: token.value, file: token.file, line: token.line, column: token.column };
        }
        else if (token.type === "number" || token.type === "string") {
            this.consumeToken();
            // Added file, line, column
            node = { type: "Literal", value: token.value, file: token.file, line: token.line, column: token.column };
        }
        else if (token.value === "[") {
            this.consumeToken();
            const elements: Expression[] = [];
            if (this.getNextToken() && this.getNextToken()!.value !== "]") { // Added !
                while (true) {
                    elements.push(this.parseExpression());
                    if (this.getNextToken() && this.getNextToken()!.value === ",") { // Added !
                        this.consumeToken();
                    } else {
                        break;
                    }
                }
            }
            this.expectToken("]");
            this.consumeToken();
            // Added file, line, column
            node = { type: "ArrayLiteral", elements, file: token.file, line: token.line, column: token.column };
        }
        else if (token.value === "{") {
            this.consumeToken();
            const properties: { key: string; value: Expression }[] = [];
            if (this.getNextToken() && this.getNextToken()!.value !== "}") { // Added !
                while (true) {
                    let keyToken = this.getNextToken()!; // Added !
                    if (keyToken.type !== "identifier" && keyToken.type !== "string") {
                        throw new NovaError(keyToken, "Expected identifier or string as object key");
                    }
                    const key = keyToken.value;
                    this.consumeToken();
                    this.expectToken(":");
                    this.consumeToken();
                    const value = this.parseExpression();
                    properties.push({ key, value });
                    if (this.getNextToken() && this.getNextToken()!.value === ",") { // Added !
                        this.consumeToken();
                    } else {
                        break;
                    }
                }
            }
            this.expectToken("}");
            this.consumeToken();
            // Added file, line, column
            node = { type: "ObjectLiteral", properties, file: token.file, line: token.line, column: token.column };
        }
        else if (token.type === "identifier") {
            this.consumeToken();
            // MODIFIED: Removed direct handling of ArrayAccess and FuncCall here.
            // These will now be handled by parseCallMemberExpression.
            node = { type: "Identifier", name: token.value, file: token.file, line: token.line, column: token.column };
        }
        else if(token.value == "def"){
            // x = def (...)  ... end
            this.consumeToken();
            this.expectToken("(");
            this.consumeToken();

            const parameters: Parameter[] = [];
            if (this.getNextToken() && this.getNextToken()!.value !== ")") { // Added !
                while (true) {
                    const paramToken = this.expectType("identifier");
                    const paramName = paramToken.value;
                    this.consumeToken();

                    let annotationType: string | null = null; // MODIFIED: Renamed
                    let defaultExpr: Expression | undefined;

                    // optional type annotation
                    if (this.getNextToken()?.type === "identifier") {
                        const typeToken = this.getNextToken()!; // Added !
                        if (["string", "number", "bool", "boolean"].includes(typeToken.value)) { // Added "boolean"
                            annotationType = typeToken.value; // MODIFIED: Renamed
                            this.consumeToken();
                        }
                    }

                    // optional default value
                    if (this.getNextToken()?.value === "=") {
                        this.consumeToken();
                        defaultExpr = this.parseExpression();
                        if (annotationType) { // MODIFIED: Logic for explicit type + default
                            throw new NovaError(token, "Cannot have both explicit type annotation and a default value. Consider removing the type annotation to allow type inference from the default value.");
                        }

                        // Infer type from default value if no explicit annotation
                        if (defaultExpr.type === "Literal") {
                            const inferredType = typeof (defaultExpr as Literal).value;
                            if (inferredType === "string") annotationType = "string";
                            else if (inferredType === "number") annotationType = "number";
                            else if (inferredType === "boolean") annotationType = "bool"; // Use "bool" for consistency with parser
                            else annotationType = null; // Cannot infer
                        } else {
                            annotationType = null; // For complex expressions, inference at parse time is difficult.
                        }
                    }

                    // Ensure Parameter also has token info
                    parameters.push({
                        name: paramName,
                        annotationType: annotationType, // MODIFIED: Renamed
                        default: defaultExpr,
                        file: paramToken.file,
                        line: paramToken.line,
                        column: paramToken.column,
                        type: paramToken.type, // Token type (e.g., "identifier")
                        value: paramToken.value // Token value (the identifier name)
                    });

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
                type: "LambdaDecl",
                parameters,
                body,
                file: token.file, line: token.line, column: token.column
            };
        }
        else if (token.value === "(") {
            this.consumeToken();
            node = this.parseExpression();
            this.expectToken(")");
            this.consumeToken();
            // The node returned from parseExpression already has token info.
            // No need to add it here, as it's just a grouping.
        }
        else {
            throw new NovaError(token, `Unexpected token: ${token.value}`);
        }

        // MODIFIED: Removed the old while loop for chained access, now handled by parseCallMemberExpression
        return node;
    }

    // ----------------------
    // Evaluation / Execution
    // ----------------------
    interpret(): void {
        const statements = this.parseBlock();
        try {
            this.executeBlock(statements, this.globals);
        } catch (err) {
            // Ensure deferred statements still execute
            this.globals.executeDeferred(this);
            throw err
        }
    }
    executeBlock(statements: Statement[], env: Environment): void {
        const previousEnv = this.currentEnv;
        this.currentEnv = env;

        try {
            for (const stmt of statements) {
                this.executeStmt(stmt, env);
            }
        } finally {
            // Execute deferred statements in reverse order
            env.executeDeferred(this);
            this.currentEnv = previousEnv;
        }
        this.currentEnv = previousEnv;
    }

    getCurrentContext(): Environment | null {
        return this.currentEnv;
    }

    executeStmt(stmt: Statement, env: Environment): void {
        switch (stmt.type) {
            case "VarDecl": {
                const varDeclStmt = stmt as VarDeclStmt; // Cast to VarDeclStmt
                const value = this.evaluateExpr(varDeclStmt.initializer, env);
                if (varDeclStmt.typeAnnotation) {
                    checkType(varDeclStmt.typeAnnotation, value, varDeclStmt, this); // MODIFIED: Pass `this`
                }
                env.define(varDeclStmt.name, value);
                break;
            }

            case "DeferStmt": {
                // Add statements to be executed when the block exits
                const stack: Statement[] = []
                stmt.body.forEach(e => stack.push(e));
                stack.reverse().forEach(e => env.addDeferred(e))

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
                    // If e is an Error or NovaError, pass it. Otherwise, wrap it.
                    const errorToDefine = (e instanceof Error || e instanceof NovaError) ? e : new NovaError(stmt, String(e));
                    catchEnv.define(stmt.errorVar, errorToDefine);
                    this.executeBlock(stmt.catchBlock, catchEnv);
                }
                break;
            }
            case "IfStmt": {
                const condition = this.evaluateExpr(stmt.condition, env);
                if (condition) {
                    this.executeBlock(stmt.thenBlock, new Environment(env));
                }
                // Handle elseif
                else if (stmt.elseIf) {
                    let matched = false;
                    // Create a new environment for the elseif chain
                    const elseifEnv = new Environment(env);

                    // Evaluate elseif conditions in order
                    for (const elseifBlock of stmt.elseIf) {
                        const elseifCondition = this.evaluateExpr(elseifBlock.condition, elseifEnv);
                        if (elseifCondition) {
                            this.executeBlock(elseifBlock.body, elseifEnv);
                            matched = true;
                            break;
                        }
                    }

                    // If no elseif matched, execute else block if it exists
                    if (!matched && stmt.elseBlock) {
                        this.executeBlock(stmt.elseBlock, new Environment(env));
                    }
                }
                // Original else handling
                else if (stmt.elseBlock) {
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
                if (!Array.isArray(list)) {
                    throw new NovaError(stmt, `Cannot iterate over non-array type for forEach loop. Got: ${typeof list}`);
                }
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
                const start = this.evaluateExpr(stmt.start, env);
                const end = this.evaluateExpr(stmt.end, env);
                const step = stmt.step ? this.evaluateExpr(stmt.step, env) : 1;

                if (typeof start !== 'number' || typeof end !== 'number' || typeof step !== 'number') {
                    throw new NovaError(stmt, `For loop bounds and step must be numbers. Got start: ${typeof start}, end: ${typeof end}, step: ${typeof step}`);
                }

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
                if (stmt.filename.startsWith("os:")) {
                    filePath = filePath.substring(3);
                    if (!env.has("os-import-handler")) {
                        throw new NovaError(stmt, "os-import-handler is not defined, your runtime should define it, interpreter.globals.define('os-import-handler', handler)");
                    }

                    const handler = env.get("os-import-handler", stmt) as (path: string) => any; // Pass stmt as token
                    const result = handler(filePath);
                    let name = stmt.alias || filePath;
                    env.define(name, result);
                    break;
                }
                filePath += ".nova";
                const fileDir = path.dirname(this.file);

                const fullPath = path.resolve(fileDir, filePath);
                if (this.importedFiles.has(fullPath)) break; // Check fullPath for uniqueness
                this.importedFiles.add(fullPath);


                // Check if file exists before trying to read
                if (!fs.existsSync(fullPath)) {
                    throw new NovaError(stmt, `Import error: File not found at '${fullPath}'`);
                }

                const importedInterpreter = new Interpreter(fullPath);
                // NEW: The imported interpreter should share the same customTypes map
                importedInterpreter.customTypes = this.customTypes; // Share the customTypes map

                const importedEnv = new Environment(this.globals);
                importedInterpreter.globals = importedEnv;

                importedInterpreter.functions = this.functions;
                importedInterpreter.importedFiles = this.importedFiles;

                importedInterpreter.interpret();

                const namespace: Record<string, any> = {};
                for (const key in importedEnv.values) {
                    namespace[key] = importedEnv.values[key];
                }

                const pathObj = path.parse(filePath);
                const moduleName = pathObj.name;
                let name = stmt.alias || moduleName;

                env.define(name, namespace);
                break;
            }

            case "NamespaceStmt": {
                const nenv = new Environment(env);
                this.executeBlock(stmt.body, nenv);
                env.define(stmt.name, nenv);
                break;
            }
            case "SwitchStmt": {
                const value = this.evaluateExpr(stmt.expression, env);
                let matched = false;
                let defaultCaseBody: Statement[] | null = null;

                for (const c of stmt.cases) {
                    if (c.caseExpr === null) { // This is the default case
                        defaultCaseBody = c.body;
                    } else {
                        const caseVal = this.evaluateExpr(c.caseExpr, env);
                        if (value === caseVal) {
                            this.executeBlock(c.body, new Environment(env));
                            matched = true;
                            break; // Exit switch after first match
                        }
                    }
                }

                // If no case matched and a default case exists, execute it
                if (!matched && defaultCaseBody) {
                    this.executeBlock(defaultCaseBody, new Environment(env));
                }
                break;
            }

            case "ReturnStmt": {
                const value = stmt.expression ? this.evaluateExpr(stmt.expression, env) : undefined;
                throw new ReturnException(value);
            }

            case "FuncDecl": {
                const func = (...args: any[]) => {
                    const funcEnv = new Environment(env);

                    if (args.length > stmt.parameters.length) {
                        throw new NovaError(stmt, `Too many arguments passed to function '${stmt.name}'. Expected ${stmt.parameters.length}, got ${args.length}.`);
                    }

                    for (let i = 0; i < stmt.parameters.length; i++) {
                        const param = stmt.parameters[i];
                        let argVal = args[i];

                        // apply default if missing
                        if (argVal === undefined && param.default !== undefined) {
                            argVal = this.evaluateExpr(param.default, env);
                        } else if (argVal === undefined && param.default === undefined) {
                            // If an argument is missing and no default is provided
                            throw new NovaError(param, `Missing argument for parameter '${param.name}' in function '${stmt.name}'.`);
                        }

                        // soft type check
                        if (param.annotationType) { // MODIFIED: Use annotationType
                            // MODIFIED: Call checkType with interpreter instance
                            checkType(param.annotationType, argVal, param, this);
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
                    return undefined; // Functions without explicit return return undefined
                };

                env.define(stmt.name, func);
                break;
            }


            case "UsingStmt": {
                const namespace = env.get(stmt.name, stmt); // Pass stmt as token
                if (namespace instanceof Environment) {
                    for (const [key, value] of Object.entries(namespace.values)) {
                        env.define(key, value);
                    }
                } else if (typeof namespace === 'object' && namespace !== null) {
                    for (const [key, value] of Object.entries(namespace)) {
                        env.define(key, value);
                    }
                } else {
                    throw new NovaError(stmt, `Cannot 'use' non-namespace value '${stmt.name}'.`);
                }
                break;
            }

            default:
                throw new NovaError(stmt, `Unknown statement type: ${stmt.type}`);
        }
    }

    // NEW HELPER: Resolves the base object and final key/index for assignment
    resolveAssignmentTarget(targetExpr: Expression, env: Environment): { base: any, finalKey: string | number | null } {
        if (targetExpr.type === "Identifier") {
            // For simple identifiers, the base is the environment and the key is the identifier name
            return { base: env, finalKey: (targetExpr as Identifier).name };
        } else if (targetExpr.type === "ArrayAccess") {
            const arrayAccess = targetExpr as ArrayAccess;
            // Recursively evaluate the object part to get the actual array/object
            const baseObject = this.evaluateExpr(arrayAccess.object, env);
            const index = this.evaluateExpr(arrayAccess.index, env);

            if (baseObject === null || baseObject === undefined) {
                throw new NovaError(arrayAccess, `Cannot assign to index of null or undefined value.`);
            }
            if (!Array.isArray(baseObject) && typeof baseObject !== 'object') {
                throw new NovaError(arrayAccess, `Cannot assign to index of non-array/object: ${typeof baseObject}`);
            }
            if (typeof index !== 'number' && typeof index !== 'string') {
                throw new NovaError(arrayAccess, `Array/object index must be a number or string for assignment. Got: ${typeof index}`);
            }
            return { base: baseObject, finalKey: index };
        } else if (targetExpr.type === "PropertyAccess") {
            const propertyAccess = targetExpr as PropertyAccess;
            // Recursively evaluate the object part to get the actual object
            const baseObject = this.evaluateExpr(propertyAccess.object, env);
            const key = propertyAccess.property; // Property name is a string

            if (baseObject === null || baseObject === undefined) {
                throw new NovaError(propertyAccess, `Cannot assign property '${key}' of null or undefined value.`);
            }
            // Special case: if the base object is an Environment, we assign to it directly
            if (baseObject instanceof Environment) {
                return { base: baseObject, finalKey: key };
            }
            return { base: baseObject, finalKey: key };
        } else {
            throw new NovaError(targetExpr, "Invalid assignment target type: " + targetExpr.type);
        }
    }

    evaluateExpr(expr: Expression, env: Environment): any {
        switch (expr.type) {
            case "Literal":
                return expr.value;
            case "Identifier":
                return env.get(expr.name, expr); // Pass expr as token
            case "AssignmentExpr": {
                const target = (expr as AssignmentExpr).target;
                const assignedValue = this.evaluateExpr((expr as AssignmentExpr).value, env);
                const op = (expr as AssignmentExpr).operator; // Get the specific operator

                let finalValueToAssign = assignedValue;

                // Handle compound assignments (if operator is not just "=")
                if (op !== "=") {
                    // Compound assignments only make sense for mutable targets that can be read first.
                    // ArrayLiteral (destructuring) cannot be a target for compound assignment.
                    if (target.type === "ArrayLiteral") {
                         throw new NovaError(target, `Compound assignment operators like '${op}' cannot be used with array destructuring.`);
                    }

                    // Get the current value of the target before performing the operation
                    const currentValue = this.evaluateExpr(target, env);

                    switch (op) {
                        case "+=": finalValueToAssign = currentValue + assignedValue; break;
                        case "-=": finalValueToAssign = currentValue - assignedValue; break;
                        case "*=": finalValueToAssign = currentValue * assignedValue; break;
                        case "/=":
                            if (assignedValue === 0) {
                                throw new NovaError(expr, "Division by zero in compound assignment.");
                            }
                            finalValueToAssign = currentValue / assignedValue;
                            break;
                        case "%=": finalValueToAssign = currentValue % assignedValue; break;
                        default:
                            // This should theoretically not be reached if parser is correct
                            throw new NovaError(expr, `Internal error: Unknown compound assignment operator: ${op}`);
                    }
                }

                // Now, perform the actual assignment with finalValueToAssign
                // This part handles both simple assignments and the result of compound assignments
                if (target.type === "ArrayLiteral") { // Array destructuring
                    // This block will only be reached if op === "=" due to the check above
                    const sourceArray = finalValueToAssign;
                    if (!Array.isArray(sourceArray)) {
                        throw new NovaError(target, `Cannot destructure non-array value. Expected array, got ${typeof sourceArray}.`);
                    }
                    const targetArrayLiteral = target as ArrayLiteral;
                    for (let i = 0; i < targetArrayLiteral.elements.length; i++) {
                        const targetElement = targetArrayLiteral.elements[i];
                        const sourceValue = sourceArray[i];
                        // Recursively assign each element using a simple assignment
                        const tempAssignment: AssignmentExpr = {
                            type: "AssignmentExpr",
                            target: targetElement,
                            value: { type: "Literal", value: sourceValue, file: expr.file, line: expr.line, column: expr.column },
                            operator: "=", // Always simple assignment for destructuring elements
                            file: expr.file, line: expr.line, column: expr.column
                        };
                        this.evaluateExpr(tempAssignment, env);
                    }
                } else { // All other assignment targets (Identifier, ArrayAccess, PropertyAccess)
                    const { base, finalKey } = this.resolveAssignmentTarget(target, env);
                    if (base instanceof Environment) {
                        base.assign(finalKey as string, finalValueToAssign, target);
                    } else if (finalKey !== null) {
                        base[finalKey] = finalValueToAssign;
                    } else {
                        throw new NovaError(target, "Failed to resolve assignment target.");
                    }
                }
                return finalValueToAssign; // Return the value that was assigned
            }

            case "BinaryExpr": {
                const left = this.evaluateExpr(expr.left, env);
                const right = this.evaluateExpr(expr.right, env);
                switch (expr.operator) {
                    case "+": return left + right;
                    case "%": return left % right;
                    case "-": return left - right;
                    case "*": return left * right;
                    case "/":
                        if (right === 0) {
                            throw new NovaError(expr, "Division by zero is not allowed.");
                        }
                        return left / right;
                    case "==": return left === right;
                    case "!=": return left !== right;
                    case "<": return left < right;
                    case "<=": return left <= right;
                    case ">": return left > right;
                    case ">=": return left >= right;
                    case "&&": return left && right;
                    case "||": return left || right;
                    default:
                        throw new NovaError(expr, `Unknown binary operator: ${expr.operator}`);
                }
            }
            case "UnaryExpr": {
                const right = this.evaluateExpr(expr.right, env);
                switch (expr.operator) {
                    case "-":
                        if (typeof right !== 'number') {
                            throw new NovaError(expr, `Unary '-' operator can only be applied to numbers. Got: ${typeof right}`);
                        }
                        return -right;
                    case "!": return !right;
                    default:
                        throw new NovaError(expr, `Unknown unary operator: ${expr.operator}`);
                }
            }
            case "FuncCall": {
                const func = env.get(expr.name, expr); // Pass expr as token
                if (typeof func !== "function") {
                    throw new NovaError(expr, `${expr.name} is not a function`);
                }
                const args = expr.arguments.map(arg => this.evaluateExpr(arg, env));
                return func(...args);
            }
            case "MethodCall": {
                const obj = this.evaluateExpr(expr.object, env);

                if (obj === null || obj === undefined) {
                    throw new NovaError(expr, `Cannot call method '${expr.method}' on null or undefined.`);
                }

                const fn = (obj instanceof Environment) ? obj.get(expr.method, expr) : obj[expr.method]; // Pass expr as token

                if (typeof fn !== "function") {
                    throw new NovaError(expr, `${expr.method} is not a function or method on this object`);
                }

                const argVals = expr.arguments.map(arg => this.evaluateExpr(arg, env));
                return fn.apply(obj, argVals);
            }

            case "ArrayAccess": {
                // MODIFIED: Evaluate expr.object recursively
                const arr = this.evaluateExpr(expr.object, env);
                const index = this.evaluateExpr(expr.index, env);
                if (!Array.isArray(arr) && typeof arr !== 'object') {
                    throw new NovaError(expr, `Cannot access index of non-array/object: ${typeof arr}`);
                }
                if (typeof index !== 'number' && typeof index !== 'string') {
                    throw new NovaError(expr, `Array/object index must be a number or string. Got: ${typeof index}`);
                }
                return arr[index];
            }
            case "PropertyAccess": {
                // MODIFIED: Evaluate expr.object recursively
                const obj = this.evaluateExpr(expr.object, env);

                if (obj === null || obj === undefined) {
                    throw new NovaError(expr, `Cannot access property '${expr.property}' of null or undefined.`);
                }

                if (obj instanceof Environment) {
                    return obj.get(expr.property, expr); // Pass expr as token
                }

                return obj[expr.property];
            }

            case "ArrayLiteral": {
                return expr.elements.map(element => this.evaluateExpr(element, env));
            }

            case "LambdaDecl": {
                const stmt = expr;
                const func = (...args: any[]) => {
                    const funcEnv = new Environment(env);

                    if (args.length > stmt.parameters.length) {
                        throw new NovaError(stmt, `Too many arguments passed to function. Expected ${stmt.parameters.length}, got ${args.length}.`); // Removed stmt.name as lambda has no name
                    }

                    for (let i = 0; i < stmt.parameters.length; i++) {
                        const param = stmt.parameters[i];
                        let argVal = args[i];

                        // apply default if missing
                        if (argVal === undefined && param.default !== undefined) {
                            argVal = this.evaluateExpr(param.default, env);
                        } else if (argVal === undefined && param.default === undefined) {
                            // If an argument is missing and no default is provided
                            throw new NovaError(param, `Missing argument for parameter '${param.name}'.`);
                        }

                        // soft type check
                        if (param.annotationType) { // MODIFIED: Use annotationType
                            // MODIFIED: Call checkType with interpreter instance
                            checkType(param.annotationType, argVal, param, this);
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
                    return undefined; // Functions without explicit return return undefined
                };

                return func;
            }
            case "ObjectLiteral": {
                const obj: Record<string, any> = {};
                for (const prop of expr.properties) {
                    obj[prop.key] = this.evaluateExpr(prop.value, env);
                }
                return obj;
            }
            default:
                throw new NovaError(expr, `Unknown expression type: ${expr.type}`);
        }
    }
}
export default Interpreter