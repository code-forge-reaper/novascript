#!/usr/bin/env node
/**
 * NovaScript
 **/
import fs from "node:fs";
import path from "node:path";
import util from "node:util";
String.prototype.concat = function (...others) {
    let result = String(this); // convert wrapper object to primitive string
    for (const other of others) {
        result += other + " ";
    }
    return result;
};

String.prototype.format = function (...args) {
    return util.format(this, ...args);
}

interface Token {
    type: string;
    // @ts-ignore
    value?: any;
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

// NEW: Class related interfaces
interface ClassDefinition extends Statement {
    type: "ClassDefinition";
    name: string;
    superclassName: string | null; // Name of the superclass if it extends one
    body: (MethodDefinition | PropertyDefinition)[];
}

interface MethodDefinition extends Statement {
    type: "MethodDefinition";
    name: string;
    parameters: Parameter[];
    body: Statement[];
    isStatic: boolean;
    isConstructor: boolean; // True if this method is the constructor
}

interface PropertyDefinition extends Statement {
    type: "PropertyDefinition";
    name: string;
    typeAnnotation: string | null;
    initializer: Expression | null;
    isStatic: boolean;
}

interface NewInstance extends Expression {
    type: "NewInstance";
    className: string;
    arguments: Expression[];
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
        patch: 5
    };

    // Create a dummy token for internal/bootstrap errors in initGlobals
    const internalToken: Token = { type: "internal", value: "init", file: "internal_init.ts", line: 1, column: 1 };

    globals.define("isArray", Array.isArray);
    // globals.define("new", (className: any, ...args: any[]) => new className(...args)); // REMOVED: 'new' is now a keyword for class instantiation

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

// NEW: Runtime representation of a NovaScript class
class NovaClass {
    name: string;
    superClass: NovaClass | null;
    staticMembers: Map<string, any>;
    instanceProperties: Map<string, PropertyDefinition>;
    instanceMethods: Map<string, MethodDefinition>;
    staticProperties: Map<string, PropertyDefinition>; // NEW: To store static property definitions for type checking
    constructorDef: MethodDefinition | null;
    interpreter: Interpreter;
    env: Environment; // Environment where the class was defined (for evaluating initializers/defaults)

    constructor(
        name: string,
        superClass: NovaClass | null,
        interpreter: Interpreter,
        env: Environment
    ) {
        this.name = name;
        this.superClass = superClass;
        this.interpreter = interpreter;
        this.env = env;
        this.staticMembers = new Map();
        this.instanceProperties = new Map();
        this.instanceMethods = new Map();
        this.staticProperties = new Map(); // NEW: Initialize staticProperties map
        this.constructorDef = null;
    }

    // This method will be called to create an instance
    instantiate(args: any[], instanceToken: Token, instanceObj?: any): any {
        const instance = instanceObj || {}; // If instanceObj is provided, use it (for super calls)

        // NEW: Link the instance to its NovaClass definition
        Object.defineProperty(instance, '__novaClass__', {
            value: this,
            enumerable: false,
            configurable: true // Allow re-definition if needed, though not expected
        });

        // First, handle superclass construction if applicable and this is the top-level call
        if (this.superClass && !instanceObj) { // Only call superclass constructor if this is the initial instantiation
            this.superClass.instantiate(args, instanceToken, instance); // Pass the current instance to superclass
        }

        // Initialize instance properties
        for (const [propName, propDef] of this.instanceProperties.entries()) {
            let propValue;
            if (propDef.initializer) {
                // Evaluate initializer in the context of the class definition environment
                propValue = this.interpreter.evaluateExpr(propDef.initializer, this.env);
            } else {
                propValue = undefined; // Default value if no initializer
            }

            Object.defineProperty(instance, propName, {
                value: propValue,
                writable: true,
                enumerable: true,
                configurable: true
            });
        }

        // Bind instance methods to the instance
        for (const [methodName, methodDef] of this.instanceMethods.entries()) {
            //console.log(methodName)
            // Create a JS function that wraps the NovaScript method body
            const novaMethod = (...methodArgs: any[]) => {
                const methodEnv = new Environment(this.env); // Method's scope
                methodEnv.define("self", instance); // 'self' refers to the instance

                // Handle 'super' call within instance methods
                if (this.superClass) {
                    methodEnv.define("super", (...superMethodArgs: any[]) => {
                        const superMethod = this.superClass!.instanceMethods.get(methodName); // Get super method definition
                        if (!superMethod) {
                            throw new NovaError(instanceToken, `Method '${methodName}' not found in superclass '${this.superClass!.name}'.`);
                        }
                        // Create a temporary function to execute the super method
                        const tempSuperFunc = (...tempArgs: any[]) => {
                            const tempSuperEnv = new Environment(this.superClass!.env);
                            tempSuperEnv.define("self", instance); // 'self' still refers to the current instance
                            for (let i = 0; i < superMethod.parameters.length; i++) {
                                const param = superMethod.parameters[i];
                                let argVal = tempArgs[i];
                                if (argVal === undefined && param.default !== undefined) {
                                    argVal = this.interpreter.evaluateExpr(param.default, this.superClass!.env);
                                } else if (argVal === undefined && param.default === undefined) {
                                    throw new NovaError(param, `Missing argument for parameter '${param.name}' in super method '${methodName}'.`);
                                }
                                if (param.annotationType) {
                                    checkType(param.annotationType, argVal, param, this.interpreter);
                                }
                                tempSuperEnv.define(param.name, argVal);
                            }
                            try {
                                this.interpreter.executeBlock(superMethod.body, tempSuperEnv);
                            } catch (e) {
                                if (e instanceof ReturnException) {
                                    return e.value;
                                }
                                throw e;
                            }
                            return undefined;
                        };
                        return tempSuperFunc(...superMethodArgs);
                    });
                }

                // Handle parameters for current method
                for (let i = 0; i < methodDef.parameters.length; i++) {
                    const param = methodDef.parameters[i];
                    let argVal = methodArgs[i];
                    if (argVal === undefined && param.default !== undefined) {
                        argVal = this.interpreter.evaluateExpr(param.default, this.env); // Evaluate default in class's env
                    } else if (argVal === undefined && param.default === undefined) {
                        throw new NovaError(param, `Missing argument for parameter '${param.name}' in method '${methodDef.name}'.`);
                    }
                    if (param.annotationType) {
                        checkType(param.annotationType, argVal, param, this.interpreter);
                    }
                    methodEnv.define(param.name, argVal);
                }

                try {
                    this.interpreter.executeBlock(methodDef.body, methodEnv);
                } catch (e) {
                    if (e instanceof ReturnException) {
                        return e.value;
                    }
                    throw e;
                }
                return undefined;
            };
            Object.defineProperty(instance, methodName, {
                value: novaMethod,
                writable: true,
                enumerable: true,
                configurable: true
            });
        }


        // Handle constructor call (only if this is the top-level instantiation)
        if (!instanceObj) {
            if (this.constructorDef) {
                const constructorEnv = new Environment(this.env);
                constructorEnv.define("self", instance);
                // Define 'super' for constructor's environment
                if (this.superClass) {
                    constructorEnv.define("super", (...superArgs: any[]) => {
                        if (!this.superClass) { // Should not happen given the check above
                            throw new NovaError(instanceToken, "Cannot call super() in a class without a superclass.");
                        }
                        this.superClass.instantiate(superArgs, instanceToken, instance); // Pass current instance
                    });
                }

                // Handle constructor parameters
                for (let i = 0; i < this.constructorDef.parameters.length; i++) {
                    const param = this.constructorDef.parameters[i];
                    let argVal = args[i];
                    if (argVal === undefined && param.default !== undefined) {
                        argVal = this.interpreter.evaluateExpr(param.default, this.env);
                    } else if (argVal === undefined && param.default === undefined) {
                        throw new NovaError(param, `Missing argument for parameter '${param.name}' in constructor of '${this.name}'.`);
                    }
                    // Type checking for constructor parameters (already there)
                    if (param.annotationType) {
                        checkType(param.annotationType, argVal, param, this.interpreter);
                    }
                    constructorEnv.define(param.name, argVal);
                }

                try {
                    this.interpreter.executeBlock(this.constructorDef.body, constructorEnv);
                } catch (e) {
                    if (e instanceof ReturnException) {
                        // Constructors don't typically return values, but if they do, ignore it.
                    } else {
                        throw e;
                    }
                }
            }
        }

        return instance;
    }
}


export class Interpreter {
    keywords: string[] = [
        "var", "if", "else", "elseif", "end", "break", "continue", "func",
        "return", "import", "as", "namespace", "while", "forEach", "for",
        "do", "in", "test", "failed", "defer",
        "switch", "case", "default", "using",
        // "def" = "(...)=>{...}"
        // def (...) ... end
        "def",
        // type rect = {x : number,y: number, width:number, height: number}
        "type",
        // NEW: Class keywords
        "class", "inherits", "static", "new", "super"
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

    /**
     * Exposes a JavaScript class to the NovaScript environment.
     * NovaScript code can then instantiate and interact with this class using 'new' keyword.
     * @param name The name by which the class will be known in NovaScript.
     * @param jsClass The JavaScript class (constructor function) to expose.
     */
    exposeJsClass(name: string, jsClass: any): void {
        if (typeof jsClass !== 'function' || !jsClass.prototype || typeof jsClass.prototype.constructor !== 'function') {
            throw new Error(`Cannot expose '${name}': Provided value is not a valid JavaScript class constructor.`);
        }
        this.globals.define(name, jsClass);
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
                    if (source[i] === "\n") {
                        col = 1
                        line++
                    }
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
            if (t)
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

    // NEW: parseClassDefinition method
    parseClassDefinition(): ClassDefinition {
        const classToken = this.consumeToken(); // consume 'class'
        const nameToken = this.expectType("identifier");
        const name = nameToken.value;
        this.consumeToken(); // consume class name

        let superclassName: string | null = null;
        if (this.getNextToken()?.value === "inherits") { // Using 'inherits' as proposed
            this.consumeToken(); // consume 'inherits'
            const superNameToken = this.expectType("identifier");
            superclassName = superNameToken.value;
            this.consumeToken(); // consume superclass name
        }

        let hasInitializer = false
        const body: (MethodDefinition | PropertyDefinition)[] = [];
        while (this.getNextToken() && this.getNextToken()!.value !== "end") {
            const memberToken = this.getNextToken()!;
            let isStatic = false;
            if (memberToken.type === "keyword" && memberToken.value === "static") {
                this.consumeToken(); // consume 'static'
                isStatic = true;
            }

            if (this.getNextToken()?.type === "keyword" && this.getNextToken()!.value === "var") {
                // Property Definition
                this.consumeToken(); // consume 'var'
                const propNameToken = this.expectType("identifier");
                const propName = propNameToken.value;
                this.consumeToken();

                let typeAnnotation: string | null = null;
                if (this.getNextToken()?.type === "operator" && this.getNextToken()!.value === ":") {
                    this.consumeToken(); // consume ':'
                    const typeToken = this.expectType("identifier");
                    typeAnnotation = typeToken.value;
                    this.consumeToken();
                }

                let initializer: Expression | null = null;
                if (this.getNextToken()?.type === "operator" && this.getNextToken()!.value === "=") {
                    this.consumeToken(); // consume '='
                    initializer = this.parseExpression();
                }
                body.push({
                    type: "PropertyDefinition",
                    name: propName,
                    typeAnnotation,
                    initializer,
                    isStatic,
                    file: propNameToken.file, line: propNameToken.line, column: propNameToken.column
                });
            } else if (this.getNextToken()?.type === "keyword" && this.getNextToken()!.value === "func") {
                // Method Definition
                this.consumeToken(); // consume 'func'
                const methodNameToken = this.expectType("identifier");
                const methodName = methodNameToken.value;
                this.consumeToken();

                let isConstructor = false;
                if (methodName === "init") {
                    isConstructor = true;
                    hasInitializer = true
                }

                this.expectToken("(");
                this.consumeToken();

                const parameters: Parameter[] = [];
                if (this.getNextToken() && this.getNextToken()!.value !== ")") {
                    while (true) {
                        const paramToken = this.expectType("identifier");
                        const paramName = paramToken.value;
                        this.consumeToken();

                        let annotationType: string | null = null;
                        if (this.getNextToken()?.type === "identifier") {
                            const typeToken = this.getNextToken()!;
                            if (["string", "number", "bool", "boolean", ...this.customTypes.keys()].includes(typeToken.value)) {
                                annotationType = typeToken.value;
                                this.consumeToken();
                            }
                        }

                        let defaultExpr: Expression | undefined;
                        if (this.getNextToken()?.value === "=") {
                            this.consumeToken();
                            defaultExpr = this.parseExpression();
                        }

                        parameters.push({
                            name: paramName,
                            annotationType: annotationType,
                            default: defaultExpr,
                            file: paramToken.file, line: paramToken.line, column: paramToken.column,
                            type: paramToken.type, value: paramToken.value
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

                const bodyStatements = this.parseBlockUntil(["end"]);
                this.expectToken("end");
                this.consumeToken();

                body.push({
                    type: "MethodDefinition",
                    name: methodName,
                    parameters,
                    body: bodyStatements,
                    isStatic,
                    isConstructor,
                    file: methodNameToken.file, line: methodNameToken.line, column: methodNameToken.column
                });
            } else {
                throw new NovaError(memberToken, `Unexpected token in class body: ${memberToken.value}`);
            }
        }

        if (!hasInitializer) {
            throw new NovaError(classToken, "this class has no `init` initializer function")
        }

        this.expectToken("end");
        this.consumeToken(); // consume 'end'

        return {
            type: "ClassDefinition",
            name,
            superclassName,
            body,
            file: classToken.file, line: classToken.line, column: classToken.column
        };
    }

    // ----------------------
    // Parsing Statements and Expressions
    // ----------------------
    parseStatement(): Statement | undefined {
        const token = this.getNextToken();
        if (!token) {
            const lastToken = this.tokens[this.current - 1] || { type: "EOF", value: "EOF", file: this.file, line: 1, column: 1 };
            throw new NovaError(lastToken, "Unexpected end of input");
        }

        // Refactored to use switch-case for keyword statements
        if (token.type === "keyword") {
            switch (token.value) {
                case "defer":
                    this.consumeToken();
                    const deferBody = this.parseBlockUntil(["end"]);
                    this.expectToken("end");
                    this.consumeToken();
                    return { type: "DeferStmt", body: deferBody, line: token.line, column: token.column, file: token.file };

                case "type":
                    this.consumeToken(); // consume 'type'
                    const customTypeNameToken = this.expectType("identifier");
                    const customTypeName = customTypeNameToken.value;
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
                    const customTypeStmt: CustomTypeDeclStmt = {
                        type: "CustomTypeDecl",
                        name: customTypeName,
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
                    return undefined; // the statement is not handled at runtime, don't return it

                case "var":
                    this.consumeToken();
                    const varNameToken = this.expectType("identifier");
                    const varName = varNameToken.value;
                    this.consumeToken();

                    let varAnnotationType: string | null = null;
                    if (
                        this.getNextToken() &&
                        this.getNextToken()!.type === "identifier" &&
                        ["string", "number", "boolean", "void", "bool", ...this.customTypes.keys()].includes(this.getNextToken()!.value)
                    ) {
                        varAnnotationType = this.getNextToken()!.value;
                        this.consumeToken();
                    }

                    this.expectToken("=");
                    this.consumeToken();
                    const varInitializer = this.parseExpression();
                    return { type: "VarDecl", name: varName, typeAnnotation: varAnnotationType, initializer: varInitializer, line: token.line, column: token.column, file: token.file };

                case "switch":
                    this.consumeToken();
                    const switchExpr = this.parseExpression();
                    const cases: Case[] = [];
                    while (this.getNextToken()?.type === "keyword" && this.getNextToken()!.value === "case") {
                        this.consumeToken();
                        const caseExpr = this.parseExpression();
                        this.expectToken("do");
                        this.consumeToken();
                        const caseBody = this.parseBlockUntil(["end"]);
                        this.expectToken("end");
                        this.consumeToken();
                        cases.push({ caseExpr, body: caseBody });
                    }
                    if (this.getNextToken()?.type === "keyword" && this.getNextToken()!.value === "default") {
                        this.consumeToken();
                        this.expectToken("do");
                        this.consumeToken();
                        const defaultBody = this.parseBlockUntil(["end"]);
                        this.expectToken("end");
                        this.consumeToken();
                        cases.push({ caseExpr: null, body: defaultBody });
                    }
                    this.expectToken("end");
                    this.consumeToken();
                    return { type: "SwitchStmt", expression: switchExpr, cases, line: token.line, column: token.column, file: token.file };

                case "using":
                    this.consumeToken();
                    const usingNameToken = this.expectType("identifier");
                    this.consumeToken();
                    return { type: "UsingStmt", name: usingNameToken.value, line: token.line, column: token.column, file: token.file };

                case "test":
                    this.consumeToken();
                    const tryBlock = this.parseBlockUntil(["failed"]);
                    this.expectToken("failed");
                    this.consumeToken();
                    const errorVarToken = this.expectType("identifier");
                    const errorVar = errorVarToken.value;
                    this.consumeToken();
                    const catchBlock = this.parseBlockUntil(["end"]);
                    this.expectToken("end");
                    this.consumeToken();
                    return { type: "TryStmt", tryBlock, errorVar, catchBlock, line: token.line, column: token.column, file: token.file };

                case "forEach":
                    this.consumeToken();
                    const forEachVarToken = this.expectType("identifier");
                    const forEachVariable = forEachVarToken.value;
                    this.consumeToken();
                    this.expectToken("in");
                    this.consumeToken();
                    const forEachListExpr = this.parseExpression();
                    this.expectToken("do");
                    this.consumeToken();
                    const forEachBody = this.parseBlockUntil(["end"]);
                    this.expectToken("end");
                    this.consumeToken();
                    return { type: "ForEachStmt", variable: forEachVariable, list: forEachListExpr, body: forEachBody, line: token.line, column: token.column, file: token.file };

                case "for":
                    this.consumeToken();
                    const forVarToken = this.expectType("identifier");
                    const forVariable = forVarToken.value;
                    this.consumeToken();
                    this.expectToken("=");
                    this.consumeToken();
                    const forStartExpr = this.parseExpression();
                    this.expectToken(",");
                    this.consumeToken();
                    const forEndExpr = this.parseExpression();
                    let forStepExpr: Expression | undefined;
                    if (this.getNextToken()?.value === ",") {
                        this.consumeToken();
                        forStepExpr = this.parseExpression();
                    }
                    this.expectToken("do");
                    this.consumeToken();
                    const forBody = this.parseBlockUntil(["end"]);
                    this.expectToken("end");
                    this.consumeToken();
                    return { type: "ForStmt", variable: forVariable, start: forStartExpr, end: forEndExpr, step: forStepExpr, body: forBody, line: token.line, column: token.column, file: token.file };

                case "while":
                    this.consumeToken();
                    const whileCondition = this.parseExpression();
                    const whileBody = this.parseBlockUntil(["end"]);
                    this.expectToken("end");
                    this.consumeToken();
                    return { type: "WhileStmt", condition: whileCondition, body: whileBody, line: token.line, column: token.column, file: token.file };

                case "break":
                    this.consumeToken();
                    return { type: "BreakStmt", line: token.line, column: token.column, file: token.file };

                case "continue":
                    this.consumeToken();
                    return { type: "ContinueStmt", line: token.line, column: token.column, file: token.file };

                case "if":
                    this.consumeToken();
                    const ifCondition = this.parseExpression();
                    const thenBlock = this.parseBlockUntil(["else", "elseif", "end"]);

                    const elseIfBlocks: { condition: Expression, body: Statement[] }[] = [];
                    let elseBlock: Statement[] | null = null;

                    while (this.getNextToken()?.type === "keyword" && this.getNextToken()!.value === "elseif") {
                        this.consumeToken();
                        const elseifCondition = this.parseExpression();
                        const elseifBody = this.parseBlockUntil(["else", "elseif", "end"]);
                        elseIfBlocks.push({ condition: elseifCondition, body: elseifBody });
                    }

                    if (this.getNextToken()?.type === "keyword" && this.getNextToken()!.value === "else") {
                        this.consumeToken();
                        elseBlock = this.parseBlockUntil(["end"]);
                    }

                    this.expectToken("end");
                    this.consumeToken();

                    return {
                        type: "IfStmt",
                        condition: ifCondition,
                        thenBlock,
                        elseBlock,
                        elseIf: elseIfBlocks.length > 0 ? elseIfBlocks : null,
                        line: token.line, column: token.column, file: token.file
                    };

                case "namespace":
                    this.consumeToken();
                    const namespaceNameToken = this.expectType("identifier");
                    const namespaceName = namespaceNameToken.value;
                    this.consumeToken();
                    const namespaceBody = this.parseBlockUntil(["end"]);
                    this.expectToken("end");
                    this.consumeToken();
                    return { type: "NamespaceStmt", name: namespaceName, body: namespaceBody, line: token.line, column: token.column, file: token.file };

                case "func":
                    this.consumeToken();
                    const funcNameToken = this.expectType("identifier");
                    const funcName = funcNameToken.value;
                    this.consumeToken();
                    this.expectToken("(");
                    this.consumeToken();

                    const funcParameters: Parameter[] = [];
                    if (this.getNextToken() && this.getNextToken()!.value !== ")") {
                        while (true) {
                            const paramToken = this.expectType("identifier");
                            const paramName = paramToken.value;
                            this.consumeToken();

                            let annotationType: string | null = null;
                            if (this.getNextToken()?.type === "identifier") {
                                const typeToken = this.getNextToken()!;
                                if (["string", "number", "bool", "boolean", ...this.customTypes.keys()].includes(typeToken.value)) {
                                    annotationType = typeToken.value;
                                    this.consumeToken();
                                }
                            }

                            let defaultExpr: Expression | undefined;
                            if (this.getNextToken()?.value === "=") {
                                this.consumeToken();
                                defaultExpr = this.parseExpression();
                                if (annotationType) {
                                    throw new NovaError(token, "Cannot have both explicit type annotation and a default value. Consider removing the type annotation to allow type inference from the default value.");
                                }

                                if (defaultExpr.type === "Literal") {
                                    const inferredType = typeof (defaultExpr as Literal).value;
                                    if (inferredType === "string") annotationType = "string";
                                    else if (inferredType === "number") annotationType = "number";
                                    else if (inferredType === "boolean") annotationType = "bool";
                                    else annotationType = null;
                                } else {
                                    annotationType = null;
                                }
                            }

                            funcParameters.push({
                                name: paramName,
                                annotationType: annotationType,
                                default: defaultExpr,
                                file: paramToken.file, line: paramToken.line, column: paramToken.column,
                                type: paramToken.type, value: paramToken.value
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
                    const funcBody = this.parseBlockUntil(["end"]);
                    this.expectToken("end");
                    this.consumeToken();

                    return {
                        type: "FuncDecl",
                        name: funcName,
                        parameters: funcParameters,
                        body: funcBody,
                        file: token.file, line: token.line, column: token.column
                    };

                case "def":
                    this.consumeToken();
                    this.expectToken("(");
                    this.consumeToken();

                    const lambdaParameters: Parameter[] = [];
                    if (this.getNextToken() && this.getNextToken()!.value !== ")") {
                        while (true) {
                            const paramToken = this.expectType("identifier");
                            const paramName = paramToken.value;
                            this.consumeToken();

                            let annotationType: string | null = null;
                            if (this.getNextToken()?.type === "identifier") {
                                const typeToken = this.getNextToken()!;
                                if (["string", "number", "bool", "boolean", ...this.customTypes.keys()].includes(typeToken.value)) {
                                    annotationType = typeToken.value;
                                    this.consumeToken();
                                }
                            }

                            let defaultExpr: Expression | undefined;
                            if (this.getNextToken()?.value === "=") {
                                this.consumeToken();
                                defaultExpr = this.parseExpression();
                                if (annotationType) {
                                    throw new NovaError(token, "Cannot have both explicit type annotation and a default value. Consider removing the type annotation to allow type inference from the default value.");
                                }

                                if (defaultExpr.type === "Literal") {
                                    const inferredType = typeof (defaultExpr as Literal).value;
                                    if (inferredType === "string") annotationType = "string";
                                    else if (inferredType === "number") annotationType = "number";
                                    else if (inferredType === "boolean") annotationType = "bool";
                                    else annotationType = null;
                                } else {
                                    annotationType = null;
                                }
                            }

                            lambdaParameters.push({
                                name: paramName,
                                annotationType: annotationType,
                                default: defaultExpr,
                                file: paramToken.file, line: paramToken.line, column: paramToken.column,
                                type: paramToken.type, value: paramToken.value
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
                    const lambdaBody = this.parseBlockUntil(["end"]);
                    this.expectToken("end");
                    this.consumeToken();

                    return {
                        type: "LambdaDecl",
                        parameters: lambdaParameters,
                        body: lambdaBody,
                        file: token.file, line: token.line, column: token.column
                    };

                case "return":
                    this.consumeToken();
                    let returnExpression: Expression | null = null;
                    if (this.getNextToken() && !this.keywords.includes(this.getNextToken()!.value)) {
                        returnExpression = this.parseExpression();
                    }
                    return { type: "ReturnStmt", expression: returnExpression, file: token.file, line: token.line, column: token.column };

                case "import":
                    this.consumeToken();
                    const fileToken = this.expectType("string");
                    const filename = fileToken.value;
                    this.consumeToken();
                    let alias: string | null = null;
                    if (this.getNextToken() && this.getNextToken()!.value === "as") {
                        this.consumeToken();
                        const aliasToken = this.expectType("identifier");
                        alias = aliasToken.value;
                        this.consumeToken();
                    }
                    return { type: "ImportStmt", filename, alias, file: token.file, line: token.line, column: token.column };

                case "class":
                    return this.parseClassDefinition();
            }
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
        else if (token.type === "keyword" && token.value === "new") { // NEW: 'new' keyword for class instantiation
            this.consumeToken(); // consume 'new'
            const classNameToken = this.expectType("identifier");
            const className = classNameToken.value;
            this.consumeToken(); // consume class name

            this.expectToken("(");
            this.consumeToken();

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
            this.consumeToken();
            node = {
                type: "NewInstance",
                className,
                arguments: args,
                file: classNameToken.file, line: classNameToken.line, column: classNameToken.column
            };
        }
        else if (token.value == "def") {
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
                    if (this.getNextToken()?.type === "identifier") {
                        const typeToken = this.getNextToken()!; // Added !
                        if (["string", "number", "bool", "boolean"].includes(typeToken.value)) { // Added "boolean"
                            annotationType = typeToken.value; // MODIFIED: Renamed
                            this.consumeToken();
                        }
                    }

                    // optional default value
                    let defaultExpr: Expression | undefined;
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
                stmt.body.forEach((e: Statement) => stack.push(e));
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

                    /*
                    if (args.length > stmt.parameters.length) {
                        throw new NovaError(stmt, `Too many arguments passed to function '${stmt.name}'. Expected ${stmt.parameters.length}, got ${args.length}.`);
                    }
                    */

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

            case "ClassDefinition": { // NEW: Handle ClassDefinition
                const classDef = stmt as ClassDefinition;
                const novaClass = new NovaClass(classDef.name, null, this, env); // Superclass will be resolved later

                // Populate static members, instance properties/methods
                for (const member of classDef.body) {
                    if (member.type === "PropertyDefinition") {
                        if (member.isStatic) {
                            // NEW: Store the PropertyDefinition for static members
                            novaClass.staticProperties.set(member.name, member);
                            let propValue;
                            if (member.initializer) {
                                propValue = this.evaluateExpr(member.initializer, env);
                            } else {
                                propValue = undefined;
                            }
                            // Type check static properties at definition time (already there)
                            if (member.typeAnnotation) {
                                checkType(member.typeAnnotation, propValue, member, this);
                            }
                            novaClass.staticMembers.set(member.name, propValue); // Store the value
                        } else {
                            novaClass.instanceProperties.set(member.name, member);
                        }
                    } else if (member.type === "MethodDefinition") {
                        if (member.isConstructor) {
                            novaClass.constructorDef = member;
                        } else if (member.isStatic) {
                            // Wrap static methods similar to how FuncDecl is handled
                            const staticMethod = (...args: any[]) => {
                                const methodEnv = new Environment(env); // Static methods run in the class's definition environment
                                // Define 'self' for static methods to refer to the static members map
                                methodEnv.define("self", novaClass.staticMembers);
                                // Handle parameters and execute body
                                for (let i = 0; i < member.parameters.length; i++) {
                                    const param = member.parameters[i];
                                    let argVal = args[i];
                                    if (argVal === undefined && param.default !== undefined) {
                                        argVal = this.evaluateExpr(param.default, env);
                                    } else if (argVal === undefined && param.default === undefined) {
                                        throw new NovaError(param, `Missing argument for parameter '${param.name}' in static method '${member.name}'.`);
                                    }
                                    if (param.annotationType) {
                                        checkType(param.annotationType, argVal, param, this);
                                    }
                                    methodEnv.define(param.name, argVal);
                                }
                                try {
                                    this.executeBlock(member.body, methodEnv);
                                } catch (e) {
                                    if (e instanceof ReturnException) {
                                        return e.value;
                                    }
                                    throw e;
                                }
                                return undefined;
                            };
                            novaClass.staticMembers.set(member.name, staticMethod);
                        } else {
                            novaClass.instanceMethods.set(member.name, member);
                        }
                    }
                }

                // Resolve superclass if exists
                if (classDef.superclassName) {
                    const superClass = env.get(classDef.superclassName, classDef) as NovaClass;
                    if (!superClass || !(superClass instanceof NovaClass)) {
                        throw new NovaError(classDef, `Superclass '${classDef.superclassName}' not found or is not a class.`);
                    }
                    novaClass.superClass = superClass;
                }

                env.define(classDef.name, novaClass);
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
                const assignmentExpr = expr as AssignmentExpr;
                const target = assignmentExpr.target;
                let assignedValue = this.evaluateExpr(assignmentExpr.value, env);
                const op = assignmentExpr.operator; // Get the specific operator

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

                    // NEW: Type checking for assignments to properties
                    if (target.type === "PropertyAccess") {
                        const propAccessTarget = target as PropertyAccess;
                        // Re-evaluate the object part to get the actual runtime object (instance or class)
                        const objBeingAssignedTo = this.evaluateExpr(propAccessTarget.object, env);

                        let propDef: PropertyDefinition | undefined;

                        // Check if it's an assignment to a static property of a NovaClass
                        if (objBeingAssignedTo instanceof NovaClass) {
                            propDef = objBeingAssignedTo.staticProperties.get(propAccessTarget.property);
                        }
                        // Check if it's an assignment to an instance property of a NovaScript object
                        else if (objBeingAssignedTo && typeof objBeingAssignedTo === 'object' && (objBeingAssignedTo as any).__novaClass__ instanceof NovaClass) {
                            const novaClassDef = (objBeingAssignedTo as any).__novaClass__ as NovaClass;
                            propDef = novaClassDef.instanceProperties.get(propAccessTarget.property);
                        }

                        if (propDef && propDef.typeAnnotation) {
                            checkType(propDef.typeAnnotation, finalValueToAssign, assignmentExpr, this);
                        }
                    }
                    // END NEW: Type checking for assignments to properties

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

                if (typeof left !== 'number' || typeof right !== 'number') {
                    throw new NovaError(expr, `Cannot perform binary operation on non-number types: ${typeof left} and ${typeof right}`);
                }
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
                const args = expr.arguments.map((arg: Expression) => this.evaluateExpr(arg, env));
                return func(...args);
            }
            case "MethodCall": {
                const methodCallExpr = expr as MethodCall;
                const obj = this.evaluateExpr(methodCallExpr.object, env);

                if (obj === null || obj === undefined) {
                    throw new NovaError(methodCallExpr, `Cannot call method '${methodCallExpr.method}' on null or undefined.`);
                }

                const argVals = methodCallExpr.arguments.map(arg => this.evaluateExpr(arg, env));

                // Handle NovaClass static methods
                if (obj instanceof NovaClass) {
                    const staticMethod = obj.staticMembers.get(methodCallExpr.method);
                    if (typeof staticMethod === "function") {
                        // Call static method with 'self' context as the static members map (or the class itself)
                        return staticMethod.apply(obj.staticMembers, argVals); // Or obj for a more JS-like 'self' in static methods
                    }
                    // If not a static method, it might be an instance method called on the class itself, which is an error
                    throw new NovaError(methodCallExpr, `Static method '${methodCallExpr.method}' not found or is not a function on class '${obj.name}'.`);
                }

                // Handle NovaScript instance methods (bound to 'self' in NovaClass.instantiate)
                // or regular JavaScript object methods
                let fn;
                if (obj instanceof Environment) { // This case is for NovaScript's 'self' environment
                    fn = obj.get(methodCallExpr.method, methodCallExpr);
                } else {
                    // For regular JS objects or NovaScript instances
                    fn = obj[methodCallExpr.method];
                }

                if (typeof fn !== "function") {
                    throw new NovaError(methodCallExpr, `${methodCallExpr.method} is not a function or method on this object`);
                }

                // Call the method. 'apply' correctly sets 'self' to 'obj'.
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
                const propertyAccessExpr = expr as PropertyAccess;
                const obj = this.evaluateExpr(propertyAccessExpr.object, env);

                if (obj === null || obj === undefined) {
                    throw new NovaError(propertyAccessExpr, `Cannot access property '${propertyAccessExpr.property}' of null or undefined.`);
                }

                // Handle NovaClass static properties
                if (obj instanceof NovaClass) {
                    if (obj.staticMembers.has(propertyAccessExpr.property)) {
                        return obj.staticMembers.get(propertyAccessExpr.property);
                    }
                    // If not a static property, it might be an instance property accessed on the class itself, which is an error
                    throw new NovaError(propertyAccessExpr, `Static property '${propertyAccessExpr.property}' not found on class '${obj.name}'.`);
                }

                // Handle NovaScript instance properties (via 'self' environment)
                // or regular JavaScript object properties
                if (obj instanceof Environment) {
                    return obj.get(propertyAccessExpr.property, propertyAccessExpr);
                }

                // For regular JS objects or functions (for static properties)
                if (typeof obj === 'object' || typeof obj === 'function') {
                    if (propertyAccessExpr.property in obj) {
                        return obj[propertyAccessExpr.property];
                    }
                }

                throw new NovaError(propertyAccessExpr, `Property '${propertyAccessExpr.property}' not found on object.`);
            }

            case "ArrayLiteral": {
                return expr.elements.map((element: Expression) => this.evaluateExpr(element, env));
            }

            case "ObjectLiteral": {
                const obj: Record<string, any> = {};
                for (const prop of expr.properties) {
                    obj[prop.key] = this.evaluateExpr(prop.value, env);
                }
                return obj;
            }

            case "NewInstance": { // NEW: Handle NewInstance expression
                const newInstanceExpr = expr as NewInstance;
                const className = newInstanceExpr.className;
                const targetClass = env.get(className, newInstanceExpr); // Get the class definition from the environment

                const args = newInstanceExpr.arguments.map(arg => this.evaluateExpr(arg, env));

                if (targetClass instanceof NovaClass) {
                    // It's a NovaScript class
                    return targetClass.instantiate(args, newInstanceExpr);
                } else if (typeof targetClass === 'function' && targetClass.prototype && typeof targetClass.prototype.constructor === 'function') {
                    // It's a JavaScript class/constructor function
                    try {
                        // Use Reflect.construct to properly handle 'new' with arbitrary constructors
                        return Reflect.construct(targetClass, args);
                    } catch (e: any) {
                        throw new NovaError(newInstanceExpr, `Error instantiating JavaScript class '${className}': ${e.message}`);
                    }
                } else {
                    throw new NovaError(newInstanceExpr, `'${className}' is not a constructible class.`);
                }
            }

            case "LambdaDecl": {
                const stmt = expr;
                const func = (...args: any[]) => {
                    const funcEnv = new Environment(env);
                    /*
                    if (args.length > stmt.parameters.length) {
                        throw new NovaError(stmt, `Too many arguments passed to function '${stmt.name}'. Expected ${stmt.parameters.length}, got ${args.length}.`);
                    }
                    */

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
            default:
                throw new NovaError(expr, `Unknown expression type: ${expr.type}`);
        }
    }
}
export default Interpreter