#!/usr/bin/env node
/**
 * Parsel - a NovaScript subset for people that like VNs
 **/
import fs from "node:fs";
import path from "node:path";

// --- Shared Interfaces and Classes ---

export interface Token {
    type: string;
    value?: any;
    file: string;
    line: number;
    column: number;
}

export interface Statement extends Token {
    type: string;
    [key: string]: any;
}

export interface Expression extends Token {
    type: string;
    [key: string]: any;
}

export interface Parameter extends Token {
    name: string;
    annotationType: string | null;
    default?: Expression;
}

export interface VarDeclStmt extends Statement {
    type: "VarDecl";
    name: string;
    typeAnnotation: string | null;
    initializer: Expression;
}

export interface AssignmentExpr extends Expression {
    type: "AssignmentExpr";
    target: Expression;
    value: Expression;
    operator: string;
}

export interface Identifier extends Expression {
    type: "Identifier";
    name: string;
}

export interface FuncDecl extends Statement {
    type: "FuncDecl";
    name: string;
    parameters: Parameter[];
    body: Statement[];
}

export interface LambdaDecl extends Expression {
    type: "LambdaDecl";
    parameters: Parameter[];
    body: Statement[];
}

export interface ReturnStmt extends Statement {
    type: "ReturnStmt";
    expression: Expression | null;
}

export interface UsingStmt extends Statement {
    type: "UsingStmt";
    name: string; // Added name property
}

export interface ExpressionStmt extends Statement {
    type: "ExpressionStmt";
    expression: Expression;
}

export interface Literal extends Expression {
    type: "Literal";
    value: any;
}

// New/Updated for expressions
export interface BinaryExpr extends Expression {
    type: "BinaryExpr";
    operator: string;
    left: Expression;
    right: Expression;
}

export interface UnaryExpr extends Expression {
    type: "UnaryExpr";
    operator: string;
    right: Expression;
}

export interface ObjectLiteral extends Expression {
    type: "ObjectLiteral";
    properties: { key: string; value: Expression }[];
}

export interface ArrayLiteral extends Expression {
    type: "ArrayLiteral";
    elements: Expression[];
}

export interface FunctionCall extends Expression {
    type: "FuncCall";
    name: string;
    arguments: Expression[];
}

export interface MethodCall extends Expression {
    type: "MethodCall";
    object: Expression;
    method: string;
    arguments: Expression[];
}

export interface PropertyAccess extends Expression {
    type: "PropertyAccess";
    object: Expression;
    property: string;
}

export interface ArrayAccess extends Expression {
    type: "ArrayAccess";
    object: Expression;
    index: Expression;
}


// --- Ren'Py Specific AST Nodes ---

export interface CharDeclStmt extends Statement {
    type: "CharDecl";
    name: string;
    displayName: Expression; // Changed to Expression
}

export interface SceneDeclStmt extends Statement {
    type: "SceneDecl";
    name: string;
    body: Statement[];
}

export interface SayStmt extends Statement {
    type: "SayStmt";
    text: string;
    who: Identifier | null;
}

export interface ThinkStmt extends Statement {
    type: "ThinkStmt";
    text: string;
    character: Identifier;
}

export interface OptionChoice {
    text: Expression; // Changed from string to Expression
    condition?: Expression; // New: optional condition for the choice
    body: Statement[];
}

export interface OptionsBlockStmt extends Statement {
    type: "OptionsBlockStmt";
    choices: OptionChoice[];
}

export interface ImportStmt extends Statement {
    type: "ImportStmt";
    path: string; // Added path property
    alias?: string; // Added alias property
    body: Statement[];
    os: boolean
}

export interface GotoStmt extends Statement {
    type: "GotoStmt";
    sceneName: string;
}

export interface IfStmt extends Statement {
    type: "IfStmt";
    condition: Expression;
    thenBlock: Statement[];
    elseBlock: Statement[] | null;
    elseIf: { condition: Expression, body: Statement[] }[] | null;
}

export interface PauseStmt extends Statement {
    type: "PauseStmt";
}

export interface ExitStmt extends Statement {
    type: "ExitStmt";
}

// --- Exceptions ---

export class ParselError extends Error {
    line: number;
    column: number;
    file: string;
    constructor(token: Token | null | undefined, userMessage?: string) {
        const file = token?.file || "unknown_file";
        const line = token?.line || 0;
        const column = token?.column || 0;
        const tokenValue = token?.value !== undefined ? token.value : 'N/A';

        const message = `${file}:${line}:${column} ${userMessage || 'unknown error at token: ' + tokenValue}`;
        super(message);
        this.line = line;
        this.column = column;
        this.file = file;
    }
}

// --- ParselParser Class ---

export class ParselParser {
    private tokens: Token[];
    private current: number;
    private file: string;
    private source: string;
    private importedFiles: { [key: string]: ImportStmt } = {};
    private keywords: string[] = [
        "var", "func", "return", "def", "as", "end", // Existing subset
        // New Ren'Py
        "char", "scene", "say", "think", "options", "begin", "goto", "set", "to", "in",
        "if", "else", "elseif", "pause", "exit",
        "import", "using"
    ];

    constructor(filePath: string) {
        this.source = fs.readFileSync(filePath, "utf8");
        this.file = filePath;
        this.tokens = this.tokenize(this.source, this.file);
        this.current = 0;
    }

    private tokenize(source: string, file: string): Token[] {
        const tokens: Token[] = [];
        let i = 0;
        let line = 1;
        let col = 1;
        const length = source.length;

        while (i < length) {
            let char = source[i];
            const startCol = col;
            const nextChar = source[i + 1];

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

            if (char === "/" && nextChar === "/") {
                while (i < length && source[i] !== "\n") {
                    i++;
                }
                continue;
            }
            if (char === "/" && nextChar === "*") {
                i += 2;
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
                    i += 2;
                    col += 2;
                } else {
                    throw new ParselError({ type: "error", value: "Unterminated comment", file: file, line: line, column: col }, "Unterminated multi-line comment");
                }
                continue;
            }

            let matchedOperator = false;
            const twoCharOps = ["+=", "-=", "*=", "/=", "%=", "==", "!=", "<=", ">=", "&&", "||"];
            const currentTwoChar = char + (nextChar || '');

            if (i + 1 < length && twoCharOps.includes(currentTwoChar)) {
                tokens.push({ type: "operator", value: currentTwoChar, line, column: startCol, file });
                i += 2;
                col += 2;
                matchedOperator = true;
            } else {
                const potentialSingleOps = "=+-*/%<>!.";
                if (potentialSingleOps.includes(char)) {
                    tokens.push({ type: "operator", value: char, line, column: startCol, file });
                    i++;
                    col++;
                    matchedOperator = true;
                } else {
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
                        col = 1;
                        line++;
                    }
                }
                if (i < length && source[i] === '"') {
                    i++;
                    col++;
                } else {
                    throw new ParselError({ type: "error", value: "Unterminated string", file: file, line: line, column: col }, "Unterminated string literal");
                }
                tokens.push({ type: "string", value: str, line, column: startCol, file: file });
                continue;
            }

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

            throw new ParselError({ type: "error", value: char, file: file, line: line, column: col }, `Unexpected character: ${char}`);
        }
        return tokens;
    }

    private getNextToken(): Token | null {
        return this.tokens[this.current] || null;
    }

    private consumeToken(): Token {
        return this.tokens[this.current++];
    }

    private expectType(type: string): Token {
        const token = this.getNextToken();
        if (!token || token.type !== type) {
            throw new ParselError(token, `Expected token type ${type}, got ${token ? ([JSON.stringify(token, null, 1), token.type]) : "EOF"}`);
        }
        return token;
    }

    private expectToken(value: string): Token {
        const token = this.getNextToken();
        if (!token) {
            const lastToken = this.tokens[this.current - 1] || { type: "EOF", value: "EOF", file: this.file, line: 1, column: 1 };
            throw new ParselError(lastToken, `Expected token '${value}', got EOF`);
        }
        if (token.value !== value) {
            throw new ParselError(token, `Expected token '${value}', got ${token.value}`);
        }
        return token;
    }

    private parseBlockUntil(terminators: string[] = []): Statement[] {
        const statements: Statement[] = [];
        while (this.current < this.tokens.length) {
            const token = this.getNextToken();
            if (!token) {
                const lastToken = this.tokens[this.current - 1] || { type: "EOF", value: "EOF", file: this.file, line: 1, column: 1 };
                throw new ParselError(lastToken, "Unexpected end of input");
            }
            if (
                (token.type === "keyword" && terminators.includes(token.value)) ||
                (token.type === "operator" && terminators.includes(token.value))
            ) {
                break;
            }
            const stmt = this.parseStatement();
            if (stmt) {
                statements.push(stmt);
            }
        }
        return statements;
    }

    // Main parsing entry point for the script
    public parse(): Statement[] {
        const statements: Statement[] = [];
        while (this.current < this.tokens.length) {
            const stmt = this.parseStatement();
            if (stmt) {
                statements.push(stmt);
            }
        }
        return statements;
    }

    private parseStatement(): Statement | undefined {
        const token = this.getNextToken();
        if (!token) {
            const lastToken = this.tokens[this.current - 1] || { type: "EOF", value: "EOF", file: this.file, line: 1, column: 1 };
            throw new ParselError(lastToken, "Unexpected end of input");
        }

        switch (token.value) {
            case "char": return this.parseCharDeclaration();
            case "scene": return this.parseSceneDeclaration();
            case "say": return this.parseSayStatement();
            case "think": return this.parseThinkStatement();
            case "options": return this.parseOptionsBlock();
            case "goto": return this.parseGotoStatement();
            case "set": return this.parseSetStatement();
            case "if": return this.parseIfStatement();
            case "pause": {
                this.consumeToken();
                return { type: "PauseStmt", file: token.file, line: token.line, column: token.column };
            }
            case "exit": {
                this.consumeToken();
                return { type: "ExitStmt", file: token.file, line: token.line, column: token.column };
            }
            case "var": return this.parseVarDeclaration();
            case "func": return this.parseFuncDeclaration();
            case "def": return this.parseLambdaDeclarationAsStatement();
            case "return": return this.parseReturnStatement();
            case "import": return this.parseImportStatement();
            case "using": return this.parseUsingStatement();
            default:
                // Fallback to expression statement
                const expr = this.parseExpression();
                return { type: "ExpressionStmt", expression: expr, file: token.file, line: token.line, column: token.column };
        }
    }

    // --- Ren'Py Specific Parsers ---

    parseUsingStatement(): UsingStmt {
        const usingToken = this.consumeToken(); // consume 'using'
        const what = this.expectType("identifier");
        this.consumeToken(); // consume identifier
        return { type: "UsingStmt", name: what.value, file: usingToken.file, line: usingToken.line, column: usingToken.column };
    }

    parseImportStatement(): ImportStmt {
        const imp = this.consumeToken(); // consume 'import'
        const pathToken = this.expectType("string");
        const importPath = pathToken.value;
        this.consumeToken(); // consume string

        let alias: string | undefined;
        if (this.getNextToken()?.value === "as") {
            this.consumeToken(); // consume 'as'
            const aliasToken = this.expectType("identifier");
            alias = aliasToken.value;
            this.consumeToken(); // consume the identifier for the alias
        }

        if (importPath.startsWith("os:")) {
            const pa = importPath.substring(3);
            const newImportStmt: ImportStmt = { type: "ImportStmt", path: pa, alias, body: [], os: true, file: imp.file, line: imp.line, column: imp.column };
            this.importedFiles[importPath] = newImportStmt; // Store with original path for os imports
            return newImportStmt;
        }

        const fullPath = path.resolve(path.dirname(this.file), importPath + ".par");
        if (fullPath in this.importedFiles) return this.importedFiles[fullPath];

        const parser = new ParselParser(fullPath);
        parser.importedFiles = this.importedFiles; // Share importedFiles cache
        const body = parser.parse();

        const newImportStmt: ImportStmt = { type: "ImportStmt", os: false, path: fullPath, alias, body, file: imp.file, line: imp.line, column: imp.column };
        this.importedFiles[fullPath] = newImportStmt; // Store with full path for file imports
        return newImportStmt;
    }

    private parseCharDeclaration(): CharDeclStmt {
        const charToken = this.consumeToken(); // consume 'char'
        const nameToken = this.expectType("identifier");
        const name = nameToken.value;
        this.consumeToken(); // consume identifier

        // Now parse an expression for the display name
        const displayNameExpr = this.parseExpression();

        return { type: "CharDecl", name, displayName: displayNameExpr, file: charToken.file, line: charToken.line, column: charToken.column };
    }

    private parseSceneDeclaration(): SceneDeclStmt {
        const sceneToken = this.consumeToken(); // consume 'scene'
        const nameToken = this.expectType("string");
        const name = nameToken.value;
        this.consumeToken(); // consume scene name string

        const body = this.parseBlockUntil(["end"]);
        this.expectToken("end");
        this.consumeToken(); // consume 'end'

        return { type: "SceneDecl", name, body, file: sceneToken.file, line: sceneToken.line, column: sceneToken.column };
    }

    private parseSayStatement(): SayStmt {
        const sayToken = this.consumeToken(); // consume 'say'
        const textToken = this.expectType("string"); // Say statement text is still a string literal
        const text = textToken.value;
        this.consumeToken(); // consume string
        let who: Identifier | null = null;
        if (this.getNextToken() && this.getNextToken()!.type === "keyword" && this.getNextToken()!.value === "as") {
            this.consumeToken(); // consume 'as'
            let whoTok = this.expectType("identifier");
            this.consumeToken(); // consume character identifier

            who = {
                type: "Identifier",
                name: whoTok.value,
                file: whoTok.file, line: whoTok.line, column: whoTok.column
            }
        }

        return { type: "SayStmt", text, who, file: sayToken.file, line: sayToken.line, column: sayToken.column };
    }

    private parseThinkStatement(): ThinkStmt {
        const thinkToken = this.consumeToken(); // consume 'think'
        const textToken = this.expectType("string");
        const text = textToken.value;
        this.consumeToken(); // consume string

        this.expectToken("as");
        this.consumeToken(); // consume 'as'

        const charToken = this.expectType("identifier");
        const character: Identifier = {
            type: "Identifier",
            name: charToken.value,
            file: charToken.file, line: charToken.line, column: charToken.column
        };
        this.consumeToken(); // consume character identifier

        return { type: "ThinkStmt", text, character, file: thinkToken.file, line: thinkToken.line, column: thinkToken.column };
    }

    private parseOptionsBlock(): OptionsBlockStmt {
        const optionsToken = this.consumeToken(); // consume 'options'

        const choices: OptionChoice[] = [];
        // Loop as long as we can parse an expression for the choice text
        while (this.current < this.tokens.length && this.getNextToken() && this.isExpressionStart(this.getNextToken()!)) {
            const textExpr = this.parseExpression(); // Parse expression for choice text

            let condition: Expression | undefined;
            if (this.getNextToken()?.value === "if") {
                this.consumeToken(); // consume 'if'
                condition = this.parseExpression(); // Parse condition expression
            }

            this.expectToken("begin");
            this.consumeToken(); // consume 'begin'

            const body = this.parseBlockUntil(["end"]);
            this.expectToken("end");
            this.consumeToken(); // consume 'end'

            choices.push({ text: textExpr, condition, body });
        }

        // The outer 'options' block also has an 'end'
        this.expectToken("end");
        this.consumeToken(); // consume outer 'end'

        return { type: "OptionsBlockStmt", choices, file: optionsToken.file, line: optionsToken.line, column: optionsToken.column };
    }

    private isExpressionStart(token: Token): boolean {
        // Helper to determine if a token can start an expression for options block
        return token.type === "number" ||
               token.type === "string" ||
               token.type === "boolean" ||
               token.type === "identifier" ||
               token.value === "(" || // Parenthesized expression
               token.value === "[" || // Array literal
               token.value === "{" || // Object literal
               token.value === "-" || // Unary minus
               token.value === "!" || // Unary not
               token.value === "def"; // Lambda declaration
    }


    private parseGotoStatement(): GotoStmt {
        const gotoToken = this.consumeToken(); // consume 'goto'
        const sceneNameToken = this.expectType("string");
        const sceneName = sceneNameToken.value;
        this.consumeToken(); // consume scene name string

        return { type: "GotoStmt", sceneName, file: gotoToken.file, line: gotoToken.line, column: gotoToken.column };
    }

    private parseSetStatement(): AssignmentExpr {
        const setToken = this.consumeToken(); // consume 'set'

        let target: Expression;
        const firstIdent = this.expectType("identifier");
        this.consumeToken(); // consume first identifier

        if (this.getNextToken()?.value === "in") {
            this.consumeToken(); // consume 'in'
            const objectIdent = this.expectType("identifier"); // Expect the object name
            this.consumeToken(); // consume object name
            // Construct PropertyAccess: object.property
            target = {
                type: "PropertyAccess",
                object: { type: "Identifier", name: objectIdent.value, file: objectIdent.file, line: objectIdent.line, column: objectIdent.column },
                property: firstIdent.value, // The first identifier is the property name
                file: firstIdent.file, line: firstIdent.line, column: firstIdent.column
            };
        } else {
            // Simple identifier assignment: set a to ...
            target = { type: "Identifier", name: firstIdent.value, file: firstIdent.file, line: firstIdent.line, column: firstIdent.column };
        }

        this.expectToken("to");
        this.consumeToken(); // consume 'to'

        const value = this.parseExpression();

        return {
            type: "AssignmentExpr",
            target: target,
            value: value,
            operator: "=", // Always simple assignment for 'set'
            file: setToken.file, line: setToken.line, column: setToken.column
        };
    }

    // --- Existing Parsers (adapted) ---

    private parseIfStatement(): IfStmt {
        const ifToken = this.consumeToken(); // consume 'if'
        const condition = this.parseExpression();

        const thenBlock = this.parseBlockUntil(["else", "elseif", "end"]);

        const elseIfBlocks: { condition: Expression, body: Statement[] }[] = [];
        let elseBlock: Statement[] | null = null;

        while (this.getNextToken()?.type === "keyword" && this.getNextToken()!.value === "elseif") {
            this.consumeToken(); // consume 'elseif'
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
            condition,
            thenBlock,
            elseBlock,
            elseIf: elseIfBlocks.length > 0 ? elseIfBlocks : null,
            file: ifToken.file, line: ifToken.line, column: ifToken.column
        };
    }

    private parseVarDeclaration(): VarDeclStmt {
        const varToken = this.consumeToken(); // consume 'var'
        const nameToken = this.expectType("identifier");
        const name = nameToken.value;
        this.consumeToken();

        let annotationType: string | null = null;
        if (
            this.getNextToken() &&
            this.getNextToken()!.type === "identifier" &&
            ["string", "number", "boolean", "void", "bool"].includes(this.getNextToken()!.value)
        ) {
            annotationType = this.getNextToken()!.value;
            this.consumeToken();
        }

        this.expectToken("=");
        this.consumeToken();
        const initializer = this.parseExpression();
        return { type: "VarDecl", name, typeAnnotation: annotationType, initializer, line: varToken.line, column: varToken.column, file: varToken.file };
    }

    private parseFuncDeclaration(): FuncDecl {
        const funcToken = this.consumeToken(); // consume 'func'
        const nameToken = this.expectType("identifier");
        const name = nameToken.value;
        this.consumeToken();
        this.expectToken("(");
        this.consumeToken();

        const parameters: Parameter[] = [];
        if (this.getNextToken() && this.getNextToken()!.value !== ")") {
            while (true) {
                const paramToken = this.expectType("identifier");
                const paramName = paramToken.value;
                this.consumeToken();

                let annotationType: string | null = null;
                let defaultExpr: Expression | undefined;

                if (this.getNextToken()?.type === "identifier") {
                    const typeToken = this.getNextToken()!;
                    if (["string", "number", "bool", "boolean"].includes(typeToken.value)) {
                        annotationType = typeToken.value;
                        this.consumeToken();
                    }
                }

                if (this.getNextToken()?.value === "=") {
                    this.consumeToken();
                    defaultExpr = this.parseExpression();
                    if (annotationType) {
                        throw new ParselError(funcToken, "Cannot have both explicit type annotation and a default value. Consider removing the type annotation to allow type inference from the default value.");
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

                parameters.push({
                    name: paramName,
                    annotationType: annotationType,
                    default: defaultExpr,
                    file: paramToken.file,
                    line: paramToken.line,
                    column: paramToken.column,
                    type: paramToken.type,
                    value: paramToken.value
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
            file: funcToken.file, line: funcToken.line, column: funcToken.column
        };
    }

    private parseLambdaDeclaration(): LambdaDecl {
        const defToken = this.consumeToken(); // consume 'def'
        this.expectToken("(");
        this.consumeToken();

        const parameters: Parameter[] = [];
        if (this.getNextToken() && this.getNextToken()!.value !== ")") {
            while (true) {
                const paramToken = this.expectType("identifier");
                const paramName = paramToken.value;
                this.consumeToken();

                let annotationType: string | null = null;
                let defaultExpr: Expression | undefined;

                if (this.getNextToken()?.type === "identifier") {
                    const typeToken = this.getNextToken()!;
                    if (["string", "number", "bool", "boolean"].includes(typeToken.value)) {
                        annotationType = typeToken.value;
                        this.consumeToken();
                    }
                }

                if (this.getNextToken()?.value === "=") {
                    this.consumeToken();
                    defaultExpr = this.parseExpression();
                    if (annotationType) {
                        throw new ParselError(defToken, "Cannot have both explicit type annotation and a default value. Consider removing the type annotation to allow type inference from the default value.");
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

                parameters.push({
                    name: paramName,
                    annotationType: annotationType,
                    default: defaultExpr,
                    file: paramToken.file,
                    line: paramToken.line,
                    column: paramToken.column,
                    type: paramToken.type,
                    value: paramToken.value
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
            file: defToken.file, line: defToken.line, column: defToken.column
        };
    }

    // This is a wrapper to allow 'def' to be caught by parseStatement
    private parseLambdaDeclarationAsStatement(): ExpressionStmt {
        const lambdaExpr = this.parseLambdaDeclaration();
        return { type: "ExpressionStmt", expression: lambdaExpr, file: lambdaExpr.file, line: lambdaExpr.line, column: lambdaExpr.column };
    }


    private parseReturnStatement(): ReturnStmt {
        const returnToken = this.consumeToken(); // consume 'return'
        let expression: Expression | null = null;
        // In Ren'Py context, 'return' typically doesn't return a value to the caller,
        // but rather controls flow. Keeping expression optional for consistency.
        if (this.getNextToken() && !this.keywords.includes(this.getNextToken()!.value)) {
            expression = this.parseExpression();
        }
        return { type: "ReturnStmt", expression, file: returnToken.file, line: returnToken.line, column: returnToken.column };
    }

    // --- Expression Parsing (Recursive Descent) ---
    private parseExpression(): Expression {
        return this.parseAssignment();
    }

    private parseAssignment(): Expression {
        let expr = this.parseLogicalOr();
        const nextToken = this.getNextToken();

        if (
            nextToken &&
            nextToken.type === "operator" &&
            ["=", "+=", "-=", "*=", "/=", "%="].includes(nextToken.value)
        ) {
            const assignmentOpToken = this.consumeToken();
            const valueExpr = this.parseAssignment();

            // Corrected: ArrayLiteral is not a valid assignment target.
            if (expr.type !== "Identifier" && expr.type !== "PropertyAccess" && expr.type !== "ArrayAccess") {
                throw new ParselError(expr, `Invalid assignment target: Cannot assign to ${expr.type}`);
            }

            return {
                type: "AssignmentExpr",
                target: expr,
                value: valueExpr,
                operator: assignmentOpToken.value,
                file: assignmentOpToken.file,
                line: assignmentOpToken.line,
                column: assignmentOpToken.column
            };
        }
        return expr;
    }

    private parseLogicalOr(): Expression {
        let expr = this.parseLogicalAnd();
        while (
            this.getNextToken() &&
            this.getNextToken()!.type === "operator" &&
            this.getNextToken()!.value === "||"
        ) {
            const operatorToken = this.consumeToken();
            const operator = operatorToken.value;
            const right = this.parseLogicalAnd();
            expr = { type: "BinaryExpr", operator, left: expr, right, file: operatorToken.file, line: operatorToken.line, column: operatorToken.column };
        }
        return expr;
    }

    private parseLogicalAnd(): Expression {
        let expr = this.parseEquality();
        while (
            this.getNextToken() &&
            this.getNextToken()!.type === "operator" &&
            this.getNextToken()!.value === "&&"
        ) {
            const operatorToken = this.consumeToken();
            const operator = operatorToken.value;
            const right = this.parseEquality();
            expr = { type: "BinaryExpr", operator, left: expr, right, file: operatorToken.file, line: operatorToken.line, column: operatorToken.column };
        }
        return expr;
    }

    private parseEquality(): Expression {
        let expr = this.parseComparison();
        while (
            this.getNextToken() &&
            this.getNextToken()!.type === "operator" &&
            (this.getNextToken()!.value === "==" || this.getNextToken()!.value === "!=")
        ) {
            const operatorToken = this.consumeToken();
            const operator = operatorToken.value;
            const right = this.parseComparison();
            expr = { type: "BinaryExpr", operator, left: expr, right, file: operatorToken.file, line: operatorToken.line, column: operatorToken.column };
        }
        return expr;
    }

    private parseComparison(): Expression {
        let expr = this.parseTerm();
        while (
            this.getNextToken() &&
            this.getNextToken()!.type === "operator" &&
            ["<", "<=", ">", ">="].includes(this.getNextToken()!.value)
        ) {
            const operatorToken = this.consumeToken();
            const operator = operatorToken.value;
            const right = this.parseTerm();
            expr = { type: "BinaryExpr", operator, left: expr, right, file: operatorToken.file, line: operatorToken.line, column: operatorToken.column };
        }
        return expr;
    }

    private parseTerm(): Expression {
        let expr = this.parseFactor();
        while (
            this.getNextToken() &&
            this.getNextToken()!.type === "operator" &&
            (this.getNextToken()!.value === "+" || this.getNextToken()!.value === "-")
        ) {
            const operatorToken = this.consumeToken();
            const operator = operatorToken.value;
            const right = this.parseFactor();
            expr = { type: "BinaryExpr", operator, left: expr, right, file: operatorToken.file, line: operatorToken.line, column: operatorToken.column };
        }
        return expr;
    }

    private parseFactor(): Expression {
        let expr = this.parseUnary();
        while (
            this.getNextToken() &&
            this.getNextToken()!.type === "operator" &&
            (this.getNextToken()!.value === "*" || this.getNextToken()!.value === "/" || this.getNextToken()!.value === "%")
        ) {
            const operatorToken = this.consumeToken();
            const operator = operatorToken.value;
            const right = this.parseUnary();
            expr = { type: "BinaryExpr", operator, left: expr, right, file: operatorToken.file, line: operatorToken.line, column: operatorToken.column };
        }
        return expr;
    }

    private parseUnary(): Expression {
        if (
            this.getNextToken() &&
            this.getNextToken()!.type === "operator" &&
            (this.getNextToken()!.value === "-" || this.getNextToken()!.value === "!")
        ) {
            const operatorToken = this.consumeToken();
            const operator = operatorToken.value;
            const right = this.parseUnary();
            return { type: "UnaryExpr", operator, right, file: operatorToken.file, line: operatorToken.line, column: operatorToken.column };
        }
        return this.parseCallMemberExpression();
    }

    private parseCallMemberExpression(): Expression {
        let expr = this.parsePrimary();

        while (true) {
            const nextToken = this.getNextToken();
            if (!nextToken) break;

            if (nextToken.value === ".") {
                this.consumeToken(); // Consume the '.' token
                const propToken = this.expectType("identifier");
                const propName = propToken.value;
                this.consumeToken(); // Consume the identifier (property name)

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
                        object: expr,
                        method: propName,
                        arguments: args,
                        file: propToken.file, line: propToken.line, column: propToken.column
                    };
                } else {
                    expr = {
                        type: "PropertyAccess",
                        object: expr,
                        property: propName,
                        file: propToken.file, line: propToken.line, column: propToken.column
                    };
                }
            } else if (nextToken.value === "[") {
                const bracketToken = this.consumeToken();
                const indexExpr = this.parseExpression();
                this.expectToken("]");
                this.consumeToken();
                expr = {
                    type: "ArrayAccess",
                    object: expr,
                    index: indexExpr,
                    file: bracketToken.file, line: bracketToken.line, column: bracketToken.column
                };
            } else if (nextToken.value === "(" && expr.type === "Identifier") {
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
                expr = {
                    type: "FuncCall",
                    name: (expr as Identifier).name,
                    arguments: args,
                    file: expr.file, line: expr.line, column: expr.column // Corrected line: expr.file to expr.line
                };
            }
            else {
                break;
            }
        }
        return expr;
    }

    private parsePrimary(): Expression {
        let node: Expression;
        const token = this.getNextToken();
        if (!token) {
            throw new ParselError(token, "Unexpected end of input");
        }

        if (token.type === "boolean") {
            this.consumeToken();
            node = { type: "Literal", value: token.value, file: token.file, line: token.line, column: token.column };
        }
        else if (token.type === "number" || token.type === "string") {
            this.consumeToken();
            node = { type: "Literal", value: token.value, file: token.file, line: token.line, column: token.column };
        }
        else if (token.value === "[") {
            this.consumeToken();
            const elements: Expression[] = [];
            if (this.getNextToken() && this.getNextToken()!.value !== "]") {
                while (true) {
                    elements.push(this.parseExpression());
                    if (this.getNextToken() && this.getNextToken()!.value === ",") {
                        this.consumeToken();
                    } else {
                        break;
                    }
                }
            }
            this.expectToken("]");
            this.consumeToken();
            node = { type: "ArrayLiteral", elements, file: token.file, line: token.line, column: token.column };
        }
        else if (token.value === "{") {
            this.consumeToken();
            const properties: { key: string; value: Expression }[] = [];
            if (this.getNextToken() && this.getNextToken()!.value !== "}") {
                while (true) {
                    let keyToken = this.getNextToken()!;
                    if (keyToken.type !== "identifier" && keyToken.type !== "string") {
                        throw new ParselError(keyToken, "Expected identifier or string as object key");
                    }
                    const key = keyToken.value;
                    this.consumeToken();
                    this.expectToken(":");
                    this.consumeToken();
                    const value = this.parseExpression();
                    properties.push({ key, value });
                    if (this.getNextToken() && this.getNextToken()!.value === ",") {
                        this.consumeToken();
                    } else {
                        break;
                    }
                }
            }
            this.expectToken("}");
            this.consumeToken();
            node = { type: "ObjectLiteral", properties, file: token.file, line: token.line, column: token.column };
        }
        else if (token.type === "identifier") {
            this.consumeToken();
            node = { type: "Identifier", name: token.value, file: token.file, line: token.line, column: token.column };
        }
        else if (token.value == "def") {
            node = this.parseLambdaDeclaration();
        }
        else if (token.value === "(") {
            this.consumeToken();
            node = this.parseExpression();
            this.expectToken(")");
            this.consumeToken();
        }
        else {
            throw new ParselError(token, `Unexpected token: ${token.value}`);
        }

        return node;
    }
}
//import readline from "readline-sync";
import { input } from "./sync-input/index.ts"

/* A special exception used to implement returning values from functions */
export class ReturnException extends Error {
    value: any;
    constructor(value: any) {
        super("Return");
        this.value = value;
    }
}

/** Simple environment with lexical scoping + deferred statements */
export class Environment {
    values: Record<string, any> = {};
    parent: Environment | null;
    constructor(parent: Environment | null = null) {
        this.parent = parent;
    }
    define(name: string, value: any) {
        this.values[name] = value;
    }
    has(name: string): boolean {
        return name in this.values || (!!this.parent && this.parent.has(name));
    }
    assign(name: string, value: any, tok: Token) {
        if (name in this.values) {
            this.values[name] = value;
        } else if (this.parent) {
            this.parent.assign(name, value, tok);
        } else {
            throw new ParselError(tok, `Undefined variable ${name}`);
        }
    }
    get(name: string, tok?: Token): any {
        if (name in this.values) return this.values[name];
        if (this.parent) return this.parent.get(name, tok);
        if (tok) throw new ParselError(tok, `Undefined variable ${name}`);
        throw new Error(`Undefined variable ${name}`);
    }
}

function showChoice(choices: string[]): number {
    console.log("Choose an option:");
    for (let i = 0; i < choices.length; i++) {
        console.log(`${i + 1}) ${choices[i]}`);
    }

    while (true) {
        try {
            const raw = input("Choice: ");
            if (raw === null) {
                console.log("EOF received. Exiting game.");
                process.exit(0);
            }

            const index = parseInt(raw) - 1;
            if (!Number.isNaN(index) && index >= 0 && index < choices.length) {
                return index;
            } else {
                console.log(`Invalid choice: "${raw}"`);
            }
        } catch (e) {
            console.log("Input cancelled. Exiting.");
            process.exit(0);
        }
    }
}
export class ParselRuntime {
    context = new Environment();
    scenes: Record<string, Statement[]> = {};
    currentScene = "start";
    sceneStack: string[] = [];

    constructor(private ast: Statement[]) {
        initGlobals(this)
        // collect scene bodies
        for (const stmt of ast) {
            if (stmt.type === "SceneDecl") {
                const sd = stmt as SceneDeclStmt;
                this.scenes[sd.name] = sd.body;
            }
        }
    }

    /** Entry point: run all top‑level code (var/char defs, etc.) */
    parse() {
        for (const stmt of this.ast) {
            if (stmt.type !== "SceneDecl") {
                this.executeStatement(stmt, this.context);
            }
        }
    }

    /** Main loop */
    play() {
        while (true) {
            if (!this.scenes[this.currentScene]) {
                throw new Error(`Scene "${this.currentScene}" not found`);
            }
            try {
                this.runScene(this.currentScene);
            } catch (e) {
                if (e instanceof ReturnException) {
                    // returning from scene -> pop back?
                    this.currentScene = this.sceneStack.pop() || "start";
                } else {
                    throw e;
                }
            }
        }
    }

    private runScene(name: string) {
        const ctx = new Environment(this.context);
        for (const stmt of this.scenes[name]) {
            this.executeStatement(stmt, ctx);
        }
    }

    checkType(expected: string, value: any, token: Token) {
        let actualType = expected
        if (expected === "bool") actualType = "boolean"

        switch (actualType) {
            case "number":
                if (typeof value !== "number") throw new ParselError(token, `Type mismatch: expected number, got ${typeof value}`);
                break;
            case "string":
                if (typeof value !== "string") throw new ParselError(token, `Type mismatch: expected string, got ${typeof value}`);
                break;
            case "boolean":
                if (typeof value !== "boolean") throw new ParselError(token, `Type mismatch: expected boolean, got ${typeof value}`);
                break;
            case "function":
                if (typeof value !== "function") throw new ParselError(token, `Type mismatch: expected function, got ${typeof value}`);
                break;
            case "void":
                // Changed to ParselError
                if (value !== undefined) throw new ParselError(token, `Type mismatch: expected void, got ${typeof value}`);
                break;
            default: // custom types might be allowed in the future, but not now, we need to get this dam language going
                throw new Error(`Unknown type: ${actualType}`);
        }

    }

    createFunc(s: FuncDecl | LambdaDecl, ctx: Environment) {
        const func = (...args: Parameter[]) => {
            const funcEnv = new Environment(ctx);

            for (let i = 0; i < s.parameters.length; i++) {
                const param = s.parameters[i];
                let argVal = args[i]
                if (argVal === undefined && param.default !== undefined) {
                    argVal = this.evaluateExpression(param.default, ctx);
                } else if (argVal === undefined && param.default === undefined) {
                    throw new ParselError(null, `Missing required parameter ${param.name} in function ${s.type === "FuncDecl" ? (s as FuncDecl).name : "lambda"}`); // Added name for FuncDecl
                }

                if (param.annotationType) {
                    this.checkType(param.annotationType, argVal, param);
                }

                funcEnv.define(param.name, argVal)
            }

            try {
                s.body.forEach(e => this.executeStatement(e, funcEnv));
            } catch (e) {
                if (e instanceof ReturnException) {
                    return e.value;
                } else {
                    throw e;
                }
            }
            return undefined
        }

        return func
    }

    /** Statement dispatcher */
    private executeStatement(stmt: Statement, ctx: Environment) {
        switch (stmt.type) {
            case "VarDecl": {
                const s = stmt as VarDeclStmt;
                ctx.define(s.name, this.evaluateExpression(s.initializer, ctx));
                break;
            }
            case "FuncDecl": {
                const s = stmt as FuncDecl;
                const func = this.createFunc(s, ctx);
                ctx.define(s.name, func);
                break
            }

            case "UsingStmt": {
                const s = stmt as UsingStmt
                const space = ctx.get(s.name, s)
                if (space instanceof Environment) {
                    for (const [key, value] of Object.entries(space.values)) {
                        ctx.define(key, value)
                    }
                } else if (typeof space === "object" && space !== null) {
                    for (const [key, value] of Object.entries(space)) {
                        ctx.define(key, value)
                    }
                } else {
                    throw new ParselError(s, `Cannot 'use' non-namespace value: ${s.name}`)
                }
                break; // Added break
            }

            case "CharDecl": {
                const s = stmt as CharDeclStmt;
                // Evaluate the expression for displayName
                const evaluatedDisplayName = this.evaluateExpression(s.displayName, ctx);
                ctx.define(s.name, evaluatedDisplayName);
                break;
            }
            case "ExpressionStmt": {
                this.evaluateExpression((stmt as ExpressionStmt).expression, ctx);
                break;
            }
            case "SayStmt": {
                const s = stmt as SayStmt;
                let who = s.who ? this.evaluateExpression(s.who, ctx) + ": " : "";
                // SayStmt text is still a string literal, so interpolate it.
                // If it were an expression, it would be evaluated.
                let text = this.interpolate(s.text, ctx);
                console.log(`${who}"${text}"`);
                break;
            }
            case "ThinkStmt": {
                const s = stmt as ThinkStmt;
                const who = this.evaluateExpression(s.character, ctx);
                const text = this.interpolate(s.text, ctx);
                console.log(`${who}: *${text}*`);
                break;
            }
            case "OptionsBlockStmt": {
                const ob = stmt as OptionsBlockStmt;
                const availableChoices: { originalIndex: number; displayText: string; body: Statement[] }[] = [];

                for (let i = 0; i < ob.choices.length; i++) {
                    const choice = ob.choices[i];
                    let includeChoice = true;
                    if (choice.condition) {
                        includeChoice = !!this.evaluateExpression(choice.condition, ctx);
                    }

                    if (includeChoice) {
                        // Evaluate the text expression for display
                        const displayText = String(this.evaluateExpression(choice.text, ctx));
                        availableChoices.push({ originalIndex: i, displayText, body: choice.body });
                    }
                }

                if (availableChoices.length === 0) {
                    console.log("No options available.");
                    return;
                }

                const choicesToDisplay = availableChoices.map(c => c.displayText);
                const selectedDisplayIndex = showChoice(choicesToDisplay);
                const originalSelectedChoice = availableChoices[selectedDisplayIndex];

                for (const child of originalSelectedChoice.body) {
                    try {
                        this.executeStatement(child, ctx);
                    } catch (e) {
                        if (e instanceof ReturnException) break;
                        throw e;
                    }
                }
                break;
            }
            case "IfStmt": {
                const s = stmt as IfStmt;
                if (this.evaluateExpression(s.condition, ctx)) {
                    s.thenBlock.forEach(st => this.executeStatement(st, ctx));
                } else {
                    let matched = false;
                    if (s.elseIf) {
                        for (const ei of s.elseIf) {
                            if (this.evaluateExpression(ei.condition, ctx)) {
                                ei.body.forEach(st => this.executeStatement(st, ctx));
                                matched = true;
                                break;
                            }
                        }
                    }
                    if (!matched && s.elseBlock) {
                        s.elseBlock.forEach(st => this.executeStatement(st, ctx));
                    }
                }
                break;
            }
            case "GotoStmt": {
                const s = stmt as GotoStmt;
                if (!this.scenes[s.sceneName]) {
                    throw new ParselError(s, `Scene ${s.sceneName} not defined`);
                }
                this.sceneStack.push(this.currentScene);
                this.currentScene = s.sceneName;
                throw new ReturnException(undefined)
            }
            case "ReturnStmt":
                const value = (stmt as ReturnStmt).expression ? this.evaluateExpression((stmt as ReturnStmt).expression!, ctx) : undefined;
                throw new ReturnException(value);

            case "PauseStmt":
                input("(Press enter to continue)");
                break;
            case "ImportStmt":
                {
                    const s = stmt as ImportStmt;
                    if (s.os) {
                        if (!this.context.has("os-import-handler")) {
                            throw new ParselError(s, "no 'os-import-handler' defined in runtime")
                        }
                        const res = this.context.get("os-import-handler", s)(s.path)
                        let name = s.alias || s.path;

                        ctx.define(name, res)
                        break
                    }
                    const interp = new ParselRuntime(s.body)
                    const env = new Environment(this.context);
                    interp.context = env
                    interp.parse()
                    const namespace: Record<string, any> = {}

                    for (const key in env.values) {
                        namespace[key] = env.values[key]
                    }

                    ctx.define(s.alias || path.basename(s.path, '.par'), namespace) // Use alias or filename as default
                    break
                }
            case "ExitStmt":
                process.exit(0);
            default:
                // For any expression‑like nodes used alone
                if ((stmt as any).expression) {
                    this.evaluateExpression((stmt as any).expression, ctx);
                } else {
                    throw new ParselError(stmt, `Unsupported stmt: ${stmt.type}`);
                }
        }
    }

    /** Expression dispatcher */
    private evaluateExpression(expr: Statement, ctx: Environment): any {
        switch (expr.type) {
            case "Literal":
                return (expr as Literal).value;
            case "Identifier":
                return ctx.get((expr as Identifier).name, expr);
            case "BinaryExpr": {
                const b = expr as BinaryExpr;
                const l = this.evaluateExpression(b.left, ctx);
                const r = this.evaluateExpression(b.right, ctx);
                return this.evalBinary(b.operator, l, r, b);
            }
            case "UnaryExpr": {
                const u = expr as UnaryExpr;
                const val = this.evaluateExpression(u.right, ctx);
                if (u.operator === "-") {
                    if (typeof val !== "number") throw new ParselError(u, "Unary - only on numbers");
                    return -val;
                }
                if (u.operator === "!") return !val;
                throw new ParselError(u, `Unknown unary: ${u.operator}`);
            }
            case "AssignmentExpr": {
                const a = expr as AssignmentExpr;
                const val = this.evaluateExpression(a.value, ctx);
                if (a.target.type === "Identifier") {
                    ctx.assign((a.target as Identifier).name, val, a);
                } else if (a.target.type === "PropertyAccess") {
                    const pa = a.target as PropertyAccess;
                    const obj = this.evaluateExpression(pa.object, ctx);
                    if (typeof obj !== 'object' || obj === null) {
                        throw new ParselError(a.target, `Cannot assign property '${pa.property}' of non-object value.`);
                    }
                    obj[pa.property] = val;
                } else if (a.target.type === "ArrayAccess") {
                    const aa = a.target as ArrayAccess;
                    const arr = this.evaluateExpression(aa.object, ctx);
                    const index = this.evaluateExpression(aa.index, ctx);
                    if (!Array.isArray(arr)) {
                        throw new ParselError(a.target, `Cannot assign element at index '${index}' of non-array value.`);
                    }
                    arr[index] = val;
                } else {
                    throw new ParselError(a.target, `Invalid assignment target: ${a.target.type}`);
                }
                return val;
            }
            case "FuncCall": {
                const f = expr as FunctionCall;
                const fn = ctx.get(f.name, f);
                if (typeof fn !== "function") throw new ParselError(f, `${f.name} is not a function`);
                const args = f.arguments.map(arg => this.evaluateExpression(arg, ctx));
                return fn(...args);
            }
            case "PropertyAccess": {
                const p = expr as PropertyAccess;
                const obj = this.evaluateExpression(p.object, ctx);
                if (typeof obj !== 'object' || obj === null) {
                    throw new ParselError(p.object, `Cannot access property '${p.property}' of non-object value.`);
                }
                return obj[p.property];
            }
            case "ObjectLiteral": {
                const o = expr as ObjectLiteral;
                const out: Record<string, any> = {};
                for (const prop of o.properties) {
                    out[prop.key] = this.evaluateExpression(prop.value, ctx);
                }
                return out;
            }
            case "ArrayLiteral":
                {
                    const a = expr as ArrayLiteral
                    const out: any[] = [];
                    for (const el of a.elements) {
                        out.push(this.evaluateExpression(el, ctx));
                    }
                    return out;
                }
            case "MethodCall": {
                const m = expr as MethodCall
                const obj = this.evaluateExpression(m.object, ctx)
                if (typeof obj !== 'object' || obj === null) {
                    throw new ParselError(m.object, `Cannot call method '${m.method}' on non-object value.`);
                }
                const args = m.arguments.map(arg => this.evaluateExpression(arg, ctx))
                if (typeof obj[m.method] !== 'function') {
                    throw new ParselError(m, `Method '${m.method}' is not a function on object.`);
                }
                return obj[m.method](...args)
            }
            case "LambdaDecl": {
                const l = expr as LambdaDecl
                return this.createFunc(l, ctx)
            }
            case "ArrayAccess": {
                const a = expr as ArrayAccess;
                const arr = this.evaluateExpression(a.object, ctx);
                const index = this.evaluateExpression(a.index, ctx);
                if (!Array.isArray(arr)) {
                    throw new ParselError(a.object, `Cannot access element at index '${index}' of non-array value.`);
                }
                return arr[index];
            }
            default:
                throw new ParselError(expr, `Unsupported expr: ${expr.type}`);
        }
    }

    /** Evaluate binary ops */
    private evalBinary(op: string, l: any, r: any, node: Statement): any {
        switch (op) {
            case "+": return l + r;
            case "-": return l - r;
            case "*": return l * r;
            case "/":
                if (r === 0) throw new ParselError(node, "Division by zero");
                return l / r;
            case "%": return l % r;
            case "==": return l === r;
            case "!=": return l !== r;
            case "<": return l < r;
            case "<=": return l <= r;
            case ">": return l > r;
            case ">=": return l >= r;
            case "&&": return l && r;
            case "||": return l || r;
            default:
                throw new ParselError(node, `Unknown binary operator: ${op}`);
        }
    }

    /** Simple interpolation: {{var}} or nested props like {{user.name}} */
    private interpolate(text: string, ctx: Environment): string {
        return text.replace(/\{\{(.+?)\}\}/g, (_m, expr) => {
            // split on dots:
            const parts = expr.trim().split(".");
            let val = ctx.get(parts[0]);
            for (let i = 1; i < parts.length; i++) {
                if (typeof val !== 'object' || val === null) {
                    throw new ParselError(null, `Cannot access property '${parts[i]}' of non-object value during interpolation: ${parts.slice(0, i).join('.')}`);
                }
                val = val[parts[i]];
            }
            return String(val);
        });
    }
}

export function initGlobals(rt: ParselRuntime) {
    // define built‑ins
    rt.context.define("print", console.log);
    rt.context.define("math", Math);
    rt.context.define("visited", (scene: string) => {
        const was = rt.sceneStack.includes(scene);
        if (!was) rt.sceneStack.push(scene); // This logic seems to be for marking scenes as visited, not checking if they were.
                                            // Ren'Py's visited() checks if a label has been visited.
                                            // For simplicity, I'll keep the current logic but note it.
        return was;
    });

    rt.context.define("input", (prompt?: string) => {
        return input(prompt || "");
    });
}
