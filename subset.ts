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
    arguments: Expression[]; // Changed from parameters
}

export interface MethodCall extends Expression {
    type: "MethodCall";
    object: Expression;
    method: string;
    arguments: Expression[]; // Changed from parameters
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
    displayName: string;
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
    text: string;
    body: Statement[];
}

export interface OptionsBlockStmt extends Statement {
    type: "OptionsBlockStmt";
    choices: OptionChoice[];
}
export interface ImportStmt extends Statement {
    type: "ImportStmt";
    body: Statement[];
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
    private keywords: string[] = [
        "var", "func", "return", "def", "as", "end", // Existing subset
        // New Ren'Py
        "char", "scene", "say", "think", "options", "begin", "goto", "set", "to", "in",
        "if", "else", "elseif", "pause", "exit",
        "import"
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
                    if(source[i] === "\n") {
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
            throw new ParselError(token, `Expected token type ${type}, got ${token ? ([JSON.stringify(token, null,1),token.type]) : "EOF"}`);
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
            case "pause":{
                this.consumeToken();
                return { type: "PauseStmt", file: token.file, line: token.line, column: token.column };
            }
            case "exit":{
                this.consumeToken();
                return { type: "ExitStmt", file: token.file, line: token.line, column: token.column };
            }
            case "var": return this.parseVarDeclaration();
            case "func": return this.parseFuncDeclaration();
            case "def": return this.parseLambdaDeclarationAsStatement(); // Lambdas can be assigned, but 'def' as a standalone statement is not typical. Assuming it's part of an assignment.
            case "return": return this.parseReturnStatement();
            case "import": return this.parseImportStatement();
            default:
                // Fallback to expression statement
                const expr = this.parseExpression();
                return { type: "ExpressionStmt", expression: expr, file: token.file, line: token.line, column: token.column };
        }
    }

    parseImportStatement(): ImportStmt {
        const imp = this.consumeToken(); // consume 'import'

        const pathToken = this.expectType("string").value;
        let alias = pathToken
        if(this.getNextToken()?.value === "as") {
            this.consumeToken(); // consume 'as'
            alias = this.expectType("identifier").value;
        }
        const fullPath = path.resolve(path.dirname(this.file), pathToken+".par");
        const parser = new ParselParser(fullPath);
        const body = parser.parse();

        return { type: "ImportStmt", path: fullPath, alias, body, file: imp.file, line: imp.line, column: imp.column };
    }

    // --- Ren'Py Specific Parsers ---

    private parseCharDeclaration(): CharDeclStmt {
        const charToken = this.consumeToken(); // consume 'char'
        const nameToken = this.expectType("identifier");
        const name = nameToken.value;
        this.consumeToken(); // consume identifier

        const displayNameToken = this.expectType("string");
        const displayName = displayNameToken.value;
        this.consumeToken(); // consume string

        return { type: "CharDecl", name, displayName, file: charToken.file, line: charToken.line, column: charToken.column };
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
        const textToken = this.expectType("string");
        const text = textToken.value;
        this.consumeToken(); // consume string
        let who: Identifier | null = null;
        //console.log(`this.getNextToken() = ${JSON.stringify(this.getNextToken())}`);
        if(this.getNextToken() && this.getNextToken()!.type === "keyword" && this.getNextToken()!.value === "as") {
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
        while (this.getNextToken() && this.getNextToken()!.type === "string") {
            const textToken = this.expectType("string");
            const text = textToken.value;
            this.consumeToken(); // consume option text

            this.expectToken("begin");
            this.consumeToken(); // consume 'begin'

            const body = this.parseBlockUntil(["end"]);
            this.expectToken("end");
            this.consumeToken(); // consume 'end'

            choices.push({ text, body });
        }

        // The outer 'options' block also has an 'end'
        this.expectToken("end");
        this.consumeToken(); // consume outer 'end'

        return { type: "OptionsBlockStmt", choices, file: optionsToken.file, line: optionsToken.line, column: optionsToken.column };
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
        // The example uses 'then' but doesn't explicitly consume it.
        // Assuming 'then' is implicit or part of the block structure.
        // If it were a keyword to consume: this.expectToken("then"); this.consumeToken();

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

    // This is for 'def (...)' as a value, not a standalone statement.
    // It's called from parsePrimary, so it returns an Expression.
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

            if (expr.type !== "Identifier" && expr.type !== "PropertyAccess" && expr.type !== "ArrayAccess" && expr.type !== "ArrayLiteral") {
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
                        arguments: args, // Changed from parameters
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
                    arguments: args, // Changed from parameters
                    file: expr.file, line: expr.file, column: expr.column
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
        else if(token.value == "def"){
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
