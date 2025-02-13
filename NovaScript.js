#!/usr/bin/env node
/*
  NovaScript/fluxv2 - A language so simple it makes you wonder why others complicate things!
*/
import fs from "fs"

/* A special exception used to implement returning values from functions */
class ReturnException extends Error {
  constructor(value) {
    super("Return");
    this.value = value;
  }
}

export class Interpreter {
  constructor(source) {
    this.source = source;
    // Tokenize the source code into an array of tokens.
    this.tokens = this.tokenize(source);
    this.current = 0;
    // Global environment for variables and platform functions.
    this.globals = {};
    // Storage for NovaScript function definitions.
    this.functions = {};
    // Storage for macro definitions.
    this.macros = {};

    // Register platform functions.
    // "print" is now a platform function.
    this.registerPlatform("print", (...args) => {
      console.log(...args);
      return undefined;
    });
    // You can add other platform functions here.
  }

  // ----------------------
  // Tokenization (with comment support, booleans, logical operators, and new loop keywords)
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
      // Single-line comment: starts with "//"
      if (char === "/" && i + 1 < length && source[i + 1] === "/") {
        while (i < length && source[i] !== "\n") {
          i++;
        }
        continue;
      }
      // Multi-line comment: starts with "/*" and ends with "*/"
      if (char === "/" && i + 1 < length && source[i + 1] === "*") {
        i += 2; // skip "/*"
        while (i < length && !(source[i] === "*" && i + 1 < length && source[i + 1] === "/")) {
          i++;
        }
        i += 2; // skip "*/"
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
        i++; // skip opening quote
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
        i++; // skip closing quote
        tokens.push({ type: "string", value: str });
        continue;
      }

      // Identifiers, keywords, booleans, and our new loop keywords.
      if (/[A-Za-z_]/.test(char)) {
        let id = "";
        while (i < length && /[A-Za-z0-9_]/.test(source[i])) {
          id += source[i];
          i++;
        }
        // Check for boolean literals.
        if (id === "true" || id === "false") {
          tokens.push({ type: "boolean", value: id === "true" });
        }
        // Recognize reserved keywords (including new loop keywords: forEach, for, do, in).
        else if (["var", "if", "else", "end", "jmp", "func", "label", "return", "def", "import", "while", "forEach", "for", "do", "in"].includes(id)) {
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

      // Allow the dot operator for property access.
      if (char === ".") {
        tokens.push({ type: "operator", value: "." });
        i++;
        continue;
      }

      // Single-character operators/punctuation.
      // Include: +, -, *, /, (, ), ,, {, }, [, ], and :
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
  /**
   * parseBlockUntil stops when one of the given terminator tokens is encountered.
   * It does not consume the terminator so the caller can handle it.
   */
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

  // Top-level block: parse until end-of-input.
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
      this.consumeToken(); // consume "label"
      const nameToken = this.expectType("identifier");
      const name = nameToken.value;
      this.consumeToken();
      return { type: "LabelStmt", name };
    }
    // --- Variable declaration: var x [#modifier] = expression ---
    if (token.type === "keyword" && token.value === "var") {
      this.consumeToken(); // consume "var"
      const nameToken = this.expectType("identifier");
      const name = nameToken.value;
      this.consumeToken();

      // Check for an optional modifier (e.g. #global)
      let modifier = null;
      if (this.getNextToken() && this.getNextToken().type === "operator" && this.getNextToken().value === "#") {
        this.consumeToken(); // consume "#"
        const modToken = this.expectType("identifier");
        modifier = modToken.value; // e.g. "global"
        this.consumeToken();
      }

      this.expectToken("=");
      this.consumeToken();
      const initializer = this.parseExpression();
      return { type: "VarDecl", name, initializer, modifier };
    }

    // --- ForEach loop (Python-like) ---
    // Syntax: forEach <variable> in <list> do <block> end
    if (token.type === "keyword" && token.value === "forEach") {
      this.consumeToken(); // consume "forEach"
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
      this.consumeToken(); // consume "end"
      return { type: "ForEachStmt", variable, list: listExpr, body };
    }

    // --- For loop (Lua-like) ---
    // Syntax: for <variable> = <start>, <end>[, <step>] do <block> end
    if (token.type === "keyword" && token.value === "for") {
      this.consumeToken(); // consume "for"
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
        this.consumeToken(); // consume the comma
        stepExpr = this.parseExpression();
      }
      this.expectToken("do");
      this.consumeToken();
      const body = this.parseBlockUntil(["end"]);
      this.expectToken("end");
      this.consumeToken(); // consume "end"
      return { type: "ForStmt", variable, start: startExpr, end: endExpr, step: stepExpr, body };
    }

    // --- While loop ---
    // Syntax: while condition <block> end
    if (token.type === "keyword" && token.value === "while") {
      this.consumeToken();
      const condition = this.parseExpression();
      const body = this.parseBlockUntil(["end"]);
      this.expectToken("end");
      this.consumeToken(); // consume "end"
      return { type: "WhileStmt", condition, body };
    }

    // --- If statement ---
    // Syntax: if condition <thenBlock> [else <elseBlock>] end
    if (token.type === "keyword" && token.value === "if") {
      this.consumeToken();
      const condition = this.parseExpression();
      const thenBlock = this.parseBlockUntil(["else", "end"]);
      let elseBlock = null;
      if (this.getNextToken() && this.getNextToken().type === "keyword" && this.getNextToken().value === "else") {
        this.consumeToken(); // consume "else"
        elseBlock = this.parseBlockUntil(["end"]);
      }
      this.expectToken("end");
      this.consumeToken(); // consume "end"
      return { type: "IfStmt", condition, thenBlock, elseBlock };
    }

    // --- Jump statement: jmp expression ---
    if (token.type === "keyword" && token.value === "jmp") {
      this.consumeToken();
      const expression = this.parseExpression();
      return { type: "JmpStmt", expression };
    }

    // --- Function declaration ---
    // Syntax: func name(param1, param2, …) <block> end
    // Now with default parameter support: parameters can be written as: param or param=defaultExpr
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
          let defaultExpr = undefined;
          if (this.getNextToken() && this.getNextToken().value === "=") {
            this.consumeToken(); // consume "="
            defaultExpr = this.parseExpression();
          }
          parameters.push({ name: paramName, default: defaultExpr });
          if (this.getNextToken() && this.getNextToken().value === ",") {
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
      this.consumeToken(); // consume "end"
      return { type: "FuncDecl", name, parameters, body };
    }

    // --- Macro definition (for compile-time expansion) ---
    // Syntax: def MACRO_NAME(param1, …) <single-expression> end
    if (token.type === "keyword" && token.value === "def") {
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
          parameters.push(paramToken.value);
          this.consumeToken();
          if (this.getNextToken() && this.getNextToken().value === ",") {
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
      this.consumeToken(); // consume "end"
      return { type: "MacroDecl", name, parameters, body };
    }

    // --- Return statement ---
    // Syntax: return [expression]
    if (token.type === "keyword" && token.value === "return") {
      this.consumeToken();
      let expression = null;
      if (this.getNextToken() && this.getNextToken().value !== "\n") {
        expression = this.parseExpression();
      }
      return { type: "ReturnStmt", expression };
    }

    // --- Import statement (Node only) ---
    // Syntax: import "filename"
    if (token.type === "keyword" && token.value === "import") {
      this.consumeToken();
      const fileToken = this.expectType("string");
      const filename = fileToken.value;
      this.consumeToken();
      return { type: "ImportStmt", filename };
    }

    // --- Expression statement (fallback) ---
    const expr = this.parseExpression();
    return { type: "ExpressionStmt", expression: expr };
  }

  // --- Expression Parsing (Recursive Descent) ---
  // We add assignment expressions as the lowest-precedence (right-associative) operator.
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
      this.consumeToken(); // consume "="
      const valueExpr = this.parseAssignment(); // right-associative
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

  // --- Modified parsePrimary with support for property access, booleans, arrays, objects, and function calls ---
  parsePrimary() {
    let node;
    const token = this.getNextToken();
    if (!token) {
      throw new Error("Unexpected end of input");
    }
    
    // Boolean literal support.
    if (token.type === "boolean") {
      this.consumeToken();
      node = { type: "Literal", value: token.value };
    }
    // Literals: number or string.
    else if (token.type === "number" || token.type === "string") {
      this.consumeToken();
      node = { type: "Literal", value: token.value };
    }
    // Array literal.
    else if (token.value === "[") {
      this.consumeToken(); // consume "["
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
      this.consumeToken(); // consume "]"
      node = { type: "ArrayLiteral", elements };
    }
    // Object literal.
    else if (token.value === "{") {
      this.consumeToken(); // consume "{"
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
          this.consumeToken(); // consume ":"
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
      this.consumeToken(); // consume "}"
      node = { type: "ObjectLiteral", properties };
    }
    // Identifiers (and function calls).
    else if (token.type === "identifier") {
      this.consumeToken();
      // Function call if followed by "(".
      if (this.getNextToken() && this.getNextToken().value === "(") {
        this.consumeToken(); // consume "("
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
    // Parenthesized expression.
    else if (token.value === "(") {
      this.consumeToken();
      node = this.parseExpression();
      this.expectToken(")");
      this.consumeToken();
    }
    else {
      throw new Error(`Unexpected token: ${JSON.stringify(token)}`);
    }

    // Handle property access via the dot operator.
    while (this.getNextToken() && this.getNextToken().value === ".") {
      this.consumeToken(); // consume "."
      const propToken = this.expectType("identifier");
      this.consumeToken();
      node = { type: "PropertyAccess", object: node, property: propToken.value };
    }
    return node;
  }

  // ----------------------
  // Execution
  // ----------------------
  execute(statements, env = this.globals) {
    // Build a map of labels for jump support.
    const labels = {};
    for (let j = 0; j < statements.length; j++) {
      if (statements[j].type === "LabelStmt") {
        labels[statements[j].name] = j;
      }
    }
    let i = 0;
    while (i < statements.length) {
      this.globals.__line__ = i;
      const stmt = statements[i];
      this.executeStatement(stmt, env);
      if (stmt.type === "JmpStmt") {
        const target = this.evaluate(stmt.expression, env);
        if (typeof target === "string") {
          if (labels.hasOwnProperty(target)) {
            i = labels[target];
            continue;
          } else {
            throw new Error(`Undefined label: ${target}`);
          }
        } else if (typeof target === "number") {
          i = target;
          continue;
        }
      }
      i++;
    }
  }

  executeStatement(stmt, env) {
    switch (stmt.type) {
      case "VarDecl": {
        const value = this.evaluate(stmt.initializer, env);
        // If the variable is marked as global, store it in the global environment.
        if (stmt.modifier === "global") {
          this.globals[stmt.name] = value;
        } else {
          env[stmt.name] = value;
        }
        break;
      }

      case "IfStmt": {
        const condition = this.evaluate(stmt.condition, env);
        if (condition) {
          this.execute(stmt.thenBlock, env);
        } else if (stmt.elseBlock) {
          this.execute(stmt.elseBlock, env);
        }
        break;
      }
      // --- ForEach loop execution ---
      case "ForEachStmt": {
        const listVal = this.evaluate(stmt.list, env);
        if (!Array.isArray(listVal)) {
          throw new Error("forEach loop expects a list (array)");
        }
        for (let element of listVal) {
          let loopEnv = Object.create(env);
          loopEnv[stmt.variable] = element;
          this.execute(stmt.body, loopEnv);
        }
        break;
      }
      // --- For loop (Lua-like) execution ---
      case "ForStmt": {
        const startVal = this.evaluate(stmt.start, env);
        const endVal = this.evaluate(stmt.end, env);
        const stepVal = stmt.step ? this.evaluate(stmt.step, env) : 1;
        if (typeof startVal !== "number" || typeof endVal !== "number" || typeof stepVal !== "number") {
          throw new Error("For loop expects numeric start, end, and step values");
        }
        if (stepVal === 0) {
          throw new Error("For loop step cannot be 0");
        }
        if (stepVal > 0) {
          for (let i = startVal; i <= endVal; i += stepVal) {
            let loopEnv = Object.create(env);
            loopEnv[stmt.variable] = i;
            this.execute(stmt.body, loopEnv);
          }
        } else { // stepVal < 0
          for (let i = startVal; i >= endVal; i += stepVal) {
            let loopEnv = Object.create(env);
            loopEnv[stmt.variable] = i;
            this.execute(stmt.body, loopEnv);
          }
        }
        break;
      }
      // --- While loop execution ---
      case "WhileStmt": {
        while (this.evaluate(stmt.condition, env)) {
          try {
            this.execute(stmt.body, env);
          } catch (e) {
            if (e instanceof ReturnException) {
              throw e;
            } else {
              throw e;
            }
          }
        }
        break;
      }
      case "JmpStmt": {
        break;
      }
      case "FuncDecl": {
        this.functions[stmt.name] = stmt;
        break;
      }
      case "MacroDecl": {
        this.macros[stmt.name] = stmt;
        break;
      }
      case "ReturnStmt": {
        const value = stmt.expression ? this.evaluate(stmt.expression, env) : undefined;
        throw new ReturnException(value);
      }
      case "ImportStmt": {
        if (typeof window === "undefined") {
          const source = fs.readFileSync(stmt.filename, "utf-8");
          const interpreter = new Interpreter(source);
          interpreter.globals = env;
          interpreter.run();
        } else {
          throw new Error("Import not supported in browser");
        }
        break;
      }
      case "ExpressionStmt": {
        this.evaluate(stmt.expression, env);
        break;
      }
      case "LabelStmt": {
        break;
      }
      default:
        throw new Error(`Unknown statement type: ${stmt.type}`);
    }
  }

  evaluate(expr, env) {
    switch (expr.type) {
      case "Literal": {
        return expr.value;
      }
      case "Identifier": {
        if (env.hasOwnProperty(expr.name)) {
          return env[expr.name];
        } else if (this.functions.hasOwnProperty(expr.name)) {
          return this.functions[expr.name];
        } else {
          return undefined;
        }
      }
      case "AssignmentExpr": {
        const value = this.evaluate(expr.value, env);
        if (expr.target.type === "Identifier") {
          env[expr.target.name] = value;
          return value;
        } else if (expr.target.type === "PropertyAccess") {
          let obj = this.evaluate(expr.target.object, env);
          if (obj === null || obj === undefined) {
            throw new Error("Cannot assign to property of null or undefined");
          }
          obj[expr.target.property] = value;
          return value;
        } else {
          throw new Error("Invalid left-hand side in assignment");
        }
      }
      case "BinaryExpr": {
        const left = this.evaluate(expr.left, env);
        const right = this.evaluate(expr.right, env);
        switch (expr.operator) {
          case "+": return left + right;
          case "-": return left - right;
          case "*": return left * right;
          case "/": return left / right;
          case "==": return left == right;
          case "!=": return left != right;
          case ">": return left > right;
          case ">=": return left >= right;
          case "<": return left < right;
          case "<=": return left <= right;
          case "&&": return left && right;
          case "||": return left || right;
          default:
            throw new Error(`Unknown binary operator ${expr.operator}`);
        }
      }
      case "UnaryExpr": {
        const right = this.evaluate(expr.right, env);
        switch (expr.operator) {
          case "-": return -right;
          case "!": return !right;
          default:
            throw new Error(`Unknown unary operator ${expr.operator}`);
        }
      }
      case "ArrayLiteral": {
        return expr.elements.map(e => this.evaluate(e, env));
      }
      case "ObjectLiteral": {
        const obj = {};
        for (let prop of expr.properties) {
          obj[prop.key] = this.evaluate(prop.value, env);
        }
        return obj;
      }
      case "PropertyAccess": {
        const obj = this.evaluate(expr.object, env);
        if (obj === null || obj === undefined) {
          throw new Error("Cannot access property of null or undefined");
        }
        return obj[expr.property];
      }
      case "FuncCall": {
        // Macro expansion.
        if (this.macros.hasOwnProperty(expr.name)) {
          const macro = this.macros[expr.name];
          const expanded = this.expandMacro(macro, expr.arguments);
          return this.evaluate(expanded, env);
        }
        // Check for platform functions.
        const candidate = env[expr.name];
        if (typeof candidate === "function") {
          const args = expr.arguments.map(arg => this.evaluate(arg, env));
          return candidate(...args);
        }
        // NovaScript functions.
        if (this.functions.hasOwnProperty(expr.name)) {
          const func = this.functions[expr.name];
          const args = expr.arguments.map(arg => this.evaluate(arg, env));
          const newEnv = Object.create(env);
          // --- Default Parameter Handling ---
          for (let i = 0; i < func.parameters.length; i++) {
            let param = func.parameters[i]; // { name, default }
            if (i < args.length) {
              newEnv[param.name] = args[i];
            } else if (param.default !== undefined) {
              newEnv[param.name] = this.evaluate(param.default, newEnv);
            } else {
              newEnv[param.name] = undefined;
            }
          }
          try {
            this.execute(func.body, newEnv);
            return undefined;
          } catch (e) {
            if (e instanceof ReturnException) {
              return e.value;
            } else {
              throw e;
            }
          }
        }
        throw new Error(`Undefined function: ${expr.name}`);
      }
      default:
        throw new Error(`Unknown expression type: ${expr.type}`);
    }
  }

  // --- Macro Expansion Helpers ---
  expandMacro(macro, argASTs) {
    if (macro.body.length !== 1 || macro.body[0].type !== "ExpressionStmt") {
      throw new Error("Macro must consist of a single expression statement");
    }
    const expr = macro.body[0].expression;
    return this.substitute(expr, macro.parameters, argASTs);
  }

  substitute(ast, params, args) {
    if (ast.type === "Identifier") {
      const index = params.indexOf(ast.name);
      if (index !== -1) {
        return args[index];
      }
      return ast;
    } else if (ast.type === "BinaryExpr") {
      return {
        type: "BinaryExpr",
        operator: ast.operator,
        left: this.substitute(ast.left, params, args),
        right: this.substitute(ast.right, params, args)
      };
    } else if (ast.type === "UnaryExpr") {
      return {
        type: "UnaryExpr",
        operator: ast.operator,
        right: this.substitute(ast.right, params, args)
      };
    } else if (ast.type === "Literal") {
      return ast;
    } else if (ast.type === "FuncCall") {
      return {
        type: "FuncCall",
        name: ast.name,
        arguments: ast.arguments.map(arg => this.substitute(arg, params, args))
      };
    }
    return ast;
  }

  // --- Platform Function and Value Support ---
  registerPlatform(name, val) {
    this.globals[name] = val;
  }

  // ----------------------
  // Run the Interpreter
  // ----------------------
  run() {
    const statements = this.parseBlock();
    this.execute(statements);
    return statements;
  }
}
