import {
	AssignmentExpr,
	CharDeclStmt,
	ExpressionStmt,
	GotoStmt,
	Identifier,
	ParselError,
	ParselParser,
	OptionsBlockStmt,
	Parameter,
	SayStmt,
	SceneDeclStmt,
	Statement,
	ThinkStmt,
	Token,
	VarDeclStmt,
	IfStmt,
	BinaryExpr,
	UnaryExpr,
	Literal,
	PropertyAccess,
	ObjectLiteral,
	ArrayLiteral,
	MethodCall,
	FunctionCall,
	FuncDecl,
	LambdaDecl,
	ReturnStmt,
    ImportStmt
} from "./subset.ts";
//import readline from "readline-sync";
import {input} from "./sync-input/index.ts"

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
class ParselRuntime {
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
		if(expected === "bool") actualType = "boolean"

		switch(actualType){
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
		const func = (...args: Parameter[])=>{
			const funcEnv = new Environment(ctx);

			for(let i = 0; i < s.parameters.length; i++){
				const param = s.parameters[i];
				let argVal = args[i]
				if(argVal === undefined && param.default !== undefined){
					argVal = this.evaluateExpression(param.default, ctx);
				}else if(argVal === undefined && param.default === undefined){
					throw new ParselError(null, `Missing required parameter ${param.name} in function ${s.name}`);
				}

				if(param.annotationType){
					this.checkType(param.annotationType, argVal, param);
				}

				funcEnv.define(param.name, argVal)
			}

			try{
				s.body.forEach(e=>this.executeStatement(e, funcEnv));
			}catch(e){
				if(e instanceof ReturnException){
					return e.value;
				}else{
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

			case "CharDecl": {
				const s = stmt as CharDeclStmt;
				ctx.define(s.name, s.displayName);
				break;
			}
			case "ExpressionStmt": {
				this.evaluateExpression((stmt as ExpressionStmt).expression, ctx);
				break;
			}
			case "SayStmt": {
				const s = stmt as SayStmt;
				let who = s.who ? this.evaluateExpression(s.who, ctx) + ": " : "";
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
				const choices = ob.choices.map(c => c.text);
				const idx = showChoice(choices);
				for (const child of ob.choices[idx].body) {
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
                const value = stmt.expression ? this.evaluateExpression(stmt.expression, ctx) : undefined;
                throw new ReturnException(value);

			case "PauseStmt":
				input("(Press enter to continue)");
				break;
			case "ImportStmt":
				{
					const s = stmt as ImportStmt;
					const interp = new ParselRuntime(s.body)
					const env = new Environment(this.context);
					interp.context = env
					interp.parse()
					const namespace: Record<string, any> = {}

					for(const key in env.values){
						namespace[key] = env.values[key]
					}

					ctx.define(s.alias, namespace)
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
				} else {
					const pa = a.target as PropertyAccess;
					const obj = this.evaluateExpression(pa.object, ctx);
					obj[pa.property] = val;
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
					//console.log(expr)
					const out: any[] = [];
					for (const el of a.elements) {
						out.push(this.evaluateExpression(el, ctx));
					}
					return out;
				}
			case "MethodCall": {
				const m = expr as MethodCall
				const obj = this.evaluateExpression(m.object, ctx)
				const args = m.arguments.map(arg => this.evaluateExpression(arg, ctx))
				return obj[m.method](...args)
			}
			case "LambdaDecl":{
				const l = expr as LambdaDecl
				return this.createFunc(l, ctx)
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
				val = val[parts[i]];
			}
			return String(val);
		});
	}
}

function initGlobals(rt: ParselRuntime) {
	// define built‑ins
	rt.context.define("print", console.log);
	rt.context.define("visited", (scene: string) => {
		const was = rt.sceneStack.includes(scene);
		if (!was) rt.sceneStack.push(scene);
		return was;
	});

	rt.context.define("input", (prompt ?: string) => {
		return input(prompt || "");
	});
}

// ---- bootstrap ----
const parser = new ParselParser(process.argv[2] || "main.par");
const ast = parser.parse();
const rt = new ParselRuntime(ast);
rt.parse();
rt.play();
