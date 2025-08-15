#!/usr/bin/env node
import path from "node:path";
import { fileURLToPath } from "node:url"
import { createRequire } from "node:module"
import { Interpreter } from "./NovaScript.ts";
const f = path.resolve(process.argv[2] || "main.nova");

// resolve __dirname (TS/ESM doesn't have __dirname)
const __filename = fileURLToPath(import.meta.url)
const __dirname = path.dirname(__filename)

// create a require function based on cwd, not this file
const requireFromCWD = createRequire(path.join(process.cwd(), 'noop.js'))

try {
	const interp: Interpreter = new Interpreter(f);
	/*interp.exposeJsClass("Test", class{
		constructor(name){
			console.log(name)
		}
	})*/

	interp.globals.define("os-import-handler", (name: string) => {
		try {
			return requireFromCWD(name)
		} catch (e) {
			console.error(e)
			return null
		}
	})


	interp.interpret();
} catch (e) {
	console.log((e as Error).message)
}
