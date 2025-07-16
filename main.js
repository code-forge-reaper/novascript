#!/usr/bin/env bun run
import path from 'path';
import { Interpreter } from './NovaScript.js'; // assuming your interpreter is saved as nova.js
//import fs from 'fs';
import { readFileSync } from 'fs';
const p = path.resolve(process.argv[2] || "main.nova")

//const code = readFileSync(p).toString()
const interpreter = new Interpreter(p);

interpreter.globals.define("js-import-handler", (name) => {
	try {
		return require(name)
	} catch (e) {
		console.log(e)
		return null
	}
})
interpreter.interpret();
