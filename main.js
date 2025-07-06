#!/usr/bin/env node
import { Interpreter } from './nova.js'; // assuming your interpreter is saved as nova.js
import fs from 'fs';
const arg = process.argv[2]||"main.nova"
const code = fs.readFileSync(arg).toString()
const interpreter = new Interpreter(code);
interpreter.globals.define("isTesting", ()=>{
	return process.env.TESTING === "true"
})
interpreter.interpret();
