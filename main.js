#!/usr/bin/env node
import { Interpreter } from './NovaScript.js'; // assuming your interpreter is saved as nova.js
//import fs from 'fs';
import { readFileSync } from 'fs';

const arg = process.argv[2]||"main.nova"
const code = readFileSync(arg).toString()
const interpreter = new Interpreter(code);

interpreter.globals.define("js-import-handler", (name)=>{
	try{// since node for some fucking reason doesn't let you use both import and require, you have to use [bun](https://bun.sh/)
		return require(name)
	}catch(e){
		console.log(e)
		return null
	}
})

interpreter.interpret();
