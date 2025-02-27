import {Interpreter} from "./NovaScript.js"
import fs from "fs"
const filename = process.argv[2] || "main.flux";

const source = fs.readFileSync(filename, "utf-8");
const runtime = new Interpreter(source)

runtime.globals.define("M_SQRT",Math.sqrt)
runtime.globals.define("M_PI",Math.PI)

//console.log(runtime.parseBlock())
runtime.interpret()
