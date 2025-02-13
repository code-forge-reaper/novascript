import {Interpreter} from "./NovaScript.js"
import fs from "fs"
const filename = process.argv[2] || "main.flux";

const source = fs.readFileSync(filename, "utf-8");
const runtime = new Interpreter(source)

runtime.registerPlatform("M_SQRT", Math.sqrt)
runtime.registerPlatform("M_PI", Math.PI)

runtime.run()
