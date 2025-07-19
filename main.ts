import path from "path";
import { Interpreter } from "./NovaScript";
const f = path.resolve(process.argv[2] || "main.nova");
const interp : Interpreter = new Interpreter(f);

interp.globals.define("js-import-handler", (name) => {
	try {
		return require(name)
	} catch (e) {
		console.log(e)
		return null
	}
})

interp.interpret();
