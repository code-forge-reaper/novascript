import path from "path";
import { Interpreter } from "./NovaScript.ts";
const f = path.resolve(process.argv[2] || "main.nova");
try {
	const interp: Interpreter = new Interpreter(f);

	interp.globals.define("js-import-handler", (name) => {
		try {
			return require(name)
		} catch (e) {
			console.log(e)
			return null
		}
	})

	interp.interpret();
} catch (e) {
	console.log(e.message)
}