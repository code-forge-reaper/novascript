#!/usr/bin/env node
import path from "node:path";
import { fileURLToPath } from "node:url"
import { createRequire } from "node:module"
import {
	ParselParser,
	ParselRuntime
} from "./parsel.ts";

// resolve __dirname (TS/ESM doesn't have __dirname)
const __filename = fileURLToPath(import.meta.url)
const __dirname = path.dirname(__filename)

const f = path.resolve(process.argv[2] || "main.par");

// create a require function based on cwd, not this file
const requireFromCWD = createRequire(path.join(process.cwd(), 'noop.js'))

try {
	const parser = new ParselParser(f);
	const interp = new ParselRuntime(parser.parse())

	interp.context.define("os-import-handler", (name: string) => {
		try {
			return requireFromCWD(name)
		} catch (e) {
			console.error(e)
			return null
		}
	})

	interp.parse();

	interp.play();
} catch (e) {
	console.log((e as Error).message)
}

