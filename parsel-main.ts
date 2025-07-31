#!/usr/bin/env node
import {
	ParselParser,
	ParselRuntime
} from "./parsel.ts";


// ---- bootstrap ----
const parser = new ParselParser(process.argv[2] || "main.par");
const ast = parser.parse();
const rt = new ParselRuntime(ast);
rt.parse();
rt.play();
