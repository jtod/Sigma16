// This is runf42.hs
//   wat2wasm f42.wat
//   node runf42.js

const { readFileSync } = require("fs");

const run = async () => {
    const buffer = readFileSync("./f42.wasm");
    const module = await WebAssembly.compile(buffer);
    const instance = await WebAssembly.instantiate(module);
    console.log ("ready");
    console.log(instance.exports.f42());
}

run()


