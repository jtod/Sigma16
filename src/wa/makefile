wasm:
	make f42.wasm

test:
	make wasm
	make testf42

emcore.wasm: emwcore.wat
	wat2wasm emwcore.wat

f42.wasm: f42.wat
	wat2wasm f42.wat
testf42: f42.wasm runf42.js
	node runf42.js
