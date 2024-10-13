%.wasm: %.wat
	wat2wasm $^ -o $@

%: %.wasm
	node index.js $^
