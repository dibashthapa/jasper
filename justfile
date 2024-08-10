build:
  cargo run 2>/dev/null > example.wat
  wat2wasm example.wat -o example.wasm
run: build
 wasm-interp --run-all-exports example.wasm
