convert:
  cargo run for.js 2>/dev/null > example.wat

build: convert
  wat2wasm example.wat -o example.wasm

run: build
 wasm-interp --run-all-exports example.wasm
