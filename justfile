convert input:
    cargo run -- {{input}} 2>/dev/null > {{without_extension(input)}}.wat

build input: (convert input)
    wat2wasm {{without_extension(input)}}.wat -o {{without_extension(input)}}.wasm

run input: (build input)
    wasm-interp --run-all-exports {{without_extension(input)}}.wasm
