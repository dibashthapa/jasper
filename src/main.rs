#![allow(clippy::print_stdout)]
use oxc_allocator::Allocator;
use wasmtime::*;

use oxc_parser::{Parser, ParserReturn};
use oxc_span::SourceType;
use std::{fs, path::Path};
use wasm::generator::CompileContext;
mod wasm;
use anyhow::Result;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let name = std::env::args().nth(1).ok_or("Missing file name").unwrap();
    let path = Path::new(&name);
    let source_text = std::fs::read_to_string(path).map_err(|_| format!("Missing '{name}'"))?;
    let allocator = Allocator::default();
    let source_type = SourceType::from_path(path).unwrap();
    let ParserReturn {
        program, errors, ..
    } = Parser::new(&allocator, &source_text, source_type).parse();
    assert_eq!(errors.len(), 0);
    let mut compiler = CompileContext::default();
    compiler.compile_statements(&program.body);
    let output = compiler.finish();
    fs::write("random.wat", &output)?;
    let engine = Engine::default();
    let module = Module::new(&engine, output)?;
    let mut store = Store::new(&engine, ());
    let print_num = wasm::console::print_num(&mut store);
    let print_float = wasm::console::print_float(&mut store);
    let print_str = wasm::console::print_str(&mut store);
    let instance = Instance::new(
        &mut store,
        &module,
        &[print_str.into(), print_num.into(), print_float.into()],
    )?;
    let main_func = instance.get_typed_func::<(), ()>(&mut store, "main")?;

    main_func.call(&mut store, ())?;
    Ok(())
}
