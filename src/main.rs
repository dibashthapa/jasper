#![allow(clippy::print_stdout)]
use oxc_allocator::Allocator;
use oxc_ast::Visit;
use wasmtime::*;

use oxc_parser::Parser;
use oxc_span::SourceType;
use std::path::Path;
use wasm::generator::WasmGenerator;
mod engine;
mod wasm;
use anyhow::Result;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let name = std::env::args().nth(1).ok_or("Missing file name").unwrap();
    let path = Path::new(&name);
    let source_text = std::fs::read_to_string(path).map_err(|_| format!("Missing '{name}'"))?;
    let allocator = Allocator::default();
    let source_type = SourceType::from_path(path).unwrap();
    let ret = Parser::new(&allocator, &source_text, source_type).parse();
    // let mut generator = ByteCodeGenerator::default();
    let mut wasm_generator = WasmGenerator::default();
    wasm_generator.visit_program(&ret.program);
    wasm_generator.print_bytecode();
    let wat = wasm_generator.get_wat();
    let engine = Engine::default();
    let module = Module::new(&engine, wat)?;
    let mut store = Store::new(&engine, ());
    let instance = Instance::new(&mut store, &module, &[])?;
    let main_func = instance.get_typed_func::<(), f64>(&mut store, "main")?;
    if !ret.errors.is_empty() {
        for error in ret.errors {
            let error = error.with_source_code(source_text.clone());
            println!("{error:?}");
            println!("Parsed with Errors.");
        }
    }

    let result = main_func.call(&mut store, ())?;
    println!("{}", result);
    Ok(())
}
