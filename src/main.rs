#![allow(clippy::print_stdout)]
use oxc_allocator::Allocator;
use oxc_ast::Visit;
use wasmtime::*;

use oxc_parser::Parser;
use oxc_span::SourceType;
use std::{fs, path::Path};
use wasm::generator::WasmGenerator;
mod wasm;
use anyhow::Result;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let name = std::env::args().nth(1).ok_or("Missing file name").unwrap();
    let path = Path::new(&name);
    let source_text = std::fs::read_to_string(path).map_err(|_| format!("Missing '{name}'"))?;
    let allocator = Allocator::default();
    let source_type = SourceType::from_path(path).unwrap();
    let ret = Parser::new(&allocator, &source_text, source_type).parse();
    let mut wasm_generator = WasmGenerator::default();
    wasm_generator.visit_program(&ret.program);
    let wat = wasm_generator.get_wat();
    println!("{}", wat);
    let engine = Engine::default();
    let module = Module::new(&engine, wat)?;
    let mut store = Store::new(&engine, ());
    let print_str = wasm::console::print_str(&mut store);
    let instance = Instance::new(&mut store, &module, &[print_str.into()])?;
    let main_func = instance.get_typed_func::<(), ()>(&mut store, "main")?;
    main_func.call(&mut store, ())?;
    Ok(())
}
