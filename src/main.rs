#![allow(clippy::print_stdout)]
use oxc_allocator::Allocator;
use oxc_ast::Visit;

use oxc_parser::Parser;
use oxc_span::SourceType;
use std::path::Path;
use wasm::generator::WasmGenerator;
mod engine;
mod wasm;

fn main() -> Result<(), String> {
    let name = std::env::args().nth(1).ok_or("Missing file name")?;
    let path = Path::new(&name);
    let source_text = std::fs::read_to_string(path).map_err(|_| format!("Missing '{name}'"))?;
    let allocator = Allocator::default();
    let source_type = SourceType::from_path(path).unwrap();
    let ret = Parser::new(&allocator, &source_text, source_type).parse();
    // dbg!(&ret.program);
    // let mut generator = ByteCodeGenerator::default();
    let mut wasm_generator = WasmGenerator::default();
    // generator.visit_program(&ret.program);
    wasm_generator.visit_program(&ret.program);
    wasm_generator.print_bytecode();
    if !ret.errors.is_empty() {
        for error in ret.errors {
            let error = error.with_source_code(source_text.clone());
            println!("{error:?}");
            println!("Parsed with Errors.");
        }
    }

    Ok(())
}
