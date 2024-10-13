#![allow(clippy::print_stdout)]
use oxc_allocator::Allocator;
use oxc_ast::Visit;
use wasmtime::*;

use core::str;
use oxc_parser::Parser;
use oxc_span::SourceType;
use std::path::Path;
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

    let log_str = Func::wrap(
        &mut store,
        |mut caller: Caller<'_, ()>, ptr: i32, len: i32| {
            let mem = match caller.get_export("memory") {
                Some(Extern::Memory(mem)) => mem,
                _ => anyhow::bail!("failed to find host memory"),
            };
            let data = mem
                .data(&caller)
                .get(ptr as u32 as usize..)
                .and_then(|arr| arr.get(..len as u32 as usize));
            let string = match data {
                Some(data) => match str::from_utf8(data) {
                    Ok(s) => s,
                    Err(_) => anyhow::bail!("invalid utf-8"),
                },
                None => anyhow::bail!("pointer/length out of bounds"),
            };
            println!("{}", string);
            Ok(())
        },
    );

    let instance = Instance::new(&mut store, &module, &[log_str.into()])?;
    let main_func = instance.get_typed_func::<(), ()>(&mut store, "main")?;
    if !ret.errors.is_empty() {
        for error in ret.errors {
            let error = error.with_source_code(source_text.clone());
            println!("{error:?}");
            println!("Parsed with Errors.");
        }
    }

    main_func.call(&mut store, ())?;
    Ok(())
}
