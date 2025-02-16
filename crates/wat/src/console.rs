use anyhow::{anyhow, Result};
use wasmtime::{Caller, Func, Store};

pub fn print_num(store: &mut Store<()>) -> Func {
    Func::wrap(store, |_: Caller<'_, ()>, num: i32| -> Result<()> {
        println!("{num}");
        Ok(())
    })
}

pub fn print_float(store: &mut Store<()>) -> Func {
    Func::wrap(store, |_: Caller<'_, ()>, num: f64| -> Result<()> {
        println!("{num}");
        Ok(())
    })
}

pub fn print_str(store: &mut Store<()>) -> Func {
    Func::wrap(
        store,
        |mut caller: Caller<'_, ()>, ptr: i32| -> Result<()> {
            let mem = caller
                .get_export("memory")
                .and_then(|ext| ext.into_memory())
                .ok_or_else(|| anyhow!("failed to find host memory"))?;

            // Read the length prefix (first byte)
            let len = mem.data(&caller)[ptr as usize] as usize;

            // Read the string data (UTF-8 assumed)
            let data = mem
                .data(&caller)
                .get((ptr + 1) as usize..)
                .and_then(|arr| arr.get(..len))
                .ok_or_else(|| anyhow!("pointer/length out of bounds"))?;

            // Attempt UTF-8 decoding
            if let Ok(utf8_string) = std::str::from_utf8(data) {
                println!("{}", utf8_string);
            } else {
                // Fallback to UTF-16 decoding
                let utf16_units: Vec<u16> = data
                    .chunks_exact(2)
                    .map(|chunk| u16::from_le_bytes([chunk[0], chunk[1]]))
                    .collect();

                let utf16_string =
                    String::from_utf16(&utf16_units).map_err(|_| anyhow!("invalid utf-16 data"))?;

                println!("{}", utf16_string);
            }

            Ok(())
        },
    )
}
