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
            let len = mem.data(&caller)[ptr as usize] + 1;

            let data = mem
                .data(&caller)
                .get((ptr + 1) as usize..)
                .and_then(|arr| arr.get(..len as usize))
                .ok_or_else(|| anyhow!("pointer/length out of bounds"))?;

            let utf8_string =
                std::str::from_utf8(data).map_err(|_| anyhow!("invalid utf-8 data"))?;

            println!("{}", utf8_string);

            Ok(())
        },
    )
}
