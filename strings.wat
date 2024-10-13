(module
    (memory $m 1)
    (export "mem" (memory $m))
    (data (i32.const 0) "Hello")
    (data (i32.const 5) "World")

    (func (export "test")
        (result i32)
        i32.const 5
    )
)
