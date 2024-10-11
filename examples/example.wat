(memory $mem 1)
(func $main(result i32 i32)
    i32.const 0xcafe
    i32.const 42
    i32.store


    i32.const 0x10000
    i32.const 43
    i32.store8

    i32.const 0xcafe
    i32.load


    i32.const 0x10000
    i32.load8_u

)
