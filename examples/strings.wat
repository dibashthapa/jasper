(module
    (memory $m 1)
    (export "memory" (memory $m))
    (global $alloc_offset (mut i32) (i32.const 32))

    ;; Store strings with their length prefixes
    (data (i32.const 1) "\28Hello my name is dibash and i love nepal")
    (data (i32.const 44) "\13i also love srijana")

    (func $len (param $addr i32) (result i32)
        local.get $addr
        call $nullthrow
        i32.load8_u  ;; Changed to load8_u since we're storing single byte lengths
    )

    (func $nullthrow (param $addr i32) (result i32)
        local.get $addr
        i32.eqz
        (if
            (then
                unreachable
            )
        )
        local.get $addr
    )


    (func $alloc (param $size i32) (result i32)
        (local $curr_alloc_addr i32)
        (local $next_alloc_addr i32)
        global.get $alloc_offset
        local.set $curr_alloc_addr
        local.get $curr_alloc_addr
        local.get $size
        i32.add
        local.set $next_alloc_addr
        local.get $next_alloc_addr
        global.set $alloc_offset
        local.get $curr_alloc_addr
    )

    (func $mem_cpy (param $src i32) (param $dest i32) (param $size i32)
        (local $idx i32)
        (local $temp i32)
        i32.const 0
        local.set $idx
        (block $block
            (loop $loop
                local.get $idx
                local.get $size
                i32.lt_s
                i32.eqz
                br_if $block
                local.get $idx
                local.get $src
                i32.add
                i32.load8_u
                local.set $temp
                local.get $idx
                local.get $dest
                i32.add
                local.get $temp
                i32.store8
                local.get $idx
                i32.const 1
                i32.add
                local.set $idx
                br $loop
            )
        )
    )

            (func $concatenate_strings (param $s1 i32) (param $s2 i32) (result i32)
                (local $len1 i32)
                (local $len2 i32)
                (local $addr i32)
                (local $total_len i32)

                ;; Get lengths from the length prefixes
                local.get $s1
                call $len
                local.set $len1

                local.get $s2
                call $len
                local.set $len2

                ;; Calculate total length needed (lengths + combined content + new length byte)
                local.get $len1
                local.get $len2
                i32.add
                local.tee $total_len
                i32.const 1    ;; Add 1 for the length prefix
                i32.add
                call $alloc
                local.set $addr

                ;; Store new combined length
                local.get $addr
                local.get $total_len
                i32.store8

                ;; Copy first string content (skipping its length prefix)
                local.get $s1
                i32.const 1
                i32.add        ;; Skip length byte
                local.get $addr
                i32.const 1
                i32.add        ;; Skip length byte
                local.get $len1
                call $mem_cpy

                ;; Copy second string content (skipping its length prefix)
                local.get $s2
                i32.const 1
                i32.add        ;; Skip length byte
                local.get $addr
                i32.const 1
                i32.add
                local.get $len1
                i32.add        ;; Position after first string
                local.get $len2
                call $mem_cpy

                local.get $addr
            )

    (func (export "test") (result i32)
        memory.size
    )

    (export "allocate" (func $alloc))
    (export "concatenate_strings" (func $concatenate_strings))
)
