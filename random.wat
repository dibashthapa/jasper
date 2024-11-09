(module
(import "" "" (func $print_str(param i32)))
                
(import "" "" (func $print_number(param f64)))
                
(memory (export "memory") 1) 
 (global $alloc_offset (mut i32) (i32.const 1024))(global $a (mut f64) (f64.const 5))
(global $i (mut f64) (f64.const 0))
  
 
            (func $len (param $addr i32) (result i32)
                local.get $addr
                call $nullthrow
                i32.load8_u  ;; changed to load8_u since we're storing single byte lengths
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



            (func $alloc (param $size i32) (result i32)
                (local $curr_alloc_addr i32)
                (local $next_alloc_addr i32)

                ;; get current allocation offset and store it in $curr_alloc_addr
                global.get $alloc_offset
                local.set $curr_alloc_addr

                ;; calculate next allocation address and store it in $next_alloc_addr
                local.get $curr_alloc_addr
                local.get $size
                i32.add
                local.set $next_alloc_addr

                ;; update global allocation offset
                local.get $next_alloc_addr
                global.set $alloc_offset

                ;; return the current allocation address
                local.get $curr_alloc_addr
            )

            (func $lowercase (param $addr i32) (result i32)
                (local $len i32)
                (local $idx i32)
                (local $char i32)
                (local $new_addr i32)

                ;; Get the length of the original string
            local.get $addr
            call $len
            local.set $len

            ;; Allocate memory for the new string (length prefix + content)
            local.get $len
            i32.const 1      ;; Add 1 byte for the length prefix
            i32.add
            call $alloc
            local.set $new_addr

            ;; Store the length of the new string at the start
            local.get $new_addr
            local.get $len
            i32.store8

            ;; Initialize index to 0
            i32.const 0
            local.set $idx

            ;; Loop to process each character
            (loop $loop
            ;; Check if the index is less than the length
            local.get $idx
            local.get $len
            i32.lt_s
            (if
                (then
                ;; Load the character at the source address + index + 1 (skip length prefix)
                local.get $addr
                i32.const 1
                i32.add
                local.get $idx
                i32.add
                i32.load8_u
                local.set $char

                ;; Check if the character is uppercase (ASCII 65 to 90)
                local.get $char
                i32.const 65
                i32.ge_s
                local.get $char
                i32.const 90
                i32.le_s
                i32.and
                (if
                    (then
                    ;; Convert to lowercase by adding 32
                    local.get $char
                    i32.const 32
                    i32.add
                    local.set $char
                    )
                )

                ;; Store the character in the new string at new_addr + idx + 1 (after length prefix)
                local.get $new_addr
                i32.const 1
                i32.add
                local.get $idx
                i32.add
                local.get $char
                i32.store8

                ;; Increment index
                local.get $idx
                i32.const 1
                i32.add
                local.set $idx

                ;; Continue loop
                br $loop
                )
            )
            )
            ;; Return the address of the new lowercase string
            local.get $new_addr
    )

            ;; concatenate two strings, returning the address of the new string
            (func $concatenate_strings (param $s1 i32) (param $s2 i32) (result i32)
                (local $len1 i32)
                (local $len2 i32)
                (local $addr i32)
                (local $total_len i32)

                ;; get lengths from the length prefixes
                local.get $s1
                call $len
                local.set $len1

                local.get $s2
                call $len
                local.set $len2

                ;; calculate total length needed (lengths + combined content + new length byte)
                local.get $len1
                local.get $len2
                i32.add
                local.tee $total_len
                i32.const 1    ;; add 1 for the length prefix
                i32.add
                call $alloc
                local.set $addr

                ;; store new combined length
                local.get $addr
                local.get $total_len
                i32.store8

                ;; copy first string content (skipping its length prefix)
                local.get $s1
                i32.const 1
                i32.add        ;; skip length byte
                local.get $addr
                i32.const 1
                i32.add        ;; skip length byte
                local.get $len1
                call $mem_cpy

                ;; copy second string content (skipping its length prefix)
                local.get $s2
                i32.const 1
                i32.add        ;; skip length byte
                local.get $addr
                i32.const 1
                i32.add
                local.get $len1
                i32.add        ;; position after first string
                local.get $len2
                call $mem_cpy

                local.get $addr
            )
         
 (func (export "main") 
    (block $block_0        
        (loop $label_0        
        global.get $i
        f64.const 2
        f64.eq
        (if
  (then
    br $block_0
  )
  (else
    )
)
        global.get $a
        f64.const 1
        f64.add
        global.set $a
        global.get $i
        f64.const 1
        f64.add
        global.set $i
        global.get $i
        f64.const 5
        f64.lt
        br_if $label_0
     )
     )
    global.get $a
    call $print_number
return )
            (export "allocate" (func $alloc))
            (export "concatenate_strings" (func $concatenate_strings))
        )