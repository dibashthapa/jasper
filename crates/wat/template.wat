(module 
    (import "" "" (func $print_str(param i32)))
    (import "" "" (func $print_number(param i32)))
    (import "" "" (func $print_float(param f64)))
    (memory $m 3)
    (export "memory" (memory $m))

    {globals}
    {data_sections}
  
      (func $len (param $addr i32) (result i32)
          local.get $addr
          call $nullthrow
          i32.load8_u ;; changed to load8_u since we're storing single byte lengths
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

      (func $uppercase(param $addr i32) (result i32)
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

          ;; Check if the character is lowercase (ASCII 65 to 90)
          local.get $char
          i32.const 97
          i32.ge_s
          local.get $char
          i32.const 122
          i32.le_s
          i32.and
          (if
              (then
              ;; Convert to lowercase by adding 32
              local.get $char
              i32.const 32
              i32.sub
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

      
(func $lowercase (param $addr i32) (result i32)
    (local $len i32)
    (local $idx i32)
    (local $char i32)
    (local $new_addr i32)
    (local $is_final_sigma i32)

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

    ;; Initialize context-sensitive state
    i32.const 0
    local.set $is_final_sigma

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

                ;; Unicode lowercase mapping
                local.get $char
                i32.const 913  ;; Uppercase Sigma (U+03A3)
                i32.eq
                (if
                    (then
                        ;; Check for final sigma rules
                        local.get $is_final_sigma
                        i32.eqz
                        (if
                            (then
                                ;; Convert to lowercase sigma (U+03C3)
                                local.get $char
                                i32.const 32  ;; Difference between U+03A3 and U+03C3
                                i32.add
                                local.set $char
                            )
                            (else
                                ;; Convert to final sigma (U+03C2)
                                local.get $char
                                i32.const 1  ;; Difference between U+03C3 and U+03C2
                                i32.sub
                                local.set $char
                            )
                        )

                        ;; Reset final sigma state
                        i32.const 0
                        local.set $is_final_sigma
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


(func $get_char (param $addr i32) (param $idx i32) (result i32)
  local.get $addr 
  local.get $idx
  i32.const 1
  i32.add
  i32.add
  i32.load8_u
)


(func $substring (param $s i32) (param $start i32) (param $end i32)
  (result i32) 
  (local $addr i32)
  (local $i i32)
  (local $len i32)
  (local $total_len i32)


  ;; Calculate the total length
  local.get $end
  local.get $start
  i32.sub
  local.tee $total_len
  i32.const 1 
  i32.add 
  call $alloc 
  local.set $addr

  ;; Get the length of the input string
  local.get $s
  call $len
  local.set $len

  ;; Validate the provided start and end indices
  local.get $start
  local.get $len
  i32.lt_u
  i32.eqz
  if
      ;; Start index out of bounds, return empty string
      local.get $len
      call $alloc
      return
  end
  local.get $end
  local.get $len
  i32.lt_u
  i32.eqz
  if
      ;; End index out of bounds, use the maximum length
      local.get $len
      local.set $end
  end

  ;; Store new combined length
  local.get $addr 
  local.get $total_len 
  i32.store8


  ;; Copy the substring characters to the new memory
  local.get $s
  i32.const 1
  i32.add 
  local.tee $s
  local.get $start
  i32.add
  local.get $addr
  i32.const 1 
  i32.add
  local.tee $addr
  local.get $total_len
  call $mem_cpy

  ;; Return the address of the new substring
  local.get $addr
  i32.const 1 
  i32.sub 
  local.tee $addr
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

(func $compare(param i32 i32) (result i32)
  (local $len1 i32)
  (local $len2 i32)
  (local $index i32)
  (local $result i32)
  (local $char1 i32)
  (local $char2 i32)

  i32.const 1
  local.set $result ;; Defaults to false

  local.get 0 ;; Get Left String
  call $len 
  local.set $len1 

  local.get 1  ;; Get Right String
  call $len 
  local.set $len2

  ;; Check if $len == $len2
  local.get $len1
  local.get $len2
  i32.ne 

  if 
    i32.const 0 
    return 
  end
  
  i32.const 0 
  local.set $index 


(block $block 
    (loop $l
            local.get $index 
            local.get $len1 
            i32.ge_u
            br_if $block 

            local.get 0
            local.get $index
            call $get_char 
            local.set $char1

            local.get 1
            local.get $index 
            call $get_char
            local.set $char2

            local.get $char1
            local.get $char2
            i32.ne 
            if 
                i32.const 0 
                local.set $result 
                br $block
            end

            local.get $index 
            i32.const 1
            i32.add 
            local.set $index 
            br $l

        )
        )

  local.get $result
  
  )

  (func $indexOf (param $s i32)  (param $search_value i32) (param $from_index i32)
    (result i32)
    (local $len i32)
    (local $searchLen i32)
    (local $i i32)

    ;; Get string length
    local.get $s
    call $len
    local.set $len 


    ;; Handle empty search string
    local.get $search_value
    call $len
    local.tee $searchLen
    ;; If searchLen is zero
    i32.eqz
    if
      local.get $from_index 
      local.get $len 
      i32.le_u
      if
        local.get $from_index 
        return
      end
    end


    ;; Initialize loop counter
    local.get $from_index
    local.set $i

    (block $exit
      (loop $l
            ;; Check if we should continue searching
            local.get $i ;; 0
            local.get $len ;; 5
            local.get $searchLen ;; 1
            i32.sub ;; 4
            i32.gt_s
            br_if $exit  ;; Exit if i > len-searchLen

            ;; Get substring and compare
            local.get $s ;; "hello"
            local.get $i 
            local.get $i 
            local.get $searchLen ;; 0 + 1 = 1
            i32.add
            call $substring ;; Hello, 0, 1 ;; returns h
            local.get $search_value ;; 'l'
            call $compare ;; 0
            i32.const 1
            i32.eq
            if
                local.get $i
                return 
            end

            ;; Increment and continue
            local.get $i
            i32.const 1
            i32.add
            local.set $i
            br $l
      )
    )

    ;; No match found
    i32.const -1
    )

  (func (export "main")
      {main_func}
      return
    )
  (export "allocate" (func $alloc))
  (export "concatenate_strings" (func $concatenate_strings))

  )
