(module
  (global $i (mut f64) (f64.const 0))  ;; i = 0
  (global $a (mut f64) (f64.const 5))  ;; a = 5

  (func (export "main") (result f64)
    ;; Initialize `a` to 5
    f64.const 5
    global.set $a
    
    ;; Initialize `i` to 0
    f64.const 0
    global.set $i

    ;; Start the outer block to enable breaking
    (block $block_0
      (loop $label_0
        ;; Check if i == 2 (if so, break)
        global.get $i
        f64.const 2
        f64.eq
        (if
          (then br $block_0)
        )

        ;; Increment a (a = a + 1)
        global.get $a
        f64.const 1
        f64.add
        global.set $a

        ;; Increment i (i = i + 1)
        global.get $i
        f64.const 1
        f64.add
        global.set $i

        ;; Check if i < 5, if so, repeat the loop
        global.get $i
        f64.const 5
        f64.lt
        br_if $label_0
      )
    )
    
    ;; Return the value of a
    global.get $a
  )
)
