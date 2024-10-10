(module 
 (memory 1) 
 (global $i (mut f64) (f64.const 0))
(global $a (mut f64) (f64.const 5))
 (func (export "main") (result f64)
    f64.const 5
    global.set $a
    f64.const 0
    global.set $i
    (block $block_0 
         (loop $label_0 
         global.get $i
         f64.const 2
         f64.eq
         (if
(then br $block_0
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
    )
)