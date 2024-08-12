(module (global $i (mut f64) (f64.const 1))
(global $x (mut f64) (f64.const 5))
 (func (export "main") (result f64)
    f64.const 5
    global.set $x
    f64.const 0
    global.set $i
    (loop $label_0        
        global.get $x
        f64.const 3
        f64.mul
        global.set $x
        global.get $i
        f64.const 1
        f64.add
        global.set $i
        global.get $i
        f64.const 2
        f64.lt
        br_if $label_0
     )
    global.get $x
  )
)