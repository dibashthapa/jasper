(module (global $x (mut f64) (f64.const 10))
 (func (export "main") (result f64)
    f64.const 10
    global.set $x 
    global.get $x
    f64.const 1
    f64.sub
    drop
    f64.const 100
    f64.const 20
    f64.add
    f64.const 3
    f64.mul
    f64.const 2
    f64.div
    f64.const 4
    f64.mul
  )
)