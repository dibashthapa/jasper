(module (global $x (mut f64) (f64.const 4))
 (func (export "main") (result f64)
    f64.const 4
    global.set $x
  )
)