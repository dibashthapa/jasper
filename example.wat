(module (global $x (mut f64) (f64.const 5))
 (func (export "main") (result f64)
    f64.const 5
    global.set $x
    global.get $x
    f64.const 5
    f64.eq
    (if 
      (then 
         global.get $x
         f64.const 5
         f64.sub
         global.set $x
      )
      (else 
         global.get $x
         f64.const 6
         f64.lt
         (if 
      (then 
         global.get $x
         f64.const 4
         f64.sub
         global.set $x
      )
      (else 
      )
    )
      )
    )
    global.get $x
  )
)