(module (global $x (mut f64) (f64.const 1))
 (func (export "main") (result f64)
    f64.const 0
    global.set $x 
    (loop $label_0
     global.get $x
     f64.const 1
     f64.add
     global.set $x 
     global.get $x
     f64.const 10
     f64.lt
     br_if $label_0    
   )
    global.get $x
    f64.const 1
    f64.sub
  )
)