(module
 (func $i (import "imports" "fooprint") (param i32))
 (func (export "exported_func")
       i32.const 42
       call $i)
 )

;; (module
;;  (func (result i32)
;;    (i32.const 42)
;;  )
;;  (export "helloWorld" (func 0))
;; )
