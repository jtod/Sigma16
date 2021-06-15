(module
 (import "imports" "wam" (memory 3 5 shared))
 (func $i (import "imports" "printnum") (param i32))
 ;; addplups1 (x,y) => x+y+1
 (func (export "addplus1") (param $x i32) (param $y i32) (result i32)
       local.get $x
       local.get $y
       i32.add
       i32.const 1
       i32.add)
 (func (export "print42")
       i32.const 42
       call $i)
 )

;; (module
;;  (func (result i32)
;;    (i32.const 42)
;;  )
;;  (export "helloWorld" (func 0))
;; )
