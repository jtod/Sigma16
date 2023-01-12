;; Sigma16: emcore.wat
;; Copyright (C) 2023 John T. O'Donnell.  License: GNU GPL Version 3 or later
;; See Sigma16/README, LICENSE, and https://jtod.github.io/home/Sigma16

;; This file is part of Sigma16.  Sigma16 is free software: you can
;; redistribute it and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.
;; Sigma16 is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.  You should have received
;; a copy of the GNU General Public License along with Sigma16.  If
;; not, see <https://www.gnu.org/licenses/>.

;; emcore.wat is pure emulator: for faster execution, it doesn't log
;; accesses to registers or memory

(module
 (import "imports" "wam" (memory 3 40 shared))
 (func $i (import "imports" "printnum") (param i32))

 ;; addplups1 (x,y) => x+y+1
 (func (export "addplus1")
       (param $x i32) (param $y i32) (result i32)
       local.get $x
       local.get $y
       i32.add
       i32.const 1
       i32.add)

 (func (export "print42")
       i32.const 42
       call $i)

)

;; ------------------------------------------------------------------------

;; readReg16 (r).  reg[r] := x
;;  (func (export "readReg16") (param $r i32)
;;        local.get $r
;;        local.get $x
;;        i32.store16)
;;  )

;;       call $i
;;       call $i
;;       i32.const 2
;;       i32.const 7
;;       i32.const 2
;;       i32.load
;;       call $i

;; (module
;;  (func (result i32)
;;    (i32.const 42)
;;  )
;;  (export "helloWorld" (func 0))
;; )
