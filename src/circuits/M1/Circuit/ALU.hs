-- Sigma16 M1 circuit: ALU.hs
-- John T. O'Donnell, 2021. Sigma16/COPYRIGHT.txt, Sigma16/LICENSE.txt

-- This file is part of Sigma16.  Sigma16 is free software: you can
-- redistribute it and/or modify it under the terms of the GNU General
-- Public License as published by the Free Software Foundation, either
-- version 3 of the License, or (at your option) any later version.
-- Sigma16 is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.  You should have received
-- a copy of the GNU General Public License along with Sigma16.  If
-- not, see <https://www.gnu.org/licenses/>.

------------------------------------------------------------------------
--			Arithmetic/Logic Unit
------------------------------------------------------------------------

module Circuit.ALU where

-- Arithmetic and logic unit for the M1 processor circuit, which implements
-- the core subset of Sigma16.

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational

{- The ALU calculates a function of word inputs x and y (which are
usually the contents of two registers) and the condition code cc (the
contents of R15).

(The Core architecture doesn't need the cc as an input to the ALU, but
the more advanced instructions do, and cc is provided as an input to
simplify future zetension of M1.)

The ALU produces a word output r, which is a
numeric result (typically loaded into the destination register), and a
comparison result ccnew which is the new value to be loaded into the
condition code register.  The ALU performs addition, subtraction,
negation, increment, and comparision.  The function is determined by
control signals (alua, alub, aluc).

Control inputs:
   alua, alub, aluc
Data inputs:
   x
   y
   cc
Data outputs:
   r      = function (a,b,c) x y cc
   ccnew  = compare x y cc

The data output r is the result of an arithmetic operation which is
determinted by the control inputs:

| op = (alua,alub,aluc)
| a b c |    r     |
|-------+-----------
| 0 0 0 |   x+y    | 
| 0 0 1 |   x-y    | 
| 0 1 0 |    -x    | 
| 0 1 1 |   x+1    | 
| 1 0 0 |   cmp    |

The condition code flags are as follows.  Sigma16 indexes bits
starting from 0 in the rightmost (least significant) position, so the
flags cluster toward the right end of a word.  (Note: The Hydra !!
operator indexes bits from the left.)

| bit index | Relation        | Symbol |
|-----------+-----------------+--------|
|         0 | > Int           | >      |
|         1 | > Nat           | G      |
|         2 | =               | =      |
|         3 | < Nat           | L      |
|         4 | < Int           | <      |
|         5 | Int overflow    | v      |
|         6 | Nat overflow    | V      |
|         7 | Carry           | C      |
|         8 | Stack overflow  | S      |
|         9 | Stack underflow | s      |
-}          

alu n (alua,alub,aluc) x y cc = (sum, ccnew)
  where

-- Constant words    
    wzero = fanout n zero
    wone = boolword n one

-- Determine type of function being evaluated
    arith = inv alua  -- doing arithmetic operation, alu abc one of 000 001 010 100
    negating = and2 (inv alua) (xor2 alub aluc)  -- alu abc = 001 or 010
    comparing = and3 alua (inv alub) (inv aluc)  -- doing comparison, alu abc = 100

-- Prepare inputs to adder    
    x' = mux2w (alub,aluc) x x wzero x
    y' = mux2w (alub,aluc) y (invw y) (invw x) wone

-- The adder    
    xy = bitslice2 x' y'
    (carry,sum) = rippleAdd negating xy
    msb = sum!!0 --- most significant bit of sum

-- Binary comparison    
    (lt,eq,gt) = rippleCmp xy

-- Two's complement comparison    
    lt_tc = mux2 (xy!!0) lt zero one lt
    eq_tc = eq
    gt_tc = mux2 (xy!!0) gt one zero gt

-- Carry and overflow
    natovfl = carry           -- natural (binary) overflow if carry = 1
    intovfl = xor2 carry msb  -- integer (2's comp) overflow if carry != msb
    noOvfl  = inv intovfl     -- no integer overflow, integer result is ok

-- Relation of integer result to 0
    any1 = orw sum                     -- 1 if any bit in sum is 1
    neg  = and3 noOvfl any1 msb        -- ok, result < 0
    z    = and2 noOvfl (inv any1)      -- ok, result is 0
    pos  = and3 noOvfl any1 (inv msb)  -- ok, result > 0

-- Overflow flags:  don't indicate overflow for a comparison operation
    fcarry   = and2 arith carry
    fnatovfl = and2 arith natovfl
    fintovfl = and2 arith intovfl

-- Comparison flags: for arithmetic, indicate comparison with 0
    flt      = mux1 arith lt    zero
    flt_tc   = mux1 arith lt_tc neg
    feq      = mux1 arith eq    z
    fgt      = mux1 arith gt    pos
    fgt_tc   = mux1 arith gt_tc pos

-- Generate the condition code
    ccnew = [ zero,   zero,     zero,     zero,    -- bit 15 14 13 12
              zero,   zero,     zero,     zero,    -- bit 11 10  9  8
              fcarry, fnatovfl, fintovfl, flt_tc,  -- bit  7  6  5  4
              flt,    feq,      fgt,      fgt_tc   -- bit  3  2  1  0
            ]
