-- Sigma16: ALU.hs
-- Copyright (C) 2021 John T. O'Donnell
-- email: john.t.odonnell9@gmail.com
-- License: GNU GPL Version 3 or later
-- See Sigma16/COPYRIGHT.txt, Sigma16/LICENSE.txt

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

-- module M1.Circuit.ALU where
module Circuit.ALU where

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational

{- The ALU calculates a function of word inputs x and y (which are
usually the contents of two registers) and cc (the contents of R15).
It produces a word output r, which is a numeric result (typically
loaded into the destination register), and a comparison result ccnew
which is the new value to be loaded into the condition code register.
The ALU performs addition, subtraction, negation, increment, and
comparision.  The function is determined by two control signals (alua,
alub).

Furthermore, the ALU receives ir_d as an input and uses it to output
cond, which is the bit in cc indexed by ir_d.  This is used by jumpc0
and jumpc1.

Control inputs:
  alua, alub
Data inputs:
  x
  y
  cc
  ir_d
Data outputs:
  r      = function (a,b) x y cc
  ccnew  = compare x y cc
  cond   = R15.ir_d  (use ir_d to select bit from condition code)

The data output r is the result of an arithmetic operation which
is determinted by the control inputs:

| op = (alua,alub,aluc)
| a b c |    r     |
|-------+-----------
| 0 0 0 |   x+y    | 
| 0 0 1 |   x-y    | 
| 0 1 0 |    -x    | 
| 0 1 1 |   x+1    | 
| 1 0 0 |   cmp    |

The condition code flags are:

| bit index | Relation        | Symbol |
|-----------+-----------------+--------|
|         0 | > Int           | g      |
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

alu n (alua,alub,aluc) x y cc d = (sum, ccnew, cond)
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
    noOvfl  = inv intovfl   -- no integer overflow, integer result is ok
-- Relation of integer result to 0
    any1 = orw sum         -- 1 if any bit in sum is 1
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
-- Conditional bit controls conditional jumps            
    cond = indexbit d cc

-- indexbitcs xs: select element at index cs from xs, where index 0 is
-- least significant position; require that length xs = 2 ^ length cs

indexbit :: Bit a => [a] -> [a] -> a
indexbit [] [x] = x
indexbit (c:cs) xs =
  mux1 c (indexbit cs (drop i xs))
         (indexbit cs (take i xs))
  where i = 2 ^ length cs

-- mux4 cs xs: select element at index cs from xs; require that length
-- xs = 2 ^ length cs

muxw :: Bit a => [a] -> [a] -> a
muxw [] [x] = x
muxw (c:cs) xs =
  mux1 c (muxw cs (take i xs))
         (muxw cs (drop i xs))
  where i = 2 ^ length cs


--    comparing = and3 a b (or2 c d)
--    comp_bool = mux2 (c,d) zero lt_tc eq_tc gt_tc
--    comp_word = boolword n comp_bool
--    z = mux1w comparing sum comp_word
