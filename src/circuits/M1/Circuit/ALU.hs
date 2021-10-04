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

module M1.Circuit.ALU where

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
| a b c |    r     | ccnew |
|-------+------------
| 0 0 0 |   x+y    | 
| 0 0 1 |   x-y    | 
| 0 1 0 |    -x    | 
| 0 1 1 |   x+1    | 
| 1 0 0 |   cmp    |

The control algorithm defines all control signals, regardless of what
operation is being performed.  The signal values for the ALU are:

  alu_add = 00
  alu_sub = 01
  alu_neg = 10
  alu_inc = 11
-}

alu n (alua,alub,aluc) x y cc d = (sum, ccnew, cond)
  where
    negating = and2 (inv alua) (xor2 alub aluc)  -- alu abc = 001 or 010
    arith = inv alua  -- doing arithmetic operation, alu abc one of 000 001 010 100
    comparing = and3 alua (inv alub) (inv aluc)  -- doing comparison, alu abc = 100
    wzero = fanout n zero
    wone = boolword n one
-- prepare inputs to adder    
    x' = mux2w (alub,aluc) x x wzero x
    y' = mux2w (alub,aluc) y (invw y) (invw x) wone
    xy = bitslice2 x' y'
    (carry,sum) = rippleAdd negating xy
-- binary comparison    
    (lt,eq,gt) = rippleCmp xy
-- two's complement comparison    
    lt_tc = mux2 (xy!!0) lt zero one lt
    eq_tc = eq
    gt_tc = mux2 (xy!!0) gt one zero gt
-- carry and overflow
    natovfl = carry
    intovfl = zero -- ????
-- condition code flags
    fcarry = and2 carry (inv comparing)
    fnatovfl = and2 natovfl (inv comparing)
    fintovfl = and2 intovfl (inv comparing)
    ccnew = [ zero,   zero,     zero,     zero,   -- bit 15 14 13 12
              zero,   zero,     zero,     zero,   -- bit 11 10  9  8
              fcarry, fnatovfl, fintovfl, lt_tc,  -- bit  7  6  5  4
              lt,     eq,       gt,       gt_tc   -- bit  3  2  1  0
            ]
-- conditional bit controls conditional jumps            
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


{-
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

--    comparing = and3 a b (or2 c d)
--    comp_bool = mux2 (c,d) zero lt_tc eq_tc gt_tc
--    comp_word = boolword n comp_bool
--    z = mux1w comparing sum comp_word
