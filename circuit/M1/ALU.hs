-- Sigma16: ALU.hs
-- Copyright (C) 2020 John T. O'Donnell
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

module ALU where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational

{- The ALU performs addition, subtraction, negation, increment, and
comparision for <, =, and >.  It receives four control signals
(a,b,c,d) that determine which function it will compute.  These are
shown in the following table, where don't-care values are left blank.

  |~~~~~~~~~~~~~~~~~~~~~~~~|
  | Function |  a,b   c,d  |
  |~~~~~~~~~~|~~~~~~~~~~~~~|
  |   x+y    |  0 0        |
  |   x-y    |  0 1        |
  |    -x    |  1 0        |
  |   x+1    |  1 1   0 0  |
  |   x<y    |  1 1   0 1  |
  |   x=y    |  1 1   1 0  |
  |   x>y    |  1 1   1 1  |
  ~~~~~~~~~~~~~~~~~~~~~~~~~~

The control algorithm defines all control signals, regardless of what
operation is being performed.  The signal values for the ALU are:

  alu_add = 0000
  alu_sub = 0100
  alu_neg = 1000
  alu_inc = 1100
  alu_lt  = 1101
  alu_eq  = 1110
  alu_gt  = 1111
-}

alu n (a,b,c,d) x y = (cout,z)
  where
    wzero = fanout n zero
    wone = boolword n one
    negating = xor2 a b
    comparing = and3 a b (or2 c d)
    x' = mux2w (a,b) x x wzero x
    y' = mux2w (a,b) y (invw y) (invw x) (mux1w (or2 c d) wone y)
    xy = bitslice2 x' y'
    (cout,sum) = rippleAdd negating xy
    (lt,eq,gt) = rippleCmp xy
    lt_tc = mux2 (xy!!0) lt zero one lt
    eq_tc = eq
    gt_tc = mux2 (xy!!0) gt one zero gt
    comp_bool = mux2 (c,d) zero lt_tc eq_tc gt_tc
    comp_word = boolword n comp_bool
    z = mux1w comparing sum comp_word

