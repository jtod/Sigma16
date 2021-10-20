-- Sigma16: ALUrun.hs
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

---------------------------------------------------------------------------
-- Simulation driver and test data for ALU
---------------------------------------------------------------------------

-- Usage: cd to the M1 directory and enter ghc -e main Circuit/ALUrun

module Main where
import HDL.Hydra.Core.Lib
import Circuit.ALU

{-

Result function

| a b c |    r     |
|-------+-----------
| 0 0 0 |   x+y    | 
| 0 0 1 |   x-y    | 
| 0 1 0 |    -x    | 
| 0 1 1 |   x+1    | 
| 1 0 0 |   cmp    |

Condition code

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

alu_input1 =
--   a  b  c     x      y   cc     Operation  Result
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  [ "0  0  0     14     15   0"    --   x+y       29
  , "0  0  0    125    590   0"    --   x+y      715
  , "0  0  0     49     15   0"    --   x+y       64
  , "0  0  0     21    -19   0"    --   x+y        2
  , "0  0  0     21    -35   0"    --   x+y      -14
  , "0  0  0   -350     75   0"    --   x+y     -275
  , "0  0  0   -420    -90   0"    --   x+y     -510

  , "0  0  1     49     15   0"    --   x-y       34
  , "0  0  1     15     49   0"    --   x-y      -34

  , "0  1  0     39      0   0"    --   -x       -39
  , "0  1  0     25     70   0"    --   -x       -25

  , "0  1  1     17      0   0"    --   x+1       18
  , "0  1  1    193     52   0"    --   x+1      194

  , "1  0  0      5      5   0"    --  cmp     cc = 0004  =
  , "1  0  0      5      7   0"    --  cmp     cc = 0018  <L
  , "1  0  0      7      5   0"    --  cmp     cc = 0003  >G
  , "1  0  0      5     -1   0"    --  cmp     cc = 0009  >L
  , "1  0  0     -1      5   0"    --  cmp     cc = 0012  <G

   ]


---------------------------------------------------------------------------
-- Simulation driver for ALU
---------------------------------------------------------------------------

main = driver  $ do

-- Word size
  let n =  16

-- Input data
  useData alu_input1

  in_a <- inPortBit "a"
  in_b <- inPortBit "b"
  in_c <- inPortBit "c"
  in_x <- inPortWord "x" n
  in_y <- inPortWord "y" n
  in_cc <- inPortWord "cc" n

  let a = inbsig in_a
  let b = inbsig in_b
  let c = inbsig in_c
  let x = inwsig in_x
  let y = inwsig in_y
  let cc = inwsig in_cc

-- Circuit  
  let (r,ccnew) = alu n (a,b,c) x y cc

  format
      [string "Inputs:  ",
       string " abc = ", bit a, bit b, bit c,
       string "\n         x = ", bits x, string " $", binhex x,
       string " (bin:", bindec 5 x, string ")",
       string " (tc: ", bitstc 6 x, string ")",
       string "\n         y = ", bits y, string " $", binhex y,
       string " (bin:", bindec 5 y, string ")",
       string " (tc: ", bitstc 6 y, string ")",
       string "\n       Outputs:  ",
       string "\n         r = ", bits r, tcdec 5 r, string "  $", binhex r,
       string "\n         ccnew = ", bits ccnew, string " $", binhex ccnew,
       string "\n"]

  runSimulation
