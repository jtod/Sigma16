-- Sigma16: ALUrun.hs
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

---------------------------------------------------------------------------
-- Simulation driver and test data for ALU
---------------------------------------------------------------------------

module Main where
import HDL.Hydra.Core.Lib
import ALU

main = sim_alu 16 alu_input1

alu_input1 :: [[Int]]
alu_input1 =
--  a  b  c  d     x      y        Operation  Result
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  [[0, 0, 0, 0,    14,    15],   --   x+y       29
   [0, 0, 0, 0,   125,   590],   --   x+y      715
   [0, 0, 0, 0,    49,    15],   --   x+y       64
   [0, 0, 0, 0,    21,   -19],   --   x+y        2
   [0, 0, 0, 0,    21,   -35],   --   x+y      -14
   [0, 0, 0, 0,  -350,    75],   --   x+y     -275
   [0, 0, 0, 0,  -420,   -90],   --   x+y     -510

   [0, 1, 0, 0,    49,    15],   --   x-y       34
   [0, 1, 0, 0,    15,    49],   --   x-y      -34

   [1, 0, 0, 0,    39,     0],   --   -x       -39
   [1, 0, 0, 0,    25,    70],   --   -x       -25

   [1, 1, 0, 0,    17,     0],   --   x+1       18
   [1, 1, 0, 0,   193,    52],   --   x+1      194

   [1, 1, 0, 1,   150,   175],   --   x<y        1
   [1, 1, 1, 0,   150,   175],   --   x=y        0
   [1, 1, 1, 1,   150,   175],   --   x>y        0

   [1, 1, 0, 1,     9,     9],   --   x<y        0
   [1, 1, 1, 0,     9,     9],   --   x=y        1
   [1, 1, 1, 1,     9,     9],   --   x<y        0

   [1, 1, 0, 1,    23,    17],   --   x<y        0
   [1, 1, 1, 0,    23,    17],   --   x=y        0
   [1, 1, 1, 1,    23,    17]    --   x>y        1
   ]


---------------------------------------------------------------------------
-- Simulation driver for ALU
---------------------------------------------------------------------------

sim_alu :: Int -> [[Int]] -> IO ()
sim_alu n input = runAllInput input output
  where
    (cout,z) = alu n (a,b,c,d) x y
    a = getbit  input 0
    b = getbit  input 1
    c = getbit  input 2
    d = getbit  input 3
    x = gettc n input 4
    y = gettc n input 5
    output =
      [string "Inputs:  ",
       string " abcd = ", bit a, bit b, bit c, bit d,
       string "\n         x = ", bits x, string " $", binhex x,
       string " (bin:", bindec 5 x, string ")",
       string " (tc: ", bitstc 6 x, string ")",
       string "\n         y = ", bits y, string " $", binhex y,
       string " (bin:", bindec 5 y, string ")",
       string " (tc: ", bitstc 6 y, string ")",
       string "\n       Outputs:  ",
       string "cout=", bit cout,
       string "\n         z = ", bits z, string " $", binhex z,
       string " (bin:", bindec 5 z, string ")",
       string " (tc: ", bitstc 6 z, string ")",
       string "\n"]
