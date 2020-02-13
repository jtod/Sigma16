---------------------------------------------------------------------------
-- Simulation driver and test data for ALU
-- Solution for Problem Set
---------------------------------------------------------------------------

module Main where
import HDL.Hydra.Core.Lib
import ALU

main = sim_alu 16 alu_input1

alu_input1 :: [[Int]]
alu_input1 =
--  a  b  c  d     x      y        Operation  Result
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Solutions for the problem set follow:

  [[0, 0, 0, 0,    13,    43],   --   x+y       56
   [0, 0, 0, 0,    19,    -2],   --   x+y       17
   [0, 1, 0, 0,    50,     5],   --   x-y       45
   [0, 1, 0, 0,    50,    -5],   --   x-y       55
   [1, 0, 0, 0,    20,     0],   --   -x       -20
   [1, 0, 0, 0,   -20,     0],   --   -x        20
   [1, 1, 0, 0,    21,     0],   --   x+1       22
   [1, 1, 0, 1,    3,      5],   --   x<y        1
   [1, 1, 0, 1,    3,     -5],   --   x<y        0
   [1, 1, 0, 1,   -3,      5],   --   x<y        1
   [1, 1, 0, 1,   -3,     -5]    --   x<y        0
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
