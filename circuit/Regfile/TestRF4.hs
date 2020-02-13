module Main where

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.StandardCircuits
import RF4

test_data_1 :: [[Int]]
test_data_1 =
  [[ 1,  7, 2, 1, 2],      -- R2 := 7    show R1, R2
   [ 0,  0, 0, 0, 1],      --            show R0, R1
   [ 0,  0, 0, 2, 3],      --            show R2, R3

   [ 1, 10, 1, 1, 2],      -- R1 := 10   show R1, R2
   [ 0,  0, 0, 0, 1],      --            show R0, R1
   [ 0,  0, 0, 2, 3],      --            show R2, R3

   [ 1,  3, 0, 0, 1],      -- R0 := 3    show R0, R1
   [ 0,  0, 0, 0, 1],      --            show R0, R1
   [ 0,  0, 0, 2, 3],      --            show R2, R3

   [ 0,  0, 0, 0, 0]]

main ::IO ()
main = runRF4 test_data_1

runRF4 :: [[Int]] -> IO ()
runRF4 input = runAllInput input output
  where
    ld  = getbit   input 0
    x   = getbin 4 input 1
    d   = getbit2  input 2
    sa  = getbit2  input 3
    sb  = getbit2  input 4
    (d0,d1) = d
    (sa0,sa1) = sa
    (sb0,sb1) = sb

    (abus,bbus) = rf4 ld x d sa sb

    output =
      [string "ld=", bit ld,
       string "  x=", hex x,
       string "  d=", bit d0, bit d1,
       string "  sa=", bit sa0, bit sa1,
       string "  sb=", bit sb0, bit sb1,
       string "     Output: ",
       string "  abus=", hex abus,
       string "  bbus=", hex bbus]
