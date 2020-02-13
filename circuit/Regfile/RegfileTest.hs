------------------------------------------------------------------------
-- Register file simulation driver
------------------------------------------------------------------------

{- RegfileTest.hs --- simulation driver and test data for register
file circuit, which is defined in HydraLib.StandardCircuits.  To
compile and run:
  ghc --make RegfileTest
  ./RegfileTest
-}

module Main where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.StandardCircuits

------------------------------------------------------------------------
-- Main program to run multiplier on test data

main :: IO ()
main = run_regfile testdata1

------------------------------------------------------------------------
-- Test data

{- This test data is suitable for word size n=16 and array size k=4.
That is, it uses 2^4 registers, each containing a 16-bit word. -}

testdata1 :: [[Int]]
testdata1 =
-----------------------------------------------------
--      ld   d  sa  sb   x         effect       a   b
-----------------------------------------------------
       [[1,  3, 12,  2, 204],  -- R3  := 204    0   0
        [1, 12, 12,  3,  74],  -- R12 :=  74    0 204
        [0,  3, 12,  3, 199],  --              74 204
        [0,  0,  0,  0,   0]]  -- R0 := 0

------------------------------------------------------------------------
-- Simulation driver

run_regfile :: [[Int]] -> IO ()
run_regfile input =
  do putStrLn "\nrun_regfile"
     runThroughInput input output
  where
    n = 16
    k = 8
    ld = getbit   input 0
    d  = getbin k input 1
    sa = getbin k input 2
    sb = getbin k input 3
    x  = getbin n input 4
    (a,b) = regfile n k ld d sa sb x
    output =
      [string "Input: ",
       string "ld = ", bit ld,
       string "d = ", bindec 2 d,
       string "sa = ", bindec 2 sa,
       string "sb = ", bindec 2 sb,
       string "   Output: ",
       string " a = ", bindec 4 a,
       string " a = ", bindec 4 b]
