-- Computer Architecture 4, Tutorial 4, Question 1
-- This file is Tut4_4.hs
-- John O'Donnell, February 2010

-- A multiplier functional unit

module Main where
import Hydra
import Hydra.StandardCircuits

------------------------------------------------------------------------
-- Main program
------------------------------------------------------------------------

main :: IO ()
main = run_multiply mult_test_data_1

------------------------------------------------------------------------
-- Circuit specification
------------------------------------------------------------------------

multiply :: Clocked a       -- synchronous
  => Int                    -- size parameter
  -> a -> [a] -> [a]        -- input signals
  -> (a,[a],[a],[a],[a])    -- output signals

multiply n start x y = (ready,prod,rx,ry,s)
  where
    rx = wlatch n (mux1w start (shr rx) x)
    ry = wlatch (2*n)
             (mux1w start
                (shl ry)
                (fanout n zero ++ y))
    prod = wlatch (2*n)
             (mux1w start
                (mux1w (lsb rx) prod s)
                (fanout (2*n) zero))
    (c,s) = rippleAdd zero (bitslice2 ry prod)
    ready = or2 (inv (orw rx)) (inv (orw ry))

------------------------------------------------------------------------
-- Test data
------------------------------------------------------------------------

mult_test_data_1 :: [[Int]]
mult_test_data_1 =
--     start  x    y
--     ~~~~~~~~~~~~~~
       [[1,  50,  75],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [1, 100, 100],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [1, 100, 100],
        [0,   0,   0],
        [0,   0,   0],
        [1,   2,   3],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0]]


------------------------------------------------------------------------
-- Simulation driver
------------------------------------------------------------------------

run_multiply :: [[Int]] -> IO ()
run_multiply input = run input spec
  where
    n = 8
    start :: Stream Bool
    start = getbit   input 0
    x     = getbin n input 1
    y     = getbin n input 2
    (rdy,prod,rx,ry,s) = multiply n start x y
    spec :: [Format Bool]
    spec =
      [string "Input: ",
       bit start, bindec 4 x, bindec 4 y,
       string "  Output: ",
       bit rdy, bindec 6 prod, bindec 4 rx, bindec 6 ry,
       bindec 6 s]
