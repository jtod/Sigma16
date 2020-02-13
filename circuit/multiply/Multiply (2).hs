------------------------------------------------------------------------
-- Multiplier circuit
------------------------------------------------------------------------

module Multiply where
import Hydra
import Hydra.StandardCircuits

{- Definition of a sequential functional unit that multiples two
binary integers, along with test data and simulation driver.

The circuit is a functional unit, which uses a start control signal to
initiate a multiplication and produces a ready output signal to
indicate completion.

The multiplier circuit uses the sequential shift-and-add algorithm to
multiply two k-bit binary numbers, producing a 2k-bit product.  The
specification is general, taking a size parameter k::Int.  The start
control signal tells the multiplier to begin computing the product
x*y, and any other multiplication in progress (if any) is aborted.  In
order to make the simulation output more interesting, the multiplier
outputs its internal register and sum values as well as the ready
signal and the product.

To run the multiplier on some sample test data, defined below, enter:

  hydra-ghci
  :load MultiplyTest
  main

With this test data defined below, the circuit will perform several
multiplications, running some to completion and interrupting
another. -}

------------------------------------------------------------------------

multiply :: Clocked a => Int
  -> a -> [a] -> [a] -> (a,[a],[a],[a],[a])

multiply k start x y = (ready,prod,rx,ry,s)
  where
    rx = wlatch k (mux1w start (shr rx) x)
    ry = wlatch (2*k)
             (mux1w start
                (shl ry)
                (fanout k zero ++ y))
    prod = wlatch (2*k)
             (mux1w start
                (mux1w (lsb rx) prod s)
                (fanout (2*k) zero))
    (c,s) = rippleAdd zero (bitslice2 ry prod)
    ready = or2 (inv (orw rx)) (inv (orw ry))


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
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0]]


------------------------------------------------------------------------
-- Main program to run multiplier on test data

main :: IO ()
main = run_multiply mult_test_data_1

-- Simulation driver

run_multiply :: [[Int]] -> IO ()
run_multiply input = run input spec
  where
    k = 8
    start :: Stream Bool
    start = getbit   input 0
    x     = getbin k input 1
    y     = getbin k input 2
    (rdy,prod,rx,ry,s) = multiply k start x y
    spec :: [Format Bool]
    spec =
      [string "Input: ",
       bit start, bindec 4 x, bindec 4 y,
       string "  Output: ",
       bit rdy, bindec 6 prod, bindec 4 rx, bindec 6 ry,
       bindec 6 s]
