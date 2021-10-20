-- MultiplyRun: simulation driver for multiply circuit
-- John O'Donnell, 2021

-- To run the multiplier:
--   $ ghci
--   ghci> :load MultiplyRun
--   ghci> :main
--   hydra> run

module Main where
import HDL.Hydra.Core.Lib
import Circuit.Multiply

mult_test_data_1 :: [String]
mult_test_data_1 =
--     start  x    y
--     ~~~~~~~~~~~~~~
       ["1    50   75",  -- start=1 to multiply 50 * 75, expect 3750
        "0    0     0",  -- start=0 to give circuit time
        "0    0     0",  -- a number of clock cycles are needed
        "0    0     0",  -- give it another cycle
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "1  100   100",  -- start=1 to multiply 100*100, expect 10000
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "1  100   100",  -- start=1 to multiply 100*100, expect 10000
        "0    0     0",  -- working on 100*100
        "0    0     0",  -- working on 100*100
        "1    2     3",  -- abort and start multiplying 2*3
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0"]

-- main :: Driver a
main :: IO ()
main = driver $ do

-- Input data  
  useData mult_test_data_1

-- Size parameter
  let k = 8

-- Input ports
  in_start <- inPortBit "start"
  in_x     <- inPortWord "x" k
  in_y     <- inPortWord "y" k

-- Input signals
  let start = inbsig in_start
  let x     = inwsig in_x
  let y     = inwsig in_y

-- Circuit
  let (rdy,prod,rx,ry,s) = multiply k start x y

-- Format the signals
  format
    [string "Input: ",
     bit start, bindec 4 x, bindec 4 y,
     string "  Output: ",
     bit rdy, bindec 6 prod, bindec 4 rx, bindec 6 ry,
     bindec 6 s,
     string "\n"]

  runSimulation
