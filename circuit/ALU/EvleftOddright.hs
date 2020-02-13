module Main where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.StandardCircuits

{- A combinational circuit that takes an 8-bit input word,
representing a binary number.  It produces a 4-bit output word.  If
the input number is even, then the output is the left half of the
input word; otherwise it is the right half. -}

main = sim input1

-- Test data, consisting of one binary number for each clock cycle.

input1 =
  [[37],
   [38],
   [209],
   [210]]

-- Specification of the circuit

select_half :: Signal a => [a] -> [a]
select_half x = mux1w (lsb x) (field x 0 4) (field x 4 4)

-- Simulation driver

sim input = run input format
  where
    x = getbin 8 input 0
    y = select_half x
    format :: [Format Bool Bool]
    format =
      [string "Input x = ", bindec 4 x,
       string " (hex ", hex x, string ")",
       string "  Output y = ", bindec 2 y,
       string " (hex ", hex y, string ")"]
