-- Computer Architecture 4, Tutorial 3, Question 2
-- This file is Tut3_2.hs
-- John O'Donnell, 29 January 2010

module Main where
import Hydra
import Hydra.StandardCircuits

{- Statement of the problem

Design and test a combinational circuit that takes an 8-bit two’s
complement integer, and outputs its absolute value as an 8-bit binary
integer.

To compile and execute:
  ghc --make Tut3_2
  ./Tut3_2

 -}

-- The main program runs the simulation using the test data input1.

main = sim input1

-- Test data, consisting of one binary number for each clock cycle.

input1 =
  [[  37],
   [   0],
   [ -29],
   [  13],
   [ -13],
   [ 125],
   [-125],
   [   1],
   [  -1],
   [-128],
   [ 127],
   [   0]]

-- Specification of the circuit

absval :: Signal a => [a] -> [a]
absval x = y
  where
    y = mux1w (x!!0) x s
    (c,s) = rippleAdd one (bitslice2 (winv x) (fanout 8 zero))

-- Simulation driver

sim input = run input format
  where
    x = gettc 8 input 0
    y = absval x
    format :: [Format Bool]
    format =
      [string "Input x = ", tcdec 4 x,
       string " (hex ", hex x, string ")",
       string "  Output y = ", bindec 3 y,
       string " (hex ", hex y, string ")"]
