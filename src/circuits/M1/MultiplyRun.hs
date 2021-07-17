-- Sigma16: MultiplyRun.hs
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

------------------------------------------------------------------------
-- Multiplier simulation driver
------------------------------------------------------------------------

module Main where
import HDL.Hydra.Core.Lib
import Multiply

------------------------------------------------------------------------
-- Main program to run multiplier on test data

main :: IO ()
main =
  do run_multiply mult_test_data_1

------------------------------------------------------------------------
-- Test data

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
-- Simulation driver

run_multiply :: [[Int]] -> IO ()
run_multiply input = runAllInput input output
  where
-- Size parameter
    k = 8

-- Extract input signals from readable input
    start = getbit   input 0
    x     = getbin k input 1
    y     = getbin k input 2

-- Connect the circuit to its inputs and outputs
    (rdy,prod,rx,ry,s) = multiply k start x y

-- Format the signals for output
    output =
      [string "Input: ",
       bit start, bindec 4 x, bindec 4 y,
       string "  Output: ",
       bit rdy, bindec 6 prod, bindec 4 rx, bindec 6 ry,
       bindec 6 s]
