-- Reg1RunData: simulate a 1-bit register circuit with pre-specified input data
-- Copyright (C) 2021 John T. O'Donnell.  This file is part of Sigma16.
-- See Sigma16/README and https://jtod.github.io/home/Sigma16/

-- This is an example of how to simulate a small circuit using
-- pre-defined input data, not reading the inputs from the user.  See
-- the User Guide and Sigma16/src/circuits/README.

-- cd Sigma16/src/circuits   -- must be in this directory
-- ghci                      -- start ghci and initialize using .ghci
-- :load Circuits/Reg1Run    -- load the simulation driver
-- :main                     -- run it: launch the main program

module Main where
import HDL.Hydra.Core.Lib   -- the hardware description language
import Circuits.Reg1        -- definition of the circuit to be simulated
import Control.Monad.State

inputs, alwaysLoad1, alwaysLoad0, idle :: [String]
inputs = [ "1 1", "0 0", "0 0", "1 0", "0 0", "0 1", "1 1", "0 1", "0 0"]
alwaysLoad1 = ["1 1", "1 1", "1 1", "1 1", "1 1", "1 1"]
alwaysLoad0 = ["1 0", "1 0","1 0","1 0","1 0","1 0","1 0"]
idle = ["0 0", "0 0", "0 0", "0 0", "0 0", "0 0"]

main :: Driver a
main = driver $ do

  initialize
  testInputLists
  storeInputList "data" inputs
  selectInputList "data"
  
  -- Input ports
  in_ld <- inPortBit "ld"
  in_x  <- inPortBit "x"

  -- Input signals
  let ld = inbsig in_ld
  let x  = inbsig in_x
  
  -- The circuit myreg1 receives input signals and defines output signals
  let (r,q) = myreg1 ld x

  -- The output ports are interfaces to the output signals
  out_r <- outPortBit "r" r
  out_q <- outPortBit "q" q
  
  -- Run interactive simulation 
  runSimulation
  
