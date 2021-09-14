-- RegRun: simulate a word register circuit
-- Copyright (C) 2021 John T. O'Donnell.  This file is part of Sigma16.
-- See Sigma16/README and https://jtod.github.io/home/Sigma16/

-- This is an example of how to simulate a small circuit
-- interactively, where the circuit (myreg) uses words as well as
-- individual bits

--   cd Sigma16/src/circuits   -- must be in this directory
--   ghci                      -- start ghci and initialize using .ghci
--   :load Circuits/RegRun     -- load the simulation driver
--   :main                     -- run it: launch the main program

module Main where
import HDL.Hydra.Core.Lib   -- the hardware description language
import Circuits.Reg         -- definition of the circuit to be simulated

main :: Driver a
main = driver $ do

  -- Input ports
  in_ld <- inPortBit "ld"
  in_x <- inPortWord "x" 4

  -- Input signals  
  let ld = inbsig in_ld
  let x = inwsig in_x
  
  -- Circuit defines output signals
  let (r,q) = myreg4 ld x

  -- Output ports  
  out_r <- outPortWord "r" r showBin
  out_q <- outPortWord "q" q showBin

  -- Run simulation
  runSimulation
