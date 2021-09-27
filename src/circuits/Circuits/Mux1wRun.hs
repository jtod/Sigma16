-- Mux1wRun: simulate they Hydra mux1w circuit
-- Copyright (C) 2021 John T. O'Donnell.  This file is part of Sigma16.
-- See Sigma16/README and https://jtod.github.io/home/Sigma16/

module Main where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational  -- defines mux1w circuit


-- The mux1w circuit is defined in HDL.Hydra.Circuits.Combinational.
-- Here is the circuit definition (commented out to avoid redefining
-- it in this file):

-- mux1w :: Bit a => a -> [a] -> [a] -> [a]
-- mux1w c x y = map2 (mux1 c) x y

testdata1 =
  [ "0 3 9",
    "1 3 9",
    "0 1 2",
    "1 1 2"]

main :: Driver a
main = driver $ do

  initialize
  storeInputList "data" testdata1
  selectInputList "data"
  
  -- Input ports
  in_c <- inPortBit "c"
  in_x <- inPortWord "x" 4
  in_y <- inPortWord "y" 4

  -- Input signals
  let c = inbsig in_c
  let x = inwsig in_x
  let y = inwsig in_y
  
  -- The circuit myreg1 receives input signals and defines output signals
  let r = mux1w c x y

-- Instead of defining outports, we will use a format to display the
-- simulation results
  
  format
    [string "Inputs:",
     string "c = ", bit c,
     string "  x = ", binhex x,
     string "  y = ", binhex y,
     string "    Outputs: r = ", binhex r,
     string "\n"
    ]

  runSimulation
