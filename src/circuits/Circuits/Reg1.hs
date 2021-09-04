module Circuits.Reg1 where

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational
-- import HDL.Hydra.Circuits.Register

--  This example is named myreg1 to avoid confusion with the reg1
-- circuit defined in the Hydra libraries.

-- Normally reg1 would only output r, as q is just an internal signal.
-- However, it is enlightening for a student to experiment
-- interactively with the circuit, and to do that it's helpful to see
-- the value of q.

myreg1 :: CBit a => a -> a -> (a,a)
myreg1 ld x = (r, q)
  where r = dff q
        q = or2 (and2 (inv ld) r) (and2 ld x)
