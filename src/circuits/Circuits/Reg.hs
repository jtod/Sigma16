module Circuits.Reg where

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

myreg4 :: CBit a => a -> [a] -> ([a],[a])
myreg4 ld [x3,x2,x1,x0] = ([r3,r2,r1,r0], [q3,q2,q1,q0])
  where r3 = dff q3
        r2 = dff q2
        r1 = dff q1
        r0 = dff q0
        q3 = or2 (and2 (inv ld) r3) (and2 ld x3)
        q2 = or2 (and2 (inv ld) r2) (and2 ld x2)
        q1 = or2 (and2 (inv ld) r1) (and2 ld x1)
        q0 = or2 (and2 (inv ld) r0) (and2 ld x0)
