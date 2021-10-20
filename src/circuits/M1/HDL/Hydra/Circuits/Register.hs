{-|
Module       : HDL.Hydra.Circuits.Register
Description  : Standard stateful circuits
Copyright    : (c) John O'Donnell 2020
License      : GPL-3.0-or-later
Maintainer   : john.t.odonnell9@gmail.com
Stability    : experimental

A collection of register circuits. -}

module HDL.Hydra.Circuits.Register
  (

-- * Latches
   latch1, latch,

-- * Registers
   reg1, reg,


  ) where

import HDL.Hydra.Core.Signal
import HDL.Hydra.Core.SigStream
import HDL.Hydra.Core.Pattern
import HDL.Hydra.Circuits.Combinational

------------------------------------------------------------------------
-- Latches

-- | latch1 is a bit latch; it is just an alternative name for a delay
-- flip flop.  A latch1 takes an input bit signal and produces an
-- output bit signal.  The latch has in internal state of 1 bit.  The
-- output always gives the current value of the state.  At a clock
-- tick, the state is discarded and replaced with the current value of
-- the input.

latch1 :: CBit a => a -> a
latch1 = dff

-- |A word latch has a size parameter k which determines how many bits
-- it contains.  The latch has a k-bit state, which is output
-- continuously.  At each clock tick the old state is discarded and
-- replaced by the value of the k-bit input word.

latch :: CBit a => Int -> [a] -> [a]
latch k x = mapn dff k x

------------------------------------------------------------------------
-- Registers

-- |reg1 is a register with a state of one bit.  It is used as a building
-- block for word registers.
-- Example: y = reg1 ld x

reg1 :: CBit a => a -> a -> a
reg1 ld x = r
  where r = dff (mux1 ld r x)

-- |A word register with a size parameter k and a state of k bits.  It
-- is used as a building block for word registers.  Example: y = reg1
-- ld x

reg
  :: CBit a
  => Int        -- k = the word size
  -> a          -- ld = the load control signal
  -> [a]        -- x = input word of size k
  -> [a]        -- y = output is the register state

reg k ld x = mapn (reg1 ld) k x


