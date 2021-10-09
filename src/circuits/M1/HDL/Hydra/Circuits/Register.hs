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

-- * Register files
   regfile1, regfile
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


------------------------------------------------------------------------
-- Register files

regfile1 :: CBit a => Int -> a -> [a] -> [a] -> [a] -> a -> (a,a)

regfile1 k ld d sa sb x
  | k==0 = (r,r)
  | k>0  = (a,b)
  where
    r = reg1 ld x
    (a0,b0) = regfile1 (k-1) ld0 ds sas sbs x
    (a1,b1) = regfile1 (k-1) ld1 ds sas sbs x
    (ld0,ld1) = demux1 d1 ld
    a = mux1 sa1 a0 a1
    b = mux1 sb1 b0 b1
    (d1:ds) = d
    (sa1:sas) = sa
    (sb1:sbs) = sb

regfile :: CBit a => Int -> Int
  -> a -> [a] -> [a] -> [a] -> [a] -> ([a],[a])

regfile n k ld d sa sb x =
   unbitslice2 [regfile1 k ld d sa sb (x!!i)  | i <- [0..n-1]]
