module Circuit.RegFile where

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational
import HDL.Hydra.Circuits.Register

-- Register files

regfile1
  :: CBit a  -- a is signal type for synchronous circuit
  => a       -- ld control
  -> [a]     -- d:ds = destination address
  -> [a]     -- sa:sas = source address a
  -> [a]     -- sb:sbs = source address b
  -> a       -- x
  -> (a,a)   -- readout (a = reg[sa], b = reg[sb])

regfile1 ld [] [] [] x = (r,r)
  where r = reg1 ld x

regfile1 ld (d:ds) (sa:sas) (sb:sbs) x = (a,b)
  where
    (a0,b0) = regfile1 ld0 ds sas sbs x
    (a1,b1) = regfile1 ld1 ds sas sbs x
    (ld0,ld1) = demux1 d ld
    a = mux1 sa a0 a1
    b = mux1 sb b0 b1

--    (d1:ds) = d
--    (sa1:sas) = sa
--    (sb1:sbs) = sb

regfile :: CBit a => Int -> Int
  -> a -> [a] -> [a] -> [a] -> [a] -> ([a],[a])

regfile n k ld d sa sb x =
   unbitslice2 [regfile1 ld d sa sb (x!!i)  | i <- [0..n-1]]

{- Register file with special treatment of R0 and R15

reg[0] always outputs 0
reg[15] is always output, and can be loaded independently from other registers

Effect on state (from programmer's perspective)
  if ld                   then reg[d] := x
  if ~(ld & d=15) & ldcc  then reg[15] := xcc

State update (from perspective of circuit)
  reg[0] there is no state
  reg[d] for 0 < d < 15:  reg[d] := if ld then x else reg[d]
  reg[15] := if ld & d=15 then x
               else if ldcc then xcc
               else reg[15]

Inputs
  ld     load control
  ldcc   load into R15
  x      data input
  xcc    R15 data input
  d      destination address
  sa     source a address
  sb     source b address

Outputs
  a = reg[sa]
  b = reg[sb]
  cc = reg[15]

Recursion is controlled by the addresses; if their lengths vary there
will be a pattern match error

-}

-- RFspan determines how to generate the circuit for the base cases

data RFspan
  = RFfull      -- contains R0
  | RFhead       -- contains R15
  | RFtail       -- contains R15
  | RFinside     -- contains neither R0 nor R15

headType, tailType :: RFspan -> RFspan
headType RFfull   = RFhead
headType RFinside = RFinside
headType RFhead   = RFhead
headType RFtail   = RFinside

tailType RFfull   = RFtail
tailType RFinside = RFinside
tailType RFhead   = RFinside
tailType RFtail   = RFtail

regFileSpec1
  :: CBit a
  => RFspan
  -> a           -- ld: if ld then reg[d] := x
  -> a           -- ldcc: if ldcc then reg[15] := xcc (but ld R15 takes precedence)
  -> [a]         -- d: destination address
  -> [a]         -- sa: source a address
  -> [a]         -- sb: source b address
  -> a           -- x = data input for reg[d]
  -> a           -- xcc = data input for condition code R15
  -> (a,a,a)     -- (reg[sa], reg[sb], reg[15])

-- Recursion is based on the address words.  There will be a pattern
-- match error if the addresses (d, sa, sb) don't all have the same
-- number of bits.  There will also be a pattern match error if the
-- base case has RFtype = RFfull, as a singleton register cannot be
-- both R0 and R15.

-- Base cases

regFileSpec1 RFinside ld ldcc [] [] [] x xcc = (r,r,zero)
  where r = reg1 ld x
regFileSpec1 RFhead ld ldcc [] [] [] x xcc = (zero,zero,zero)
regFileSpec1 RFtail ld ldcc [] [] [] x xcc = (r,r,r)
  where r = reg1 (or2 ld ldcc) (mux1 ld xcc x)

-- Recursion case

regFileSpec1 rft ld ldcc (d:ds) (sa:sas) (sb:sbs) x xcc = (a,b,cc)
  where (a0,b0,cc0) = regFileSpec1 (headType rft) ld0 ldcc ds sas sbs x xcc
        (a1,b1,cc1) = regFileSpec1 (tailType rft) ld1 ldcc ds sas sbs x xcc
        (ld0,ld1) = demux1 d ld
        a = mux1 sa a0 a1
        b = mux1 sb b0 b1
        cc = cc1

-- n-bit register file with special cases for R0 and R15

regFileSpec
  :: CBit a
  => Int             -- word size
  -> a               -- ld: if ld then reg[d] := x
  -> a               -- ldcc: load R15
  -> [a]             -- d: destination address
  -> [a]             -- sa: source a address
  -> [a]             -- sb: source b address
  -> [a]             -- x = data input for reg[d]
  -> [a]             -- xcc = data input for condition code R15
  -> ([a],[a],[a])  -- (reg[sa], reg[sb], reg[15])

regFileSpec n ld ldcc d sa sb x xcc =
  unbitslice3 [regFileSpec1 RFfull ld ldcc d sa sb (x!!i) (xcc!!i)
                 | i <- [0 .. n - 1]]
