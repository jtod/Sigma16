-----------------------------------------------------------------------
--	   The Hydra Computer Hardware Description Language
--	See the README file and www.dcs.gla.ac.uk/~jtod/Hydra/
-----------------------------------------------------------------------

module Memory where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational
import HDL.Hydra.Circuits.Register

-- Defines memory circuits using flip flops and logic gates

-- memw generates a memory circuit with 2^k n-bit words, using the two
-- size parameters n and k.

memw
  :: CBit a
  => Int    -- n = wordsize
  -> Int    -- k = real address size: 2^k words in memory
  -> a      -- sto control
  -> [a]    -- p = address
  -> [a]    -- x = data to store. if sto then mem[p]:=x
  -> [a]    -- y = data fetched = mem[p]

memw n k sto p x = mapn (mem1 k sto p) n x


-- mem1 generates a memory circuit with 2^k bits, using the size
-- parameter n.

mem1
  :: CBit a
  => Int    -- k = real address size: 2^k words in memory
  -> a      -- sto control
  -> [a]    -- p = address
  -> a      -- x = data to store. if sto then mem[p]:=x
  -> a      -- y = data fetched = mem[p]

mem1 k sto p x
  | k==0  =  reg1 sto x
  | k>0   =  mux1 q m0 m1
  where
    (q:qs) = p
    (sto0,sto1) = demux1 q sto
    m0 = mem1 (k-1) sto0 qs x
    m1 = mem1 (k-1) sto1 qs x
