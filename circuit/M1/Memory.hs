-- Sigma16: Memory.hs
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

-----------------------------------------------------------------------
-- Memory
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
