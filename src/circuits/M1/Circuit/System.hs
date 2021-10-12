-- Sigma16: M1.hs
-- Copyright (C) 2021 John T. O'Donnell
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

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Circuit.System
  ( m1
  , module Circuit.Interface
  , module Circuit.ALU
  , module Circuit.Datapath
  , module Circuit.Control
  , module Circuit.Memory

  ) where

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational
import HDL.Hydra.Circuits.Register

import Circuit.Interface
import Circuit.ALU
import Circuit.Datapath
import Circuit.Control
import Circuit.Memory

-- M1 is a digital circuit that implements the Core subset of the
-- Sigma16 instruction set architecture, apart from mul and div, which
-- are unimplemented.

------------------------------------------------------------------------
{- Instruction set architecture of Sigma16

RRR instructions
________________________________________________________________________

 op   format  mnemonic   operands   action
---- -------- ---------- ---------- ------------------------------------
 0     RRR    add        R1,R2,R3   R1 := R2+R3
 1     RRR    sub        R1,R2,R3   R1 := R2-R3
 2     RRR    mul        R1,R2,R3   R1 := R2*R3, R15 := high word
 3     RRR    div        R1,R2,R3   R1 := R2/R3, R15 := R2 mod R3
 4     RRR    cmp        R2,R3      R15 := R2 cmp R3
 b     RRR    trap       R1,R2,R3   trap interrupt
 e     EXP                          (expand to EXP format)
 f     RX                           (expand to RX format)

Table: **Instructions represented in RRR format**
________________________________________________________________________

RX instructions
________________________________________________________________________

 op   b   format  mnemonic   operands   action
---- --- -------- ---------- ---------- ---------------------------------
 f    0     RX    lea        Rd,x[Ra]   Rd := x+Ra
 f    1     RX    load       Rd,x[Ra]   Rd := mem[x+Ra]
 f    2     RX    store      Rd,x[Ra]   mem[x+Ra] := Rd
 f    3     RX    jump       x[Ra]      pc := x+Ra
 f    4     RX    jumpc0     Rd,x[Ra]   if Rd==0 then pc := x+Ra
 f    5     RX    jumpc1     Rd,x[Ra]   if Rd/=0 then pc := x+Ra
 f    6     RX    jal        Rd,x[Ra]   Rd := pc, pc := x+Ra

Table: **Instructions represented by RX and X formats**
________________________________________________________________________
-}



{-
I/O control inputs
dma           1 bit    indicates stolen clock cycle
dma_store     1 bit    mem[dma_a] := dma_d
dma_fetch     1 bit    m_out = mem[dma_a]
dma_reg       1 bit    x = reg[dma_a]  (least significant 4 bits
dma_a         16 bits  address
dma_d         16 bits  data
-}

m1 reset (SysIO {..}) =
  (ctl_state, ctl_start, ctlsigs,              -- control
   dp,                                         -- datapath
   m_sto, m_addr, m_real_addr, m_data, m_out)  -- memory

  where

-- Size parameters
    n = 16        -- word size is n, and address space is 2^n words
    msize = 16    -- installed memory contains 2^msize words
      -- if msize=n then full memory is available
      -- if msize<n the simulation may be faster but prog has less memory

-- Datapath
    dp = datapath ctlsigs (SysIO {..}) m_out
--    (r,ccnew,condcc) = aluOutputs dp
    (r,ccnew) = aluOutputs dp

-- Control
    (ctl_state, ctl_start, ctlsigs) =
--       control reset (ir dp) (cc dp) condcc (SysIO {..})
             control reset (ir dp) (cc dp) (SysIO {..})

-- Memory
    m_real_addr = field m_addr (n-msize) msize
    m_out = memw n msize m_sto m_real_addr m_data

-- Input/Output using DMA
    m_sto = or2 (and2 io_DMA io_memStore)              -- I/O store
                (and2 (inv io_DMA) (ctl_sto ctlsigs))  -- CPU store
    m_data = mux1w io_DMA (md dp) io_data
    m_addr = mux1w io_DMA (ma dp) io_address
