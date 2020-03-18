module M1 where

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational
import HDL.Hydra.Circuits.Register

import ALU
import ControlSignals
import Datapath
import Control
import Memory

m1 reset dma dma_a dma_d =                     -- inputs: reset and dma

  (ctl_state, ctl_start, ctlsigs,              -- output control info
   datapath_outputs,                           -- output datapath info
   m_sto, m_addr, m_real_addr, m_data, m_out)  -- output memory info

  where

-- Size parameters
    n = 16        -- word size is n, and address space is 2^n words
    msize = 16    -- installed memory contains 2^msize words
      -- if msize=n then full memory is available
      -- if msize<n the simulation may be faster but prog has less memory

-- Datapath
    datapath_outputs = datapath ctlsigs m_out
    (ma,md,cond,a,b,ir,pc,ad,ovfl,r,x,y,p) = datapath_outputs

-- Control
    (ctl_state, ctl_start, ctlsigs) = control reset ir cond

-- Memory
    m_real_addr = field m_addr (n-msize) msize
    m_out = memw n msize m_sto m_real_addr m_data

-- Input/Output using DMA
    m_data = mux1w dma md dma_d
    m_sto = or2 dma (ctl_sto ctlsigs)
    m_addr = mux1w dma ma dma_a
