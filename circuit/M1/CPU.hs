------------------------------------------------------------------------
-- The Sigma16_M1 CPU circuit
------------------------------------------------------------------------

{- This module M1, a digital circuit that implements a subset of the
Sigma16 instruction set architecture. To run the circuit, use the
RunM1 simulation driver along with a main module that defines a
machine language program.  See ArrayMax.hs for an example. -}

module CPU where

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.StandardCircuits

import ALU
import Datapath
import Control

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
 4     RRR    cmplt      R1,R2,R3   R1 := R2<R3
 5     RRR    cmpeq      R1,R2,R3   R1 := R2=R3
 6     RRR    cmpgt      R1,R2,R3   R1 := R2>R3
 7     RRR    inv        R1,R2,R3   R1 := bwinv R2
 8     RRR    and        R1,R2,R3   R1 := R2 bwand R3
 9     RRR    or         R1,R2,R3   R1 := R2 bwor R3
 a     RRR    xor        R1,R2,R3   R1 := R2 bwxor R3
 b     RRR    shiftl     R1,R2,R3   R1 := R2 shiftl R3 bits
 c     RRR    shiftr     R1,R2,R3   R1 := R2 shiftr R3 bits
 d     RRR    trap       R1,R2,R3   trap interrupt
 e     XX                           (expand to XX format)
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
 f    4     RX    jumpf      Rd,x[Ra]   if Rd==0 then pc := x+Ra
 f    5     RX    jumpt      Rd,x[Ra]   if Rd/=0 then pc := x+Ra
 f    6     RX    jal        Rd,x[Ra]   Rd := pc, pc := x+Ra

Table: **Instructions represented by RX and X formats**
________________________________________________________________________
-}


------------------------------------------------------------------------
-- System
------------------------------------------------------------------------

m1_system reset dma dma_a dma_d =
  let n = 16
      (ctl_state, ctl_start, ctl_signals) = control reset ir cond
      ctl_sto = get_ctl_sto ctl_signals
      datapath_outputs = datapath ctl_signals m_out
      (ma,md,cond,a,b,ir,pc,ad,ovfl,r,x,y,p) = datapath_outputs
      m_sto = or2 dma ctl_sto
      m_addr = mux1w dma ma dma_a
      m_real_addr = field m_addr (n-8) 8
      m_data = mux1w dma md dma_d
      m_out = memw n 8 m_sto m_real_addr m_data
  in (ctl_state, ctl_start, ctl_signals,
      datapath_outputs,
      m_sto, m_addr, m_real_addr, m_data, m_out)

{- get_ctl_sto is a projection that extracts the ctl_sto field from
the entire control word. -}

get_ctl_sto (ctl_rf_ld,  ctl_rf_pc, ctl_rf_alu, ctl_rf_sd,  ctl_alu_op,
   ctl_ir_ld,  ctl_pc_ld,  ctl_pc_ad, ctl_adr_ld, ctl_adr_alu,
   ctl_ma_pc,  ctl_x_pc,   ctl_y_ad,   ctl_sto)
     = ctl_sto
