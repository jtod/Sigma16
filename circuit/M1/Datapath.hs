------------------------------------------------------------------------
--			     M1 Datapath
------------------------------------------------------------------------

module Datapath where

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.StandardCircuits

import ALU

------------------------------------------------------------------------
--			       Datapath
------------------------------------------------------------------------

{- The datapath contains the registers, computational systems, and
interconnections.  It has two inputs: a set of control signals
provided by the control unit, and a data word from the either the
memory system or the DMA input controller. -}

datapath control memdat = (ma,md,cond,a,b,ir,pc,ad,ovfl,r,x,y,p)
  where

-- Give names to the individual control signals

      (ctl_rf_ld,  ctl_rf_pc,  ctl_rf_alu, ctl_rf_sd,  ctl_alu_op,
       ctl_ir_ld,  ctl_pc_ld,  ctl_pc_ad,  ctl_adr_ld,  ctl_adr_alu,
       ctl_ma_pc,  ctl_x_pc,   ctl_y_ad,   ctl_sto)
         = control

-- Specify the size parameters

      n = 16    -- word size
      k =  4    -- the register file contains 2^k registers 

-- Registers

      (a,b) = regfile n k ctl_rf_ld ir_d rf_sa rf_sb p
      ir = reg n ctl_ir_ld memdat
      pc = reg n ctl_pc_ld q
      ad = reg n ctl_adr_ld (mux1w ctl_adr_alu memdat r)

-- ALU

      (ovfl,r) = alu n ctl_alu_op x y

-- Internal processor signals

      x = mux1w ctl_x_pc a pc         -- alu input 1
      y = mux1w ctl_y_ad b ad         -- alu input 2
      rf_sa = mux1w ctl_rf_sd ir_sa ir_d  -- a = reg[rf_sa]
      rf_sb = ir_sb                       -- b = reg[rf_sb]
      p  = mux1w ctl_rf_pc            -- data input to register file
             (mux1w ctl_rf_alu memdat r)
             pc
      q = mux1w ctl_pc_ad r ad        -- input to pc
      ma = mux1w ctl_ma_pc ad pc      -- memory address - to memory
      md = a                          -- memory data - to memory for store
      cond = orw a                    -- boolean for conditional jumps

-- Instruction fields

      ir_op = field ir  0 4           -- instruction opcode
      ir_d  = field ir  4 4           -- instruction destination register
      ir_sa = field ir  8 4           -- instruction source a register
      ir_sb = field ir 12 4           -- instruction source b register

