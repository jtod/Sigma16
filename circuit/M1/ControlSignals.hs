module ControlSignals where
import HDL.Hydra.Core.Lib

{- The behaviour of the datapath is determined by the values of the
control signals.  A naming convention is used for these signals.  Each
control signal has a name with three parts, separated by _.  The first
part is ctl (to indicate that it's a control signal).  The second part
is the name of the subsystem in the datapath which is being
controlled, and the third part is the specific operation being
commanded by the signal. -}

data CtlSig a = CtlSig
  {
-- Controls for ALU
   ctl_alu_a,   -- 4-bit alu operation code (see section on the ALU)
   ctl_alu_b,   --   "
   ctl_alu_c,   --   "
   ctl_alu_d,   --   "
   ctl_x_pc,    -- Transmit pc on x (if 0, transmit reg[sa])
   ctl_y_ad,    -- Transmit ad on y (if 0, transmit reg[sb])

-- Controls for register file
   ctl_rf_ld,   --  Load  register file (if 0, remain unchanged)
   ctl_rf_pc,   --   Input to register file is pc (if 0, check ctl_rf_alu)
   ctl_rf_alu,  -- Input to register file is ALU output r (if 0, use m)
   ctl_rf_sd,   -- Use ir_d as source a address (if 0, use ir_sa)

-- Controls for system registers
   ctl_ir_ld,   -- Load ir register (if 0, remain unchanged)
   ctl_pc_ld,   -- Load pc register (if 0, remain unchanged)
   ctl_pc_ad,   -- Input to pc is ad (if 0, r)
   ctl_ad_ld,   -- Load ad register (if 0, remain unchanged)
   ctl_ad_alu,  -- Obtain ad input from alu (if 0, from memory data input)

-- Controls for memory
   ctl_ma_pc,   -- Transmit pc on memory address bus (if 0, transmit addr)
   ctl_sto      -- Memory store (if 0, fetch)

   :: a         -- all controls are bit signals
  }

data CtlState a = CtlState
  {st_instr_fet,
   st_dispatch,
   st_add,
   st_sub,
   st_mul0,
   st_cmplt,
   st_cmpeq,
   st_cmpgt,
   st_trap0,
   st_lea0, st_lea1,
   st_load0, st_load1, st_load2,
   st_store0, st_store1, st_store2,
   st_jump0, st_jump1,
   st_jumpf0, st_jumpf1,
   st_jumpt0, st_jumpt1,
   st_jal0, st_jal1
   :: a         -- all control states are bit signals
  }
