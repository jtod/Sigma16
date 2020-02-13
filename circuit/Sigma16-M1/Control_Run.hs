module Control_Run where

import HDL.Hydra.Core.Lib
import Control

---------------------------------------------------------------------------
-- Test data for control unit
---------------------------------------------------------------------------

-- Command to run the control unit test case

main = sim_control control_input1

-- Test data for the control unit

control_input1 :: [[Int]]
control_input1 =
--  reset   ir   cond
  [[  1,     0,   0],
   [  0,     0,   0],
   [  0,     0,   0],
   [  0,     0,   0],
   [  0,     0,   0]
  ]


---------------------------------------------------------------------------
-- Simulation driver for control unit
---------------------------------------------------------------------------

sim_control :: [[Int]] -> IO ()
sim_control input =
  let
      reset = getbit    input 0
      ir    = getbin 16 input 1
      cond  = getbit    input 2

      (state,start,ctl_signals) = control reset ir cond

      (ctl_rf_ld,  ctl_rf_pc,  ctl_rf_alu, ctl_rf_sd,  ctl_alu_op,
       ctl_ir_ld,  ctl_pc_ld,  ctl_pc_ad,  ctl_adr_ld,  ctl_adr_alu,
       ctl_ma_pc,  ctl_x_pc,   ctl_y_ad,   ctl_sto)
         = ctl_signals

      [st_instr_fet, st_dispatch, st_nop,
       st_load0, st_load1, st_load2, st_ldval0,st_ldval1,
       st_add, st_sub, st_neg, st_mul0, st_store0, st_store1, st_store2,
       st_cmpeq, st_cmplt, st_cmpgt, st_jumpt0, st_jumpt1,
       st_jumpf0, st_jumpf1, st_jump0, st_jump1, st_trape, st_trapf]
         = state

      simoutput :: [Format Bool ()]
      simoutput =
        [string "Control inputs\n        ",
         string " reset=", bit reset,
         string " ir=", bindec 4 ir,
         string " cond=", bit cond,

         string "\n       Control state\n        ",
         string " st_instr_fet = ", bit st_instr_fet,
         string "       st_nop = ",  bit st_nop,
         string "     st_load0 = ", bit st_load0,
         string "     st_load1 = ", bit st_load1,
         string "\n        ",
         string "     st_load2 = ", bit st_load2,
         string "    st_ldval0 = ", bit st_ldval0,
         string "    st_ldval1 = ", bit st_ldval1,
         string "       st_add = ", bit st_add,
         string "\n        ",
         string "       st_sub = ", bit st_sub,
         string "       st_neg = ", bit st_neg,
         string "      st_mul0 = ", bit st_mul0,
         string "    st_store0 = ", bit st_store0,
         string "\n        ",
         string "    st_store1 = ", bit st_store1,
         string "    st_store2 = ", bit st_store2,
         string "     st_cmpeq = ", bit st_cmpeq,
         string "     st_cmplt = ", bit st_cmplt,
         string "\n        ",
         string "     st_cmpgt = ", bit st_cmpgt,
         string "    st_jumpt0 = ", bit st_jumpt0,
         string "    st_jumpt1 = ", bit st_jumpt1,
         string "\n        ",
         string "    st_jumpf0 = ", bit st_jumpf0,
         string "    st_jumpf1 = ", bit st_jumpf1,
         string "     st_jump0 = ", bit st_jump0,
         string "\n        ",
         string "     st_jump1 = ", bit st_jump1,
         string "     st_trape = ", bit st_trape,
         string "     st_trapf = ", bit st_trapf,

         string "\n       Control signals\n        ",
         string " ctl_rf_ld = ", bit ctl_rf_ld,
         string " ctl_sto", bit ctl_sto,
         string "\n"
        ]
  in do putStrLn "\nDatapath simulation"
        runAllInput input simoutput

