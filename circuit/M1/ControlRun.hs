module Main where
import HDL.Hydra.Core.Lib
import ControlSignals
import Control

---------------------------------------------------------------------------
-- Test data for control unit
---------------------------------------------------------------------------

-- Command to run the control unit test case

main = run_control control_input1

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

run_control :: [[Int]] -> IO ()
run_control input = runAllInput input output
  where

-- Extract input signals from readable input
    reset = getbit    input 0
    ir    = getbin 16 input 1
    cond  = getbit    input 2

-- Connect circuit to inputs and outputs
    (state, ctl_start, ctlsigs) = control reset ir cond

-- Format output
    output =
      [string "Control inputs\n        ",
       string " reset=", bit reset,
       string " ir=", bindec 4 ir,
       string " cond=", bit cond,

       string "\n       Control state\n        ",
       string " st_instr_fet = ", bit (st_instr_fet state),
       string "     st_load0 = ", bit (st_load0 state),
       string "     st_load1 = ", bit (st_load1 state),
       string "\n        ",
       string "     st_load2 = ", bit (st_load2 state),
       string "    st_lea0 = ", bit (st_lea0 state),
       string "    st_lea1 = ", bit (st_lea1 state),
       string "       st_add = ", bit (st_add state),
       string "\n        ",
       string "       st_sub = ", bit (st_sub state),
       string "      st_mul0 = ", bit (st_mul0 state),
       string "    st_store0 = ", bit (st_store0 state),
       string "\n        ",
       string "    st_store1 = ", bit (st_store1 state),
       string "    st_store2 = ", bit (st_store2 state),
       string "     st_cmpeq = ", bit (st_cmpeq state),
       string "     st_cmplt = ", bit (st_cmplt state),
       string "\n        ",
       string "     st_cmpgt = ", bit (st_cmpgt state),
       string "    st_jumpt0 = ", bit (st_jumpt0 state),
       string "    st_jumpt1 = ", bit (st_jumpt1 state),
       string "\n        ",
       string "    st_jumpf0 = ", bit (st_jumpf0 state),
       string "    st_jumpf1 = ", bit (st_jumpf1 state),
       string "     st_jump0 = ", bit (st_jump0 state),
       string "\n        ",
       string "     st_jump1 = ", bit (st_jump1 state),
       string "     st_trap0 = ", bit (st_trap0 state),

       string "\n       Control signals\n        ",
       string " ctl_rf_ld = ", bit (ctl_rf_ld ctlsigs),
       string " ctl_sto", bit (ctl_sto ctlsigs),
       string "\n"
        ]

