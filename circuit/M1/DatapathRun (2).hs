{-# LANGUAGE NamedFieldPuns #-}
-- allows concise definition of record of control signals

module Main where

import HDL.Hydra.Core.Lib
import ControlSignals
import Datapath


main = run_datapath testdata1

------------------------------------------------------------------------
-- Test data

testdata1 :: [[Int]]
testdata1 =
--  ctl_rf_ld   ctl_ir_ld   ctl_ad_alu  ctl_sto
--     ctl_rf_pc   ctl_pc_ld   ctl_ma_pc   memdat
--        ctl_rf_alu  ctl_pc_ad   ctl_x_pc
--           ctl_rf_sd   ctl_ad_ld   ctl_y_ad
  [[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],

-- Cycle 1.  ir = hex 0500
   [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, wd (0,5,0,0) ],

-- Cycle 2.  R5 = 18
   [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 79],

-- Cycle 3.  ir = hex 0900
   [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, wd (0,9,0,5) ],

-- Cycle 4.  R9 = 3
   [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 91],

-- Cycle 5.  ir = hex 0059
   [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, wd (0,0,5,9) ],

-- Cycle 6. r = a+b
   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],

--
   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ]

------------------------------------------------------------------------

-- wd takes 4 small ints (should be 0-15, representing 4-bit fields)
-- and returns the value of a 16-bit word consisting of those fields.
-- It's just a convenience function.

wd :: (Int,Int,Int,Int) -> Int
wd (a,b,c,d) = 4096*a +256*b + 16*c + d

------------------------------------------------------------------------

run_datapath :: [[Int]] -> IO ()
run_datapath input = runAllInput input output
  where

-- Size parameters
    n = 16    -- word size
    k =  4    -- the register file contains 2^k registers 

-- Obtain input signals from numeric input data
    ctl_rf_ld    = getbit input    0
    ctl_rf_pc    = getbit input    1
    ctl_rf_alu   = getbit input    2
    ctl_rf_sd    = getbit input    3
    ctl_ir_ld    = getbit input    4
    ctl_pc_ld    = getbit input    5
    ctl_pc_ad    = getbit input    6
    ctl_ad_ld    = getbit input    7
    ctl_ad_alu   = getbit input    8
    ctl_ma_pc    = getbit input    9
    ctl_x_pc     = getbit input   10
    ctl_y_ad     = getbit input   11
    ctl_sto      = getbit input   12
    memdat       = getbin n input 13

-- Just make the alu perform addition (aluop=0000)
-- There is a separate test AluRun for the various alu operations
    ctl_alu_a    = zero
    ctl_alu_b    = zero
    ctl_alu_c    = zero
    ctl_alu_d    = zero

-- Define the tuple of control signals
    ctlsigs = CtlSig
        {ctl_alu_a,  ctl_alu_b,  ctl_alu_c,  ctl_alu_d,
         ctl_x_pc,   ctl_y_ad,   ctl_rf_ld,  ctl_rf_pc,
         ctl_rf_alu, ctl_rf_sd,  ctl_ir_ld,  ctl_pc_ld,
         ctl_pc_ad,  ctl_ad_ld,  ctl_ad_alu, ctl_ma_pc,
         ctl_sto}

-- Connect the datapath circuit to its inputs and outputs
    (ma,md,cond,a,b,ir,pc,ad,ovfl,r,x,y,p) =
      datapath ctlsigs memdat

-- Format the output
    output =
      [
       clockcycle (\c -> take 72 (repeat '.') ++
                    "\nClock cycle " ++ show c),

       string "\n\nControl signals\n  ",
       string "  ctl_alu_a   = ", bit ctl_alu_a,
       string "  ctl_alu_b   = ", bit ctl_alu_b,
       string "  ctl_alu_c   = ", bit ctl_alu_c,
       string "  ctl_alu_d   = ", bit ctl_alu_d,
       string "\n  ",
       string "  ctl_x_pc    = ", bit ctl_x_pc,
       string "  ctl_y_ad    = ", bit ctl_y_ad,
       string "  ctl_rf_ld   = ", bit ctl_rf_ld,
       string "  ctl_rf_pc   = ", bit ctl_rf_pc,
       string "\n  ",
       string "  ctl_rf_alu  = ", bit ctl_rf_alu,
       string "  ctl_rf_sd   = ", bit ctl_rf_sd,
       string "  ctl_ir_ld   = ", bit ctl_ir_ld,
       string "  ctl_pc_ld   = ", bit ctl_pc_ld,
       string "\n  ",
       string "  ctl_ad_ld   = ", bit ctl_ad_ld,
       string "  ctl_ad_alu  = ", bit ctl_ad_alu,
       string "  ctl_ma_pc   = ", bit ctl_ma_pc,
       string "  ctl_sto     = ", bit ctl_sto,

         string "\n\nSystem bus\n",
         string "   memdat = ", binhex memdat,

         string "\n\nRegister outputs\n ",
           string "  ir = ", binhex ir,
           string "  pc = ", binhex pc,
           string "  ad = ", binhex ad,
           string "   a = ", binhex a,
           string "   b = ", binhex b,

         string "\n\nALU\n",
           string "   x = ", binhex x,
           string "   y = ", binhex y,
           string "   r = ", binhex r,

         string "\n\nCommunication signals\n",
           string "   p = ", binhex p,
           string "  ma = ", binhex ma,
           string "  md = ", binhex md,
           string " cnd = ", bit cond,

         string "\n"
        ]
