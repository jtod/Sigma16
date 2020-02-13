module Main where

{- Simulation driver and test data for Datapath1. -}

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.StandardCircuits

import Datapath

main = run_datapath datapath_data_1

{- The first letter of the name of a control signal begins directly
above the column in the data for that signal.  The last column of
input is memdat, a 16-bit word.  This can be given as a decimal number
(e.g. 79).  If you want to provide a value to place in the Instruction
Register, you can write wd (a,b,c,d) which will generate a word where
op=a, d=b, sa=c, sb=d. -}

datapath_data_1 :: [[Int]]
datapath_data_1 =
--  ctl_rf_ld   ctl_alu_op_a   ctl_pc_ld      ctl_x_pc
--     ctl_rf_pc   ctl_alu_op_b   ctl_pc_ad      ctl_y_ad
--        ctl_rf_alu  ctl_alu_op_c   ctl_adr_ld     ctl_sto
--           ctl_rf_sd   ctl_alu_op_d   ctl_adr_alu    memdat
--                          ctl_ir_ld      ctl_ma_pc
  [[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],

-- Cycle 1.  ir = hex 0500
   [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, wd (0,5,0,0) ],

-- Cycle 2.  R5 = 18
   [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 79],

-- Cycle 3.  ir = hex 0900
   [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, wd (0,9,0,5) ],

-- Cycle 4.  R9 = 3
   [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 91],

-- Cycle 5.  ir = hex 0059
   [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, wd (0,0,5,9) ],

-- Cycle 6. r = a+b
   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],

--
   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
  ]


wd :: (Int,Int,Int,Int) -> Int
wd (a,b,c,d) = 4096*a +256*b + 16*c + d

run_datapath :: [[Int]] -> IO ()
run_datapath input =
  do putStrLn "\nRun datapath"
     runAllInput input output
  where

-- Size parameters
    n = 16    -- word size
    k =  4    -- the register file contains 2^k registers 

-- Obtain input signals from numeric input data

    ctl_rf_ld    = getbit input    0
    ctl_rf_pc    = getbit input    1
    ctl_rf_alu   = getbit input    2
    ctl_rf_sd    = getbit input    3
    ctl_alu_op_a = getbit input    4
    ctl_alu_op_b = getbit input    5
    ctl_alu_op_c = getbit input    6
    ctl_alu_op_d = getbit input    7
    ctl_ir_ld    = getbit input    8
    ctl_pc_ld    = getbit input    9
    ctl_pc_ad    = getbit input   10
    ctl_adr_ld    = getbit input  11
    ctl_adr_alu   = getbit input  12
    ctl_ma_pc    = getbit input   13
    ctl_x_pc     = getbit input   14
    ctl_y_ad     = getbit input   15
    ctl_sto      = getbit input   16
    memdat       = getbin n input 17

-- Connect the datapath circuit to its inputs and outputs

    (ma,md,cond,a,b,ir,pc,ad,ovfl,r,x,y,p) =
      datapath control memdat

-- Define the tuple of control signals
    ctl_alu_op = (ctl_alu_op_a,ctl_alu_op_b,ctl_alu_op_c,ctl_alu_op_d)
    control =
      (ctl_rf_ld,  ctl_rf_pc,  ctl_rf_alu, ctl_rf_sd,  ctl_alu_op,
       ctl_ir_ld,  ctl_pc_ld,  ctl_pc_ad,  ctl_adr_ld,  ctl_adr_alu,
       ctl_ma_pc,  ctl_x_pc,   ctl_y_ad,   ctl_sto)

--      (ctl_rf_ld,  ctl_rf_pc,  ctl_rf_alu, ctl_rf_sd,
--       ctl_ir_ld,  ctl_pc_ld,  ctl_pc_ad,  ctl_adr_ld,  ctl_adr_alu,
--       ctl_ma_pc,  ctl_x_pc,   ctl_y_ad,   ctl_sto)

-- Format the output

    output =
      [
       clockcycle (\c -> take 72 (repeat '.') ++
                    "\nClock cycle " ++ show c),

         string "\n\nControl signals\n  ",
           string " ctl_rf_ld  = ",  bit ctl_rf_ld,
           string " ctl_rf_pc  = ",   bit ctl_rf_pc,
           string " ctl_rf_alu = ",  bit ctl_rf_alu,
           string " ctl_rf_sd  = ",  bit ctl_rf_sd,
           string "\n  ",
           string " ctl_ir_ld  = ",  bit ctl_ir_ld,
           string " ctl_pc_ld  = ",  bit ctl_pc_ld,
           string " ctl_adr_ld  = ",  bit ctl_adr_ld,
           string " ctl_adr_alu = ",  bit ctl_adr_alu,
           string "\n  ",
           string " ctl_ma_pc  = ",  bit ctl_ma_pc,
           string " ctl_x_pc   = ",   bit ctl_x_pc,
           string " ctl_y_ad   = ",   bit ctl_y_ad,
           string " ctl_sto    = ",    bit ctl_sto,

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


{-
OLD TEST DATA

test_data_1 :: [[Int]]
test_data_1 =
--  ctl_rf_ld   ctl_ir_ld   ctl_adr_alu  ctl_sto
--     ctl_rf_pc   ctl_pc_ld   ctl_ma_pc   memdat
--        ctl_rf_alu  ctl_pc_ad   ctl_x_pc
--           ctl_rf_sd   ctl_adr_ld   ctl_y_ad

-- Cycle 0.  Doesn't do anything.
--     Expect:
--       During cycle:  everthing is 0
--       After clock tick:  all registers remain 0
  [[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],

-- Cycle 1.
--    ir = hex 0505
--    Assert [ctl_ir_ld], memdat = 0505
--    Expect:
--      During cycle:  ctl_ir_ld=1, memdat=0500
--      After clock tick:  ir=0500
   [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, wd (0,5,0,5) ],

-- Cycle 2.
--    R5 = 79
--    Assert [ctl_rf_ld], memdat=79 (hex 004f)
--    Expect:
--      During cycle:  ctl_rf_ld=1, memdat=004f, a=0, b=0
--      After clock tick:  R5=004f, b=004f
   [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 79],

-- Last cycle.
--   No more operations, just a dummy cycle to allow any
--   register updates from previous cycle to become visible.
   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ]

-}
