module Main where

{- Simulation driver and test data for Datapath1, with solution to CA4
Unassessed Exercise 5. -}


import HydraLib
import HydraLib.StandardCircuits
import Datapath1

main =
  do putStrLn "\nProblem 4\n"
     run_datapath problem_4_data

------------------------------------------------------------------------

{- Note on reading the test data.  The first letter of the name of a
control signal begins directly above the column in the data for that
signal.  The last column of input is memdat, a 16-bit word.  This can
be given as a decimal number (e.g. 79).  If you want to provide a
value to place in the Instruction Register, you can write wd (a,b,c,d)
which will generate a word where op=a, d=b, sa=c, sb=d.

Hex numbers will be written as four digits, 0500, while decimal
numbers will just be written like 23.
-}

------------------------------------------------------------------------
-- Problem 4
------------------------------------------------------------------------

{- We want to make the datapath add 2+3, and we hope to see the result
5.  Work out the inputs to the datapath that are required to make it
do this.  Test your answer by simulation.

Strategy: perform the following sequence of high level actions
  R5 := 2
  R6 := 3
  compute R5 + R6 and observe the result

Note that this doesn't correspond exactly to the Sigma16 instruction
set, which has no instruction equivalent to R1 := 2.  But the datapath
has the ability to perform the actions given above.  Alternatively, we
could write out a solution using Sigma16 instructions, using lea to
get the constants into the registers, and using an add instruction.
That solution would be slightly longer than the one given here.

To perform those high level actions, we need a sequence of lower level
actions that can be performed in the datapath, one per clock cycle.

Cycle  operation       inputs                    prediction
------------------------------------------------------------------------
  1    ir := 0500    ctl_ir_ld   memdat=0500
  2    R5 := 2       ctl_rf_ld   memdat=0002
  3    ir := 0656    ctl_ir_ld   memdat=0656
  4    R6 := 3       ctl_rf_ld   memdat=0003
  5    r = R5 + R6                               r = 0005
------------------------------------------------------------------------
 -}

problem_4_data :: [[Int]]
problem_4_data =
--  ctl_rf_ld   ctl_ir_ld   ctl_ad_alu  ctl_sto
--     ctl_rf_pc   ctl_pc_ld   ctl_ma_pc   memdat
--        ctl_rf_alu  ctl_pc_ad   ctl_x_pc
--           ctl_rf_sd   ctl_ad_ld   ctl_y_ad

-- Cycle 0.  Doesn't do anything.
--     Expect:
--       During cycle:  everthing is 0
--       After clock tick:  all registers remain 0
  [[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],

-- Cycle 1.
--    ir = hex 0500
--    Assert [ctl_ir_ld], memdat = 0500
--    Expect:
--      During cycle:  ctl_ir_ld=1, memdat=0500
--      Next cycle:    ir=0500
   [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, wd (0,5,0,0) ],

-- Cycle 2.
--    R5 = 2
--    Assert [ctl_rf_ld], memdat=0002
--    Expect:
--      During cycle:  ctl_rf_ld=1, memdat=0002, a=0, b=0
--      After clock tick:  R5=0002 (hidden inside regfile)
   [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2],

-- Cycle 3.
--    ir = hex 0656
--    Assert [ctl_ir_ld], memdat = 0656
--    Expect:
--      During cycle:  ctl_ir_ld=1, memdat=0656
--      After clock tick:  ir=0656
   [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, wd (0,6,5,6) ],

-- Cycle 4.
--    R5 = 3
--    Assert [ctl_rf_ld], memdat=0003
--    Expect:
--      During cycle:  ctl_rf_ld=1, memdat=0003, a=0, b=0
--      After clock tick:  R5=0003
   [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3],

-- Last cycle.
--    Expect:
--       a=0002 b=0003 x=a=0002 y=b=0003 r=0005

--   No more operations, just a dummy cycle to allow any
--   register updates from previous cycle to become visible.
   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ]


------------------------------------------------------------------------
-- Problem 4

{- Make the datapath perform the instruction load R7,1234[R8].  Just
do the parts of the work that are executed by the datapath.  Since the
memory is not part of the datapath, you will need to provide the data
provided by the memory; assume this is 39.

The machine language representation of the instruction comprises two
words: f780 1234.  The effective address is 1234+R8; we don't know the
contents of R8, but we can make the datapath go ahead and compute the
effective address, and we will supply the data 0027 (hex
representation of 39) when the memory data fetch is required.

       ad := mem[pc], pc++;
       ad := reg[ir_sa] + ad
       reg[ir_d] := mem[ad]


-}

------------------------------------------------------------------------

------------------------------------------------------------------------

-- Build an instruction word from its four fields

wd :: (Int,Int,Int,Int) -> Int
wd (a,b,c,d) = 4096*a +256*b + 16*c + d

-- Simulation driver for datapath

run_datapath :: [[Int]] -> IO ()
run_datapath input =
  do putStrLn "\nRun datapath"
     runThroughInput input output
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

-- Connect the datapath circuit to its inputs and outputs

    (ma,md,cond,a,b,ir,pc,ad,ovfl,r,x,y,p) =
      datapath control memdat

-- Define the tuple of control signals
    control =
      (ctl_rf_ld,  ctl_rf_pc,  ctl_rf_alu, ctl_rf_sd,
       ctl_ir_ld,  ctl_pc_ld,  ctl_pc_ad,  ctl_ad_ld,  ctl_ad_alu,
       ctl_ma_pc,  ctl_x_pc,   ctl_y_ad,   ctl_sto)

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
           string " ctl_ad_ld  = ",  bit ctl_ad_ld,
           string " ctl_ad_alu = ",  bit ctl_ad_alu,
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
