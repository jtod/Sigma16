------------------------------------------------------------------------
-- Simulation driver and test cases for M1 architecture
-- John O'Donnell, Department of Computing Science,
-- University of Glasgow, Glasgow, UK
-- February 2010
------------------------------------------------------------------------

-- $$$ Exercise 7.1

{- This module defines simulation drivers that run the M1 circuit for
the Sigma16 instruction set architecture. A separate main program
should be defined that imports this module, and that defines the
machine language program.  It uses run_M1_program, defined below, to
execute the program. -}

module Tut7_1_RunM1 where  -- $$$ Exercise 7.1

import Hydra
import Hydra.StandardCircuits
import Tut7_1_M1 -- $$$ Exercise 7.1

------------------------------------------------------------------------
-- Simulation model
------------------------------------------------------------------------

{- The simulations are carried out using the Stream Bool model.  The
types Bit and Word are defined for clarity; a Bit is represented as a
Stream of Bools for the simulations. -}

type Bit = Stream Bool
type Word = [Bit]

------------------------------------------------------------------------
-- Running a machine language program
------------------------------------------------------------------------

{- The run_M1_program function takes a program (in the form of a list
of strings), loads it into the machine, and executes it, using the
sim_m1 simulation driver. -}

run_M1_program :: [String] -> Int -> IO ()
run_M1_program prog n =
  let inps = zipWith f [0..] prog ++ [[1,0,0,0]] ++ repeat [0,0,0,0]
      f i x = [0, 1, i, hexbin 16 x]
      m = n + length prog
  in  sim_m1 (take m inps)

------------------------------------------------------------------------
-- Simulation driver for the M1 system
------------------------------------------------------------------------

-- The simulation driver maintains its own state, consisting of a pair
-- (c,s) where c :: Int is the clock cycle number, and s ::
-- DriverState contains information the driver saves for its own use
-- later on.

data DriverState = DriverState
  {displacement :: (Int,[Int]),      -- (cycle, displacement)
   effAddr :: [Int],                 -- effective address
   rfloads :: [(Int,[Int],[Int])],   -- [(cycle,reg,value)]
   memStores :: [(Int,[Int],[Int])], -- [(cycle,addr,value)]
   jumps :: [(Int,Int,[Int])],       -- [(cycle,jumped,pcvalue)]
   trap :: Bool}                     -- has a trap just been executed?
  deriving Show

initDriverState :: DriverState
initDriverState =
  DriverState
    {displacement = (0, take 16 (repeat 0)),
     effAddr = take 16 (repeat 0),
     rfloads = [],
     memStores = [],
     jumps = [],
     trap = False}

-- Record and display the effective address

setEffAddr :: (Signal a, Static a) =>
   (Int,DriverState) -> [[a]] -> DriverState
setEffAddr (c,s) [x] =
  s {effAddr = map sigInt x}

showEffAddr :: (Int,DriverState) -> String
showEffAddr (c,s) = ints16hex4 (effAddr s)

-- Record and display loads to the register file

setRfLoad :: (Signal a, Static a) =>
   (Int,DriverState) -> [[a]] -> DriverState
setRfLoad (c,s) [r,x] =
  s {rfloads = (c, map sigInt r, map sigInt x) : rfloads s}

clearRfLoads :: (Signal a, Static a) =>
  (Int,DriverState) -> [[a]] -> DriverState
clearRfLoads (c,s) _ = s {rfloads = []}

showRfLoads :: (Int,DriverState) -> String
showRfLoads (c,s) = concat (map f (reverse (rfloads s)))
  where f (c,r,x) =
          "R" ++ show (intsInt r) ++ " := " ++  ints16hex4 x
            ++ " was loaded in cycle " ++ show c ++ "\n"

-- Record and display stores to the memory

setMemStore :: (Signal a, Static a) =>
   (Int,DriverState) -> [[a]] -> DriverState
setMemStore (c,s) [a,x] =
  s {memStores = (c, map sigInt a, map sigInt x) : memStores s}

clearMemStores :: (Signal a, Static a) =>
  (Int,DriverState) -> [[a]] -> DriverState
clearMemStores (c,s) _ = s {memStores = []}

showMemStores :: (Int,DriverState) -> String
showMemStores (c,s) = concat (map f (reverse (memStores s)))
  where f (c,a,x) =
          "mem[" ++ ints16hex4 a ++ "] := " ++  ints16hex4 x
            ++ " was stored in cycle " ++ show c ++ "\n"

-- Record and display jumps

setJump :: (Signal a, Static a) =>
   (Int,DriverState) -> [[a]] -> DriverState
setJump (c,s) [[b],x] =
  s {jumps = (c, sigInt b, map sigInt x) : jumps s}

clearJumps :: (Signal a, Static a) =>
  (Int,DriverState) -> [[a]] -> DriverState
clearJumps (c,s) _ = s {jumps = []}

showJumps :: (Int,DriverState) -> String
showJumps (c,s) = concat (map f (reverse (jumps s)))
  where
    f (c,b,x) =
      (if b==1
        then "jumped to " ++ ints16hex4 x
        else "did not jump")
        ++ " in cycle " ++ show c ++ "\n"

-- When the driver discovers that a trap has executed, it uses setTrap
-- to record this in the driver state.  The termination predicate uses
-- this value to decide when to stop the simulation.

setTrap :: (Int,DriverState) -> [a] -> DriverState
setTrap (c,s) _ = s {trap = True}

setDisplacement :: (Signal a, Static a) =>
   (Int,DriverState) -> [[a]] -> DriverState
setDisplacement (c,s) [w] =
  s {displacement = (c, map sigInt w)}

showDisplacement :: (Int,DriverState) -> String
showDisplacement (c,s) =
  let (c,d) = displacement s
--  in "displacement " ++ ints16hex4 d
--       ++ " loaded in cycle " ++ show c ++ "\n"
  in ints16hex4 d

-- The termination predicate, termpred, stops the simulation when a
-- trap instruction is executed, or after 1000 cycles.

termpred :: (Int,DriverState) -> Bool
termpred (c,s) = trap s || c > 1000

-- The simulation driver is sim_m1.

sim_m1 :: [[Int]] -> IO ()
sim_m1 input =
  let

-- Get the system inputs from the "input" argument.

      reset = getbit    input 0 :: Bit
      dma   = getbit    input 1 :: Bit
      dma_a = getbin 16 input 2 :: Word
      dma_d = getbin 16 input 3 :: Word

-- Apply the circuit m1_system to the system inputs, and obtain its
-- outputs.

      (ctl_state, ctl_start, ctl_signals, datapath_outputs,
       m_sto, m_addr, m_real_addr, m_data, m_out)
         = m1_system reset dma dma_a dma_d

-- Define names for the outputs from the datapath.

      (ma,md,cond,a,b,ir,pc,ad,ovfl,r,x,y,p) = datapath_outputs

-- Define names for the control state flipflops.

      [st_instr_fet, st_dispatch, st_add, st_sub,
       st_mul0,
       st_cmpeq, st_cmplt, st_cmpgt, st_trap0, st_trap1,
       st_lea0, st_lea1, st_load0, st_load1, st_load2,
       st_store0, st_store1, st_store2, st_jump0, st_jump1,
       st_jumpf0, st_jumpf1, st_jumpt0, st_jumpt1,
       st_jal0, st_jal1]
        = ctl_state

-- Define names for the individual control signals.

      (ctl_rf_ld,  ctl_rf_pc,  ctl_rf_alu, ctl_rf_sd,  ctl_alu_op,
       ctl_ir_ld,  ctl_pc_ld,  ctl_pc_ad,  ctl_ad_ld,  ctl_ad_alu,
       ctl_ma_pc,  ctl_x_pc,   ctl_y_ad,   ctl_sto,
       ctl_new_signal) -- $$$ Exercise 7.1
        = ctl_signals
      (ctl_alu_a,ctl_alu_b,ctl_alu_c,ctl_alu_d) = ctl_alu_op

-- Format the output, inserting string labels for the signals, and
-- converting them to readable form.

      simoutput :: [Format Bool DriverState]
      simoutput =
        [
         clockcycle (\c -> take 72 (repeat '.') ++
                      "\nClock cycle " ++ show c ++ "\n"),

         string "Computer system inputs\n        ",
         string " reset=", bit reset, string " dma=", bit dma,
         string " dma_a=", binhex dma_a, string " dma_d=", binhex dma_d,
         string "\nctl_start = ", bit ctl_start, string "\n",

         string "\nControl state\n  ",

         string " st_instr_fet = ", bit st_instr_fet,
         string "  st_dispatch = ", bit st_dispatch,
         string "       st_add = ", bit st_add,
         string "       st_sub = ", bit st_sub,
         string "\n  ",
         string "      st_mul0 = ", bit st_mul0,
         string "     st_cmplt = ", bit st_cmplt,
         string "     st_cmpeq = ", bit st_cmpeq,
         string "     st_cmpgt = ", bit st_cmpgt,
         string "\n  ",
         string "     st_trap0 = ", bit st_trap0,
         string "      st_lea0 = ", bit st_lea0,
         string "      st_lea1 = ", bit st_lea1,
         string "     st_load0 = ", bit st_load0,
         string "\n  ",
         string "     st_load1 = ", bit st_load1,
         string "     st_load2 = ", bit st_load2,
         string "    st_store0 = ", bit st_store0,
         string "    st_store1 = ", bit st_store1,
         string "\n  ",
         string "    st_store2 = ", bit st_store2,
         string "     st_jump0 = ", bit st_jump0,
         string "     st_jump1 = ", bit st_jump1,
         string "    st_jumpf0 = ", bit st_jumpf0,
         string "\n  ",
         string "    st_jumpf1 = ", bit st_jumpf1,
         string "    st_jumpt0 = ", bit st_jumpt0,
         string "    st_jumpt1 = ", bit st_jumpt1,
         string "      st_jal0 = ", bit st_jal0,
         string "\n  ",
         string "      st_jal1 = ", bit st_jal1,

         string "\n\nControl signals\n  ",
           string " ctl_alu_a  = ", bit ctl_alu_a,
           string " ctl_alu_b  = ", bit ctl_alu_b,
           string " ctl_alu_c  = ", bit ctl_alu_c,
           string " ctl_alu_d  = ", bit ctl_alu_d,
           string "\n  ",
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
           string "\n  ",                   -- $$$ Exercise 7.1
           string "ctl_new_signal = ",      -- $$$ Exercise 7.1
           bit ctl_new_signal,              -- $$$ Exercise 7.1

         string "\n\nDatapath\n  ",
           string "  ir = ", binhex ir,
           string "  pc = ", binhex pc,
           string "  ad = ", binhex ad,
           string "   a = ", binhex a,
           string "   b = ", binhex b,
           string "   r = ", binhex r,
           string "\n  ",
           string "   x = ", binhex x,
           string "   y = ", binhex y,
           string "   p = ", binhex p,
           string "  ma = ", binhex ma,
           string "  md = ", binhex md,
           string " cnd = ", bit cond,

-- Memory interface

         string "\n\nMemory\n  ",
           string " ctl_sto = ", bit ctl_sto,
           string "      m_sto = ", bit m_sto,
           string "\n  ",
           string "   m_addr = ", binhex m_addr,
           string "  m_real_addr = ", binhex m_real_addr,
           string "  m_data = ", binhex m_data,
           string "  m_out =", binhex m_out,
         string "\n\n",

-- .....................................................................
-- Higher level analysis of what happened on this cycle.  The
-- following actions examine various signals in order to detect what
-- is happening in the machine, and they print higher level
-- description.
-- .....................................................................

-- If a trap is being executed, indicate this in the simulation driver
-- state, so the driver can terminate the simulation

         fmtIf st_trap0
           [setStateWs setTrap [],
            string ("\n" ++ take 72 (repeat '*') ++ "\n"),
            string "Trap instruction executed\n",
            string "Simulation of Sigma16_M1 circuit terminating\n",
            string (take 72 (repeat '*') ++ "\n")
           ]
           [],

-- Print a message when the system is reset

         fmtIf reset
           [string ("\n" ++ take 72 (repeat '*') ++ "\n"),
            string "Reset: control algorithm starting",
            string ("\n" ++ take 72 (repeat '*'))]
           [],

-- When the displacement for an RX instruction is fetched, save
-- it in the simulation driver state

         fmtIf (orw [st_lea1, st_load1, st_store1,
                     st_jump1, st_jumpt1, st_jumpf1,st_jal1])
           [setStateWs setDisplacement [ad],
            string "Fetched displacement = ",
            simstate showDisplacement,
            string "\n"]
           [],

-- Record the effective address when it is calculated.  This is the r
-- output of the ALU, and usually will be loaded into the adr
-- register.

         fmtIf (orw [st_lea1, st_load1, st_store1, st_jump1,
                     st_jumpf1, st_jumpt1, st_jal1])
           [setStateWs setEffAddr [r]]
           [],

-- Say whether a conditional jump was performed

         fmtIf (and2 st_jumpt0 (inv cond))
           [string "jumpt instruction did not perform jump",
            setStateWs setJump [[zero], ad]]
           [],

         fmtIf st_jumpt1
           [string "jumpt instruction jumped",
            setStateWs setJump [[one], ad]]
           [],

         fmtIf (and2 st_jumpf0  cond)
           [string "jumpf instruction did not perform jump",
            setStateWs setJump [[zero], ad]]
           [],

         fmtIf st_jumpf1
           [string "jumpf instruction jumped",
            setStateWs setJump [[one], r]]
           [],

         fmtIf (or2 st_jump1 st_jal1)
           [
            setStateWs setJump [[one], r]]
           [],

-- Process a load to the register file
         fmtIf ctl_rf_ld
           [string "Register file update: ",
            string "R",
            bindec 1 (field ir 4 4),
            string " := ", hex p,
            setStateWs setRfLoad [field ir 4 4, p],
            string "\n"
            ]
           [],

-- Process a store to memory
         fmtIf ctl_sto
           [string "Memory store:  ",
            string "mem[",
            binhex m_addr,
            string "] := ", hex m_data,
            setStateWs setMemStore [m_addr, m_data]
           ]
           [],

-- If an instruction was completed during this clock cycle, fetch it,
-- decode it, and print it.  The first word of the instruction is in
-- ir, the second word (if it is an RX instruction) is in the
-- displacement field of the simulation driver state.

         fmtIf (and2 ctl_start (inv reset))
           [string ("\n" ++ take 72 (repeat '*') ++ "\n"),
            string "Executed instruction:  ",
            fmtWordsGeneral findMnemonic [field ir 0 4, field ir 12 4],
            string " ",
            fmtIf (orw [st_add, st_sub, st_cmpeq, st_cmplt, st_cmpgt])
              [string " R", bindec 1 (field ir 4 4),    -- RRR format
               string ",R", bindec 1 (field ir 8 4),
               string ",R", bindec 1 (field ir 12 4)]
              [string " R", bindec 1 (field ir 4 4),    -- RX format
               string ",",
               simstate showDisplacement,
               string "[R", bindec 1 (field ir 8 4), string "]",
               string "   effective address = ",
               simstate showEffAddr],
            string "\n",
            simstate showRfLoads,
            setStateWs clearRfLoads [],
            simstate showMemStores,
            setStateWs clearMemStores [],
            simstate showJumps,
            setStateWs clearJumps [],
            string "Processor state:  ",
            string "  pc = ", binhex pc,
            string "  ir = ", binhex ir,
            string "  ad = ", binhex ad,
            string ("\n" ++ take 72 (repeat '*') ++ "\n")]
--            simstate show  -- show the simulation driver state
           []
        ]

  in do putStrLn "\nSigma16_M1 simulation"
        runtp initDriverState termpred input simoutput


------------------------------------------------------------------------
-- Decoding instructions

-- When an instruction is decoded, findMnemonic returns the assembly
-- language mnemonic for the instruction, given the opcode.  The
-- opcode consists of the op field of the instruction, as well as the
-- sb field.  For RRR instructions, the op field determines the
-- instruction.  For RX instructions, the op is 15, which indicates an
-- escape to the sb field.

findMnemonic :: [[Int]] -> String
findMnemonic [opfield, bfield] =
  let op = intsInt opfield
      b = intsInt bfield
      mnemonics_RRR =
        ["add", "sub", "mul", "div",
         "cmplt", "cmpeq", "cmpgt", "inv",
         "and", "or", "xor", "shiftl",
         "shiftr", "trap", "expandXX", "expandRX"]
      mnemonics_RX =
        ["lea", "load", "store", "jump",
         "jumpf", "jumpt", "jal", "nop",
         "nop", "nop", "nop", "nop",
         "nop", "nop", "nop", "nop"]
  in if op==15
       then mnemonics_RX !! b
       else mnemonics_RRR !! op
