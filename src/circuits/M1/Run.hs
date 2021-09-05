-- Sigma16: M1/Run.hs
-- Copyright (C) 2020,2021 John T. O'Donnell
-- email: john.t.odonnell9@gmail.com
-- License: GNU GPL Version 3 or later
-- See Sigma16/COPYRIGHT.txt, Sigma16/LICENSE.txt

-- This file is part of Sigma16.  Sigma16 is free software: you can
-- redistribute it and/or modify it under the terms of the GNU General
-- Public License as published by the Free Software Foundation, either
-- version 3 of the License, or (at your option) any later version.
-- Sigma16 is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.  You should have received
-- a copy of the GNU General Public License along with Sigma16.  If
-- not, see <https://www.gnu.org/licenses/>.

----------------------------------------------------------------------
-- M1driver: Simulation driver for M1 implementation of Sigma16 ISA
----------------------------------------------------------------------


--------------------------------------------------------------------------------
-- See Installation section of the Sigma16 User Guide, and
-- Sigma16/src/circuits/README.txt

-- Usage
--   cd to src/circuits  -- must be in this directory
--   ghci                -- start ghci and initialize using .ghci
--   :main Simple/Add    -- run M1 circuit on examples/Core/Simple/Add.obj.txt
--   ^C                  -- stop and return to ghci prompt
--   :r                  -- reload, after you have changed a circuit source file
--   uparrow             -- repeat previous command
--   :q                  -- quit ghci, return to shell


--   :main batch Simple/Add
--   :main cli
--   :main cli inputdata.txt



{- This module defines simulation drivers that run the M1 circuit for
the Sigma16 instruction set architecture. A separate main program
should be defined that imports this module, and that defines the
machine language program.  It uses run_M1_program, defined below, to
execute the program. -}

--------------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}

module M1.Run where
import System.Environment
import System.FilePath

import Control.Monad.State

import Sigma16.ReadObj

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational
import M1.Circuit.Interface
import M1.Circuit.System

import Sigma16.ReadObj

--------------------------------------------------------------------------------
-- Main program
--------------------------------------------------------------------------------

main :: IO ()
main = do
  (op, operand) <- getCmd
  putStrLn ("operation = " ++ show op)
  case op of
    CmdOpBatch -> runM1 operand
    CmdOpCLI -> runDriver operand
    CmdOpEmpty -> return ()
  

runDriver :: String -> IO ()
runDriver xs = do
  putStrLn ("running simulationDriver " ++ xs)
--  execStateT simulationDriver initState
  return ()

--------------------------------------------------------------------------------
-- Simulation driver
--------------------------------------------------------------------------------

{-
simulationDriver :: StateT SysState IO ()
simulationDriver = do
  liftIO $ putStrLn "simulationDriver starting"
-- Input ports
  in_ld <- inPortBit "ld"
  in_x <- inPortBit "x"

-- Input signals  
  let ld = inbsig in_ld
  let x = inbsig in_x
  
-- Circuit defines output signals
  let (r,q) = myreg1 ld x

-- Output ports  
  out_r <- outPortBit "r" r
  out_q <- outPortBit "q" q

-- Run simulation


liftIO $ putStrLn "Enter command after prompt, h for help"
  commandLoop
  liftIO $ putStrLn "Simulation terminated"

-}


--------------------------------------------------------------------------------
-- old main run executable
--------------------------------------------------------------------------------

-- ghci
-- :main Simple/Add

runM1 :: String -> IO ()
runM1 fname = do
  putStrLn "M1 Run starting"
  let limit = 200
  liftIO $ putStrLn ("fname = " ++ show fname)
  let fnameExt = splitPath (fname ++ ".obj.txt")
  liftIO $ putStrLn ("fnameExt = " ++ show fnameExt)
  let corePath = ["..", "..", "examples", "Core"]
  let objParts = corePath ++ fnameExt
  liftIO $ putStrLn ("objParts = " ++ show objParts)
  let objpath = joinPath objParts
  liftIO $ putStrLn ("objpath = " ++ objpath)
  code <- readObject objpath
  putStrLn ("Object code = " ++ show code)
  run_Sigma16_executable code limit
  putStrLn "M1 Run finished"

--  code <- readObject fname
--  code <- liftIO $ readObject objpath

--------------------------------------------------------------------------------
-- Boot
--------------------------------------------------------------------------------

{-
boot :: StateT SysState IO [Int]
boot = do
-- Read the command arguments
  liftIO $ putStrLn "booting..."
  s <- get
  let xs = args s
  let fname = splitPath (head xs ++ ".obj.txt")
  liftIO $ putStrLn ("fname = " ++ show fname)
  let corePath = ["..", "..", "examples", "Core"]
  let objParts = corePath ++ fname
  liftIO $ putStrLn ("objParts = " ++ show objParts)
  let objpath = joinPath objParts
  liftIO $ putStrLn ("fname = " ++ show fname)
  liftIO $ putStrLn ("objpath = " ++ objpath)
  code <- liftIO $ readObject objpath
  return code
-}

----------------------------------------------------------------------
-- Simulation model
----------------------------------------------------------------------

{- The simulations are carried out using the Stream Bool model.  The
types Bit and Word are defined for clarity; a Bit is represented as a
Stream of Bools for the simulations. -}

----------------------------------------------------------------------
-- Running a machine language program
----------------------------------------------------------------------

{- The run_M1_program function takes a program (in the form of a list
of strings), loads it into the machine, and executes it, using the
sim_m1 simulation driver. -}

-- prog is a list of hex strings giving object code
run_Sigma16_program :: [String] -> Int -> IO ()
run_Sigma16_program prog n =
  let inps = zipWith f [0..] prog ++ [[1,0,0,0]] ++ repeat [0,0,0,0]
      f i x = [0, 1, i, hexbin 16 x]
      m = n + length prog
  in  sim_m1 (take m inps)

-- prog is a list of ints giving object code
run_Sigma16_executable :: [Int] -> Int -> IO ()
run_Sigma16_executable prog n = do
  let f i x = [0, 1, i, x]
  let inps = zipWith f [0..] prog ++ [[1,0,0,0]] ++ repeat [0,0,0,0]
  let m = n + length prog
  putStrLn ("run executable limit = " ++ show m)
  putStrLn ("input = " ++ show (take m inps))
  sim_m1 (take m inps)

----------------------------------------------------------------------
-- Simulation driver for the M1 system
----------------------------------------------------------------------

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

mytermpred :: (Int,DriverState) -> Bool
mytermpred (c,s) = trap s || c > 1000

-- The simulation driver is sim_m1.

sim_m1 :: [[Int]] -> IO ()
sim_m1 input =
  let

-- Get the system inputs from the "input" argument.

      reset = getbit    input 0  -- :: Bit
      dma   = getbit    input 1  -- :: Bit
      dma_a = getbin 16 input 2  -- :: Word
      dma_d = getbin 16 input 3  -- :: Word

-- Apply the circuit m1_system to the system inputs, and obtain its
-- outputs.

      (ctlstate, ctl_start, ctlsigs, dp,
       m_sto, m_addr, m_real_addr, m_data, m_out)
         = m1 reset dma dma_a dma_d
      
-- Define names for the outputs from the datapath.

--      (r,ccnew,condcc) = aluOutputs dp
--    (ir_op,ir_d,ir_a,ir_b) = irFields

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

         string " st_instr_fet = ", bit (st_instr_fet ctlstate),
         string "  st_dispatch = ", bit (st_dispatch ctlstate),
         string "       st_add = ", bit (st_add ctlstate),
         string "       st_sub = ", bit (st_sub ctlstate),
         string "\n  ",
         string "      st_mul0 = ", bit (st_mul0 ctlstate),
         string "      st_cmp = ",  bit (st_cmp ctlstate),
         string "\n  ",
         string "     st_trap0 = ", bit (st_trap0 ctlstate),
         string "      st_lea0 = ", bit (st_lea0 ctlstate),
         string "      st_lea1 = ", bit (st_lea1 ctlstate),
         string "     st_load0 = ", bit (st_load0 ctlstate),
         string "\n  ",
         string "     st_load1 = ", bit (st_load1 ctlstate),
         string "     st_load2 = ", bit (st_load2 ctlstate),
         string "    st_store0 = ", bit (st_store0 ctlstate),
         string "    st_store1 = ", bit (st_store1 ctlstate),
         string "\n  ",
         string "    st_store2 = ", bit (st_store2 ctlstate),
         string "     st_jump0 = ", bit (st_jump0 ctlstate),
         string "     st_jump1 = ", bit (st_jump1 ctlstate),
         string "     st_jump2 = ", bit (st_jump2 ctlstate),
         string "\n  ",
         string "     st_jumpc00 = ", bit (st_jumpc00 ctlstate),
         string "     st_jumpc01 = ", bit (st_jumpc01 ctlstate),
         string "     st_jumpc02 = ", bit (st_jumpc02 ctlstate),

         string "     st_jumpc10 = ", bit (st_jumpc10 ctlstate),
         string "     st_jumpc11 = ", bit (st_jumpc11 ctlstate),
         string "     st_jumpc12 = ", bit (st_jumpc12 ctlstate),

         string "\n  ",
         string "      st_jal0 = ", bit (st_jal0 ctlstate),
         string "\n  ",
         string "      st_jal1 = ", bit (st_jal1 ctlstate),
         string "      st_jal2 = ", bit (st_jal2 ctlstate),

         string "\n\nControl signals\n  ",
           string "  ctl_alu_a   = ", bit (ctl_alu_a ctlsigs),
           string "  ctl_alu_b   = ", bit (ctl_alu_b ctlsigs),
           string "\n  ",
           string "  ctl_x_pc    = ", bit (ctl_x_pc ctlsigs),
           string "  ctl_y_ad    = ", bit (ctl_y_ad ctlsigs),
           string "\n  ",
           string "  ctl_rf_ld   = ", bit (ctl_rf_ld ctlsigs),
           string "  ctl_rf_ldcc = ", bit (ctl_rf_ldcc ctlsigs),
           string "  ctl_rf_pc   = ", bit (ctl_rf_pc ctlsigs),
           string "\n  ",
           string "  ctl_pc_ld   = ", bit (ctl_pc_ad ctlsigs),
           string "  ctl_pc_ad   = ", bit (ctl_pc_ad ctlsigs),
           
           string "\n  ",
           string "  ctl_rf_alu  = ", bit (ctl_rf_alu ctlsigs),
           string "  ctl_rf_sd   = ", bit (ctl_rf_sd ctlsigs),
           string "  ctl_ir_ld   = ", bit (ctl_ir_ld ctlsigs),
           string "  ctl_pc_ld   = ", bit (ctl_pc_ld ctlsigs),
           string "\n  ",
           string "  ctl_ad_ld   = ", bit (ctl_ad_ld ctlsigs),
           string "  ctl_ad_alu  = ", bit (ctl_ad_alu ctlsigs),
           string "  ctl_ma_pc   = ", bit (ctl_ma_pc ctlsigs),
           string "  ctl_sto     = ", bit (ctl_sto ctlsigs),

         string "\n\nDatapath\n  ",
           string "  ir = ", binhex (ir dp),
           string "  pc = ", binhex (pc dp),
           string "  ad = ", binhex (ad dp),
           string "   a = ", binhex (a dp),
           string "   b = ", binhex (b dp),
           string "   r = ", binhex (r dp),
           string "\n  ",
           string "   x = ", binhex (x dp),
           string "   y = ", binhex (y dp),
           string "   p = ", binhex (p dp),
           string "  ma = ", binhex (ma dp),
           string "  md = ", binhex (md dp),
           string "\n  ",
           string "  cc = ", binhex (cc dp),
           string " condcc = ", bit (condcc dp),

-- Memory interface

         string "\n\nMemory\n  ",
           string " ctl_sto = ", bit (ctl_sto ctlsigs),
           string "      m_sto = ", bit m_sto,
           string "\n  ",
           string "   m_addr = ", binhex m_addr,
           string "  m_real_addr = ", binhex m_real_addr,
           string "  m_data = ", binhex m_data,
           string "  m_data = ", bits m_data,
--           string "  m_out =", binhex m_out,
           string "  m_out =", bits m_out,
           string "  m_real_addr = ", binhex m_real_addr,
           string "\n",
           string "  reset = ", bit reset,
           string "  dma = ", bit dma,
           string "  dma_a = ", bits dma_a,
           string "  dma_d = ", bits dma_d,
           string "\n",
           string "  md = ", bits (md dp),
           string "  dma_d = ", bits dma_d,
           string "\n",
           string "  m_sto = ", bit m_sto,
           string "  m_real_addr = ", bits m_real_addr,
           string "  m_data = ", bits m_data,
           string "  m_out = ", bits m_out,
           string "  q = ", bits (q dp),
           string "\n",
           string "ALU inputs: ",
           string "  x = ", bits (x dp),
           string "  cc = ", bits (cc dp),
           string "  ir_d = ", bits (ir_d dp),
           string "\n",
           string "ALU outputs: ",
           string "  r = ", bits (r dp),
           string "  ccnew = ", bits (ccnew dp),
           string "  condcc = ", bit (condcc dp),
           string "\n\n",

-- ...................................................................
-- Higher level analysis of what happened on this cycle.  The
-- following actions examine various signals in order to detect what
-- is happening in the machine, and they print higher level
-- description.
-- ...................................................................

-- If a trap is being executed, indicate this in the simulation driver
-- state, so the driver can terminate the simulation

         fmtIf (st_trap0 ctlstate)
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

         fmtIf (orw [st_lea1 ctlstate, st_load1 ctlstate, st_store1 ctlstate,
                     st_jump1 ctlstate, st_jumpc01 ctlstate, st_jumpc11 ctlstate,
                     st_jal1 ctlstate])
           [setStateWs setDisplacement [(ad dp)],
            string "Fetched displacement = ",
            simstate showDisplacement,
            string "\n"]
           [],

-- Record the effective address when it is calculated.  This is the r
-- output of the ALU, and usually will be loaded into the ad register.

         fmtIf (orw [st_lea1 ctlstate, st_load1 ctlstate, st_store1 ctlstate,
                     st_jump1 ctlstate, st_jumpc01 ctlstate, st_jumpc11 ctlstate,
                     st_jal1 ctlstate])
           [setStateWs setEffAddr [r dp]]
           [],

-- Say whether a conditional jump was performed

         fmtIf (and2 (st_jumpc00 ctlstate)  (inv (condcc dp)))
           [string "jumpc0 instruction did not perform jump",
            setStateWs setJump [[zero], ad dp]]
           [],

         fmtIf (and2 (st_jumpc10 ctlstate)  (condcc dp))
           [string "jumpc1 instruction jumped",
            setStateWs setJump [[one], r dp]]
           [],

         fmtIf (or2 (st_jump2 ctlstate) (st_jal2 ctlstate))
           [
            setStateWs setJump [[one], r dp]]
           [],
         
-- Process a load to the register file
         fmtIf (ctl_rf_ld ctlsigs)
           [string "Register file update: ",
            string "R",
            bindec 1 (field (ir dp) 4 4),
            string " := ", hex (p dp),
            setStateWs setRfLoad [field (ir dp) 4 4, (p dp)],
            string "\n"
            ]
           [],

-- Process a store to memory
         fmtIf (ctl_sto ctlsigs)
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
            fmtWordsGeneral findMnemonic [field (ir dp) 0 4, field (ir dp) 12 4],
            string " ",
            fmtIf (orw [st_add ctlstate, st_sub ctlstate, st_cmp ctlstate])

              [string " R", bindec 1 (field (ir dp) 4 4),    -- RRR format
               string ",R", bindec 1 (field (ir dp) 8 4),
               string ",R", bindec 1 (field (ir dp) 12 4)]
              [string " R", bindec 1 (field (ir dp) 4 4),    -- RX format
               string ",",
               simstate showDisplacement,
               string "[R", bindec 1 (field (ir dp) 8 4), string "]",
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
            string "  pc = ", binhex (pc dp),
            string "  ir = ", binhex (ir dp),
            string "  ad = ", binhex (ad dp),

            string ("\n" ++ take 72 (repeat '*') ++ "\n")]
--            simstate show  -- show the simulation driver state
           [],
         
           string "\ndone\n\n"
        ]
  in do putStrLn "\nSigma16_M1 simulation"
        runUntil initDriverState mytermpred input simoutput

----------------------------------------------------------------------
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
         "cmp", "nop", "nop", "nop",
         "nop", "nop", "nop", "trap",
         "nop", "nop", "expandExp", "expandRX"]
      mnemonics_RX =
        ["lea",    "load",   "store", "jump",
         "jumpc0", "jumpc1", "jal",   "nop",
         "nop",    "nop",    "nop",   "nop",
         "nop",    "nop",    "nop",   "nop"]
  in if op==15
       then mnemonics_RX !! b
       else mnemonics_RRR !! op
