-- Sigma16 M1.Run, Simulation driver for M1 circuit implementation of Sigma16 ISA
-- Copyright (C) 2021 John T. O'Donnell.  This file is part of Sigma16.
-- See Sigma16/README and https://jtod.github.io/home/Sigma16/

-- Usage:
--   cd to src/circuits  -- must be in this directory
--   ghci                -- start ghci and initialize using .ghci
--   :load M1/Run        -- run M1 circuit on examples/Core/Simple/Add.obj.txt
--   :main Simple/Add    -- run M1 circuit on examples/Core/Simple/Add.obj.txt
--   ^C                  -- stop and return to ghci prompt
--   :r                  -- reload, after you have changed a circuit source file
--   uparrow             -- repeat previous command
--   :q                  -- quit ghci, return to shell

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module M1.Run where

import HDL.Hydra.Core.Lib
import M1.ReadObj
import M1.Circuit.System

import System.Environment
import System.IO
import Control.Monad.State
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- Main program
--------------------------------------------------------------------------------


getObject :: IO [Int]
getObject = do
  args <- getArgs
  putStrLn (show args)
  case args of
    [] -> do
      putStrLn "Usage: :main path/to/objectfile"
      return []
    (a:_) -> do
      let fname = a ++ ".obj.txt"
      putStrLn ("Reading object file " ++ fname)
      code <- liftIO $ readObjectCode fname
      putStrLn ("Object coode is " ++ show code)
      return code
  
-- Generate control signals to boot object code
bootInputs :: [Int] -> [String]
bootInputs code =
--  code <- readObjectFile
  let f i x = "0 1 1 0 0 " ++ show i ++ " " ++ show x
      inps = zipWith f [0..] code
  in inps

putStoredInput :: [String] -> StateT (SysState a) IO ()
putStoredInput storedInput = do
  s <- get
  put $ s {storedInput}
  
main :: IO ()
main = driver $ do
  printLine "M1 computer circuit starting"
--  initialize
--  defineCommand "boot" cmdBoot
--  defineCommand "reset" cmdReset
--  defineCommand "step" cmdM1ClockCycle
  objectCode <- liftIO getObject
  liftIO $ putStrLn ("Object = " ++ show objectCode)
  let bootData = bootInputs objectCode
  putStoredInput bootData
  liftIO $ putStrLn ("Boot input data = " ++ show bootData)
--  return ()
--  bootData <- liftIO bootInputs
  printLine ("Boot system inputs = " ++ show bootData)
--  storeInputList "Booting" bootData
--  storeInputList "Resetting" resetData
--  storeInputList "Running" runData
--  selectInputList "Booting"

  -- Input ports
  in_reset        <- inPortBit  "reset"
  in_io_DMA       <- inPortBit  "io_DMA"
  in_io_memStore  <- inPortBit  "io_memStore"
  in_io_memFetch  <- inPortBit  "io_memFetch"
  in_io_regFetch  <- inPortBit  "io_regFetch"
  in_io_address   <- inPortWord "io_address" 16
  in_io_data      <- inPortWord "io_data" 16

  -- Input signals
  let reset         = inbsig in_reset
  let io_DMA        = inbsig in_io_DMA
  let io_memStore   = inbsig in_io_memStore
  let io_memFetch   = inbsig in_io_memFetch
  let io_regFetch   = inbsig in_io_regFetch
  let io_address    = inwsig in_io_address
  let io_data       = inwsig in_io_data
  let io = SysIO {..}

-- The M1 circuit
  let  (CtlState {..}, ctl_start, (CtlSig {..}), dp,
        m_sto, m_addr, m_real_addr, m_data, m_out)
         = m1 reset io

  setPeek m_out
  setPeek (b dp)
  
  let ctlStateLookupTable =
        [ ("st_instr_fet",  st_instr_fet)
        , ("st_dispatch",   st_dispatch)
        , ("st_add",        st_add)
        , ("st_sub",        st_sub)
        , ("st_mul0",       st_mul0)
        , ("st_div0",       st_div0)
        , ("st_cmp",        st_cmp)
        , ("st_trap0",      st_trap0)
        , ("st_lea0",       st_lea0)
        , ("st_load0",      st_load0)
        , ("st_store0",     st_store0)
        , ("st_jump0",      st_jump0)
        , ("st_jumpc00",    st_jumpc00)
        , ("st_jumpc10",    st_jumpc10)
        , ("st_jal0",       st_jal0)
        ]
  let flags = ctlStateLookupTable
  setFlagTable flags

-- Define names for subsystem outputs
  let (r,ccnew,condcc) = aluOutputs dp
--    (ir_op,ir_d,ir_a,ir_b) = irFields

-- Format the output

  format
    [ string "\nSystem control\n"
    , string "  reset = ", bit reset
--    , string "  cpu = ", bit (cpu ctlsigs)
    , string "  cpu = ", bit cpu
    , string "  ctl_start = ", bit ctl_start
    , string "\n"
    , string "\nInput/Output\n"
    , string "  io_DMA = ", bit io_DMA
    , string "  io_memStore = ", bit io_memStore
    , string "  io_memFetch = ", bit io_memFetch
    , string "  io_regFetch = ", bit io_regFetch
    , string "\n"
    , string "  io_address = ", binhex io_address
    , string "  io_data = ", binhex io_data
--    , string "  md = ", bits (md dp)
    , string "\n"
    , string "\nControl state\n  "
    , string " st_instr_fet = ", bit dff_instr_fet, bit st_instr_fet
    , string "  st_dispatch = ", bit dff_dispatch, bit st_dispatch
    , string "       st_add = ", bit dff_add, bit st_add
    , string "       st_sub = ", bit dff_sub, bit st_sub
    , string "\n  "
    , string "      st_mul0 = ", bit dff_mul0, bit st_mul0
    , string "      st_div0 = ", bit dff_div0, bit st_div0
    , string "       st_cmp = ", bit dff_cmp, bit st_cmp
    , string "     st_trap0 = ", bit dff_trap0, bit st_trap0
    , string "\n  "
    , string "      st_lea0 = ", bit dff_lea0, bit st_lea0
    , string "      st_lea1 = ", bit dff_lea1, bit st_lea1
    , string "      st_lea2 = ", bit dff_lea2, bit st_lea2
    , string "     st_load0 = ", bit dff_load0, bit st_load0
    , string "\n  "
    , string "     st_load1 = ", bit dff_load1, bit st_load1
    , string "     st_load2 = ", bit dff_load2, bit st_load2
    , string "    st_store0 = ", bit dff_store0, bit st_store0
    , string "    st_store1 = ", bit dff_store1, bit st_store1
    , string "\n  "
    , string "    st_store2 = ", bit dff_store2, bit st_store2
    , string "     st_jump0 = ", bit dff_jump0, bit st_jump0
    , string "     st_jump1 = ", bit dff_jump1, bit st_jump1
    , string "     st_jump2 = ", bit dff_jump2, bit st_jump2
    , string "\n  "
    , string "   st_jumpc00 = ", bit dff_jumpc00, bit st_jumpc00
    , string "   st_jumpc01 = ", bit dff_jumpc01, bit st_jumpc01
    , string "   st_jumpc02 = ", bit dff_jumpc02, bit st_jumpc02
    , string "   st_jumpc10 = ", bit dff_jumpc10, bit st_jumpc10
    , string "\n  "
    , string "   st_jumpc11 = ", bit dff_jumpc11, bit st_jumpc11
    , string "   st_jumpc12 = ", bit dff_jumpc12, bit st_jumpc12
    , string "      st_jal0 = ", bit dff_jal0, bit st_jal0
    , string "      st_jal1 = ", bit dff_jal1, bit st_jal1
    , string "\n  "
    , string "      st_jal2 = ", bit dff_jal2, bit st_jal2

    , string "\n\nControl signals\n  "
    , string "    ctl_alu_a = ", bit ctl_alu_a
    , string "    ctl_alu_b = ", bit ctl_alu_b
    , string "     ctl_x_pc = ", bit ctl_x_pc
    , string "     ctl_y_ad = ", bit ctl_y_ad
    , string "\n  "
    , string "    ctl_rf_ld = ", bit ctl_rf_ld
    , string "  ctl_rf_ldcc = ", bit ctl_rf_ldcc
    , string "    ctl_rf_pc = ", bit ctl_rf_pc
    , string "    ctl_pc_ld = ", bit ctl_pc_ad
    , string "\n  "
    , string "    ctl_pc_ad = ", bit ctl_pc_ad
    , string "   ctl_rf_alu = ", bit ctl_rf_alu
    , string "    ctl_rf_sd = ", bit ctl_rf_sd
    , string "    ctl_ir_ld = ", bit ctl_ir_ld
    , string "\n  "
    , string "    ctl_pc_ld = ", bit ctl_pc_ld
    , string "    ctl_ad_ld = ", bit ctl_ad_ld
    , string "   ctl_ad_alu = ", bit ctl_ad_alu
    , string "    ctl_ma_pc = ", bit ctl_ma_pc
    , string "\n  "
    , string "      ctl_sto = ", bit ctl_sto

     , string "\n\nALU\n"
     , string "  ALU inputs: "
     , string "  x = ", binhex (x dp)
     , string "  y = ", binhex (y dp)
     , string "  cc = ", binhex (cc dp)
     , string "  ir_d = ", binhex (ir_d dp)
     , string "\n  ALU outputs: "
     , string "  r = ", binhex r
     , string "  ccnew = ", binhex ccnew
     , string "  condcc = ", bit condcc
     
     , string "\n\nDatapath\n  "
     , string "    ir = ", binhex (ir dp)
     , string "    pc = ", binhex (pc dp)
     , string "    ad = ", binhex (ad dp)
     , string "    cc = ", binhex (cc dp)
     , string "\n  "
     , string "     a = ", binhex (a dp)
     , string "     b = ", binhex (b dp)
     , string "     x = ", binhex (x dp)
     , string "     y = ", binhex (y dp)
     , string "\n  "
     , string "     p = ", binhex (p dp)
     , string "     q = ", binhex (q dp)
     , string "     r = ", binhex (r)
     , string "\n  "
     , string "    ma = ", binhex (ma dp)
     , string "    md = ", binhex (md dp)

-- Memory interface
     , string "\n\nMemory\n  "
     , string "  m_sto = ", bit m_sto
     , string "  m_addr = ", binhex m_addr
     , string "  m_real_addr = ", binhex m_real_addr
     , string "  m_data = ", binhex m_data
     , string "  m_out =", binhex m_out
     , string "\n"

-- ...................................................................
-- Higher level analysis of what happened on this cycle.  The
-- following actions examine various signals in order to detect what
-- is happening in the machine, and they print higher level
-- description.
-- ...................................................................

-- Print a message when the system is reset

     ,  fmtIf reset
           [string ("\n" ++ take 72 (repeat '*') ++ "\n"),
            string "Reset: control algorithm starting",
            string ("\n" ++ take 72 (repeat '*'))]
           [],

-- When the displacement for an RX instruction is fetched, save
-- it in the simulation driver state

         fmtIf (orw [st_lea1, st_load1, st_store1, st_jump1, st_jumpc01,
                     st_jumpc11, st_jal1])
           [setStateWs setDisplacement [(ad dp)],
            string "Fetched displacement = ",
            simstate showDisplacement,
            string "\n"]
           [],

-- Record the effective address when it is calculated.  This is the r
-- output of the ALU, and usually will be loaded into the ad register.

         fmtIf (orw [st_lea1, st_load1, st_store1, st_jump1,
                     st_jumpc01, st_jumpc11, st_jal1])
         [setStateWs setEffAddr [r]]
           [],

-- Say whether a conditional jump was performed

--         fmtIf (and2 (st_jumpc00 ctlstate)  (inv (condcc dp)))
         fmtIf (and2 st_jumpc00  (inv condcc))
           [string "jumpc0 instruction did not perform jump",
            setStateWs setJump [[zero], ad dp]]
           [],

--         fmtIf (and2 (st_jumpc10 ctlstate)  (condcc dp))
         fmtIf (and2 st_jumpc10 condcc)
           [string "jumpc1 instruction jumped",
--            setStateWs setJump [[one], r dp]]
             setStateWs setJump [[one], r]]
           [],

         fmtIf (or2 st_jump2 st_jal2)
           [
--            setStateWs setJump [[one], r dp]]
             setStateWs setJump [[one], r]]
           [],
         
-- Process a load to the register file
         fmtIf ctl_rf_ld
           [string "Register file update: ",
            string "R",
            bindec 1 (field (ir dp) 4 4),
            string " := ", hex (p dp),
            setStateWs setRfLoad [field (ir dp) 4 4, (p dp)],
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
            fmtWordsGeneral findMnemonic [field (ir dp) 0 4, field (ir dp) 12 4],
            string " ",
            fmtIf (orw [st_add, st_sub, st_cmp])

              [string " R", bindec 1 (field (ir dp) 4 4),    -- RRR format
               string ",R", bindec 1 (field (ir dp) 8 4),
               string ",R", bindec 1 (field (ir dp) 12 4)]
              [string " R", bindec 1 (field (ir dp) 4 4),    -- RX format
               string ",",
               simstate showDisplacement,
               string "[R", bindec 1 (field (ir dp) 8 4), string "]" ], -- ,
--               string "   effective address = ",
--               simstate showEffAddr],
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
         
-- If a trap is being executed, indicate this in the simulation driver
-- state, so the driver can terminate the simulation

         fmtIf st_trap0
           [setStateWs setTrap [],
            setHalted,
            string ("\n" ++ take 72 (repeat '*') ++ "\n"),
            string "Trap Halt instruction executed\n",
            string "Processor has halted\n",
            string (take 72 (repeat '*') ++ "\n")
           ]
           [],
      string "\n"
    ]

{-
--  simoutput <- format "CPUstep"
    [
     clockcycle (\c -> take 72 (repeat '.') ++
                  "\nClock cycle " ++ show c ++ "\n"),
-}

  -- This ends definitions of the tools; the driver algorithms starts
  -- now
  
  printLine "\nSigma16_M1 simulation"
--  runM1simulation
  startup
  printLine "M1 Run finished"


--------------------------------------------------------------------------------
-- Top level M1 control
--------------------------------------------------------------------------------

conditional :: Bool -> StateT (SysState a) IO ()
  -> StateT (SysState a) IO ()
conditional b op =
  case b of
    True -> do op
               return ()
    False -> return ()
    

peekReg :: Int -> StateT (SysState DriverState) IO ()
peekReg regnum = do
  s <- get
  let displayFullSignals = False
  let i = cycleCount s
  let inp = inPortList s
  let outp = outPortList s
  conditional displayFullSignals $ do
    liftIO $ putStrLn (take 80 (repeat '-'))
    liftIO $ putStr ("Cycle " ++ show i)
    liftIO $ putStrLn (" ***** Peek at register R" ++ show regnum)
  let inps = "0 1 0 0 1 " ++ show regnum ++ " 0"
  conditional displayFullSignals $ do
    liftIO $ putStrLn ("inps = " ++ inps)
  takeInputsFromList inps
  conditional displayFullSignals $ do
    printInPorts
    printOutPorts
    runFormat
  s <- get
  let ps = peekList s
  let b = ps!!1  --  output b from regfile
  let bs = map current b
--  liftIO $ putStrLn ("Register value = " ++ show bs)
  liftIO $ putStrLn ("***** Peek R" ++ show regnum ++ " = " ++ bitsHex 4 bs)
  advanceInPorts
  advanceOutPorts
  advancePeeks
  s <- get
  let i = cycleCount s
  put (s {cycleCount = i + 1})

data ProcessorMode
  = Idle
  | Booting
  | Resetting
  | Running
  deriving (Eq, Read, Show)

startup :: StateT (SysState DriverState) IO ()
startup = do
  s <- get
  put (s {userState = Just initDriverState})
  setMode Booting
  commandLoop

commandLoop :: StateT (SysState DriverState) IO ()
commandLoop = do
  liftIO $ putStr "Sigma16.M1> "
  liftIO $ hFlush stdout
  xs <- liftIO getLine
  let ws = words xs
  if length ws == 0
    then m1ClockCycle
  else if ws!!0 == "help"
    then printHelp
  else if ws!!0 == "run"
    then runM1simulation
  else if ws!!0 == "cycle"
    then m1ClockCycle
  else if ws!!0 == "mem"
    then do
      let start = safeReadEltInt ws 1
      let end = safeReadEltInt ws 2
      dumpMem start end
  else if ws!!0 == "regs"
    then dumpRegFile
  else if ws!!0 == "break"
    then do
      let key = safeReadEltString ws 1
      setBreakpoint key
  else if ws!!0 == "quit"
    then do
      s <- get
      put (s {running = False})
  else liftIO $ putStrLn "Invalid command, enter help for list of commands"
  s <- get
  case running s of
    True -> commandLoop
    False -> return ()

safeReadEltInt :: [String] -> Int -> Int
safeReadEltInt ws i =
  if length ws > i && i >= 0
    then read (ws!!i)
    else 0

safeReadEltString :: [String] -> Int -> String
safeReadEltString ws i =
  if length ws > i && i >= 0
    then ws!!i
    else ""

setBreakpoint :: String -> StateT (SysState a) IO ()
setBreakpoint key = do
  s <- get
  put $ s {breakpointKey = key}
  
-- Dump memory from start to end address

dumpMem :: Int -> Int -> StateT (SysState DriverState) IO ()
dumpMem start end = do
  case start <= end of
    True -> do
      peekMem start
      dumpMem (start+1) end
    False -> return ()

peekMem :: Int -> StateT (SysState DriverState) IO ()
peekMem addr = do
  s <- get
  let displayFullSignals = False
  let i = cycleCount s
  let inp = inPortList s
  let outp = outPortList s
  conditional displayFullSignals $
    liftIO $ putStrLn (take 80 (repeat '-'))
  liftIO $ putStr ("Cycle " ++ show i ++ ".  ")
--  liftIO $ putStrLn (" ***** Peek at memory address " ++ show addr)
  let inps = "0 1 0 1 0 " ++ show addr ++ " 0"
  takeInputsFromList inps
--  printInPorts
--  printOutPorts
--  runFormat
  s <- get
  let ps = peekList s
  let a = ps!!0  -- m_out
  let bs = map current a
  liftIO $ putStrLn ("***** Peek Mem[" ++ show addr ++ "] = " ++ bitsHex 4 bs)
  advanceInPorts
  advanceOutPorts
  advancePeeks
  -- advanceFormat ????? need to do this silently
  s <- get
  let i = cycleCount s
  put (s {cycleCount = i + 1})

dumpRegFile :: StateT (SysState DriverState) IO ()
dumpRegFile = do
  let f i =
        case i <= 15 of
          True -> do
            peekReg i
            f (i+1)
          False -> return ()
  f 0

{-
peekRegFile = do
  liftIO $ putStrLn "peekRegFile"
  peekReg 0
  peekReg 1
  peekReg 2
  peekReg 3
  peekReg 4
  peekReg 5
  peekReg 6
  peekReg 7
-}

printHelp :: StateT (SysState a) IO ()
printHelp = do
  printLine "Commands for the M1 driver"
  printLine "  (blank)     -- perform one clock cycle"
  printLine "  cycle       -- perform one clock cycle"
  printLine "  run         -- perform clock cycles repeatedly until halt or break"
  printLine "  regs        -- display contents of the register file"
  printLine "  mem a b     -- display memory from address a to b"
  printLine "  break FLAG  -- run will stop when FLAG signal is 1 (see list below)"
  printLine "  quit        -- return to ghci or shell prompt"
  printLine "  help        -- list the commands"
  printLine "The following signals can be used as break FLAG:"
  s <- get
  printLine (concat (map ((' ':) . fst) (flagTable s)))
  
runM1simulation :: StateT (SysState DriverState) IO ()
runM1simulation = do
  printLine "runM1simulation starting"
  simulationLooper
--  printLine "runM1simulation terminated"

simulationLooper :: StateT (SysState DriverState) IO ()
simulationLooper = do
  s <- get
  if checkFlag (flagTable s) (breakpointKey s) || cycleCount s >= 1000
    then do
      cycle <- getClockCycle
      m1ClockCycle -- display the cycle where the breakpoint is satisfied
      liftIO $ putStrLn (take 72 (repeat '-'))
      liftIO $ putStrLn ("*** Breakpoint " ++ breakpointKey s
                         ++ " in cycle " ++ show cycle ++ " ***")
      liftIO $ putStrLn (take 72 (repeat '-'))
      return ()
    else do m1ClockCycle
            s <- get
            case halted s of
              True -> do
                printLine "Processor has halted"
                return ()
              False -> simulationLooper
            
--  doStep []
--  runSimulation
--  runUntil initDriverState mytermpred input simoutput

-- An interactive command could request something unusual (such as a
-- register dump).  However, usually the user will wish to establishM1inputs to
-- the next cycle using the normal inputs.  This is performed by
-- establishM1inputs: perform the next action if the current input list is not
-- exhausted; otherwise go to the next mode

-- If an input list is being consumed, establishM1inputs continues as long as
-- there is data.  When the input list is exhaused, it goes to the
-- next normal mode.

-- DMA is a special case.  A DMA operation may take a number of clock
-- cycles, and it interrupts normal execution of the processor.  After
-- the DMA is finished, the previous mode is resumed.  When a DMA
-- begins, the processor mode is saved in stolenMode.  A DMA operation
-- will always run to completion; it cannot be interrupted by yet
-- another DMA.

cmdM1ClockCycle :: Command DriverState
cmdM1ClockCycle _ = m1ClockCycle

getProcessorMode :: StateT (SysState DriverState) IO ProcessorMode
getProcessorMode = do
  s <- get
  let mds = userState s
  case mds of
    Nothing -> do printLine "DriverState not defined"
                  return Idle
    Just ds -> return (processorMode ds)

{-
getProcessorMode :: StateT (SysState DriverState) IO ProcessorMode
getProcessorMode = do
--  s <- get
--  let Just ds = userState s
--  printLine "getProcessorMode about to get user state"
  mds <- getUserState
  case mds of
    Just ds -> do
--      printLine "getProcessorMode back from get user state"
      printLine (show (processorMode ds))
      return (processorMode ds)
    Nothing -> do
      printError "getProcessorMode, getUserState returned Nothing, default Running"
      return Running
-}

m1ClockCycle :: Operation DriverState
m1ClockCycle = do
  liftIO $ putStrLn ("m1ClockCycle starting")
  s <- get
  let i = cycleCount s
  let inp = inPortList s
  let outp = outPortList s
  liftIO $ putStrLn (take 80 (repeat '-'))
  liftIO $ putStr ("Cycle " ++ show i ++ ".  ")
  establishM1inputs
  pm <- getProcessorMode
  liftIO $ putStr (show pm)
  liftIO $ putStrLn (if halted s then "  Halted" else "")
--  printInPorts
--  printOutPorts
  runFormat
  advanceInPorts
  advanceOutPorts
  advancePeeks
  advanceFlagTable
  s <- get
  let i = cycleCount s
  put (s {cycleCount = i + 1})
  liftIO $ putStrLn ("m1ClockCycle finished")


getCurrentInputs :: StateT (SysState DriverState) IO (Maybe String)
getCurrentInputs = do
  return $ Just "0 0 0 0 0 0 0"

establishM1inputs :: StateT (SysState DriverState) IO ()
establishM1inputs = do
  mds <- getUserState
  case mds of
    Nothing -> do
      printError "establishM1inputs: empty driver state"
      return ()
    Just ds ->
      case processorMode ds of
        Booting  -> do
          inp <- getStoredInput
          case inp of
            Just x -> takeInputsFromList x
            Nothing -> do
              setMode Resetting
              establishM1inputs
        Resetting -> do
          takeInputsFromList resettingInputs
          setMode Running
        Running -> takeInputsFromList runningInputs

resettingInputs = "1 0 0 0 0 0 0"
runningInputs   = "0 0 0 0 0 0 0"
  
{-
establishM1inputs :: StateT (SysState DriverState) IO ()
establishM1inputs = do
  printLine "establishM1inputs"
--  mx <- getInputList
--  printLine ("establish mx = " ++ show mx)
--  case mx of
  s <- get
  inp <- getStoredInput
--  inps <- getCurrentInputs
  liftIO $ putStrLn ("establish - " ++ show inp)
  case inp of
    Just x -> do -- use the data x to fill buffers
--      printLine ("establishM1inputs using " ++ x)
      takeInputsFromList x
      return ()
    Nothing -> do -- look at mode to decide what to do
      oldmode <- getProcessorMode
--      printLine ("establishM1inputs current processor mode = " ++ show oldmode)
      mds <- getUserState
      case mds of
        Just ds -> do
          case processorMode ds of
            Idle      -> setMode Booting
            Booting   -> setMode Resetting
            Resetting -> setMode Running
            Running   -> setMode Running
--          newMode <- getProcessorMode
--          selectInputList (show newMode)
--          printLine ("establishM1inputs new processor mode = " ++ show newMode)
          establishM1inputs
        Nothing -> do
          printError "esablishM1inputs, getUserState returned Nothing"
          return ()
-}
-- Each operation that requires DMA is carried out by a function that
-- supplies the required inputs, but does not use any of the input
-- lists.

-- in Driver but should be added to export list (edit Driver)
-- Don't set running to false on end of input
getStoredInput :: StateT (SysState a) IO (Maybe String)
getStoredInput = do
  s <- get
  case storedInput s of
    [] -> do
--      put $ s {running = False}  fix this
      return Nothing
    (x:xs) -> do
      put $ s {storedInput = xs}
      liftIO $ putStrLn ("getStoredInput " ++ x)
      return (Just x)

setMode :: ProcessorMode -> StateT (SysState DriverState) IO ()
setMode m = do
--  printLine ("Setting mode to " ++ show m)
  mds <- getUserState
  case mds of
    Just ds -> do
      let ds' = ds { processorMode  = m }
      s <- get
      put (s {userState = Just ds'})
      return ()
    Nothing -> do
      printError "setMode, getUserState returned Nothing"
      return ()

        
--  s <- get
--  let mds = userState s
--  case mds of
--    Just ds -> do
--      let ds' = ds { processorMode  = m }
--      put (s {userState = Just ds'})
--      return ()
--    Nothing -> do
--      giveError "userState not defined"
--      return ()

-- giveError :: String -> StateT (SysState a) IO ()
-- giveError msg = printLine ("ERROR: " ++ msg)

--------------------------------------------------------------------------------
-- M1 clock cycle
--------------------------------------------------------------------------------

{-

Every clock cycle consists of a sequence of phases:

- *choose* -- place inputs for current cycle into (currentInputString
  field of SysState).  Decide what inputs to use during the upcoming
  cycle, before it starts, and save the inputs as a string in a
  canonical format; it is placed in the currentInputString field of
  SysState.  The choice may depend on the driver state, annd/or it may
  depend on some input provided interactively by the user.  Once the
  inputs are chosen, all the signal values are fixed for the cycle
  (although what is actually output can be chosen later)

  - establish inputs - th
  - read the signals during the cycle
  - advance

The only thing that affects what happens during the cycle is the
inputs; controlling the cycle is done by establish inputs.  The middle
section just determines what is output, but not any signal values.
The advance has no choices to make; this is purely mechanical.

-}

-- selectModeInputs :: StateT (SysState DriverState) IO ()
-- selectModeInputs = do
--   s <- get
--   let ds = userState s
--   return ()  
-- Similar to Driver.clockCycle but specialized for M1 driver

        

cmdBoot :: Command DriverState
cmdBoot _ = doBoot

doBoot :: Operation a
doBoot = do
  printLine "Booting..."
--  selectInputList "boot"

cmdReset :: Command DriverState
cmdReset _ = doReset

doReset :: Operation a
doReset = do
  printLine "Resetting"
--  selectInputList  "reset"


-- At start of cycle, before establishing inputs, check to see if the
-- current input data list has been exhausted, and if so, enter the
-- appropriate mode

{-      

m1CheckInputsExhausted :: Command a
m1Step args = do
  s <- get
  ds <- driverState s
  printLine ("m1Step mode=" ++ show (processorMode ds))
  case processorMode ds of
    Idle -> do
      return ()
    Booting -> do
      return ()
    Running -> do
      return ()
    Output -> do
      return ()
    DumpRegs -> do
      return ()
    DumpMem -> do
      return ()
    Halted -> do
      return ()

  --selectInputList ["", "step"]
-}

--------------------------------------------------------------------------------
-- Clock cycle for M1
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Input signals for key stages of execution
--------------------------------------------------------------------------------

{-
I/O control inputs, copied from System
dma           1 bit    indicates stolen clock cycle
dma_store     1 bit    mem[dma_a] := dma_d
dma_fetch     1 bit    m_out = mem[dma_a]
dma_reg       1 bit    x = reg[dma_a]  (least significant 4 bits
dma_a         16 bits  address
dma_d         16 bits  data
-}


resetData :: [String]
resetData =  ["1 0 0 0 0 0 0"]

runData :: [String]
runData =  ["0 0 0 0 0 0 0"]

  
data DriverState = DriverState
  {
    displacement :: (Int,[Int])      -- (cycle, displacement)
  , effAddr :: [Int]                 -- effective address
  , rfloads :: [(Int,[Int],[Int])]   -- [(cycle,reg,value)]
  , memStores :: [(Int,[Int],[Int])] -- [(cycle,addr,value)]
  , jumps :: [(Int,Int,[Int])]       -- [(cycle,jumped,pcvalue)]
  , trap :: Bool
  , processorMode :: ProcessorMode
  }                     -- has a trap just been executed?
  deriving Show

initDriverState :: DriverState
initDriverState =
  DriverState
    {
      displacement = (0, take 16 (repeat 0))
    , effAddr = take 16 (repeat 0)
    , rfloads = []
    , memStores = []
    , jumps = []
    , trap = False
    , processorMode = Idle
    }

-- Record and display the effective address

setEffAddr :: (Signal a, Static a) =>
   DriverState -> [[a]] -> DriverState
setEffAddr s [x] =
  s {effAddr = map sigInt x}

showEffAddr :: DriverState -> String
showEffAddr s = ints16hex4 (effAddr s)

-- Record and display loads to the register file

setRfLoad :: (Signal a, Static a) =>
   DriverState -> [[a]] -> DriverState
setRfLoad s [r,x] =
  s {rfloads = (0, map sigInt r, map sigInt x) : rfloads s} -- ????????? c

clearRfLoads :: (Signal a, Static a) =>
  DriverState -> [[a]] -> DriverState
clearRfLoads s _ = s {rfloads = []}

showRfLoads :: DriverState -> String
showRfLoads s = concat (map f (reverse (rfloads s)))
  where f (c,r,x) =
          "R" ++ show (intsInt r) ++ " := " ++  ints16hex4 x
            ++ " was loaded\n"
--            ++ " was loaded in cycle " ++ show c ++ "\n"

-- Record and display stores to the memory

setMemStore :: (Signal a, Static a) =>
   DriverState -> [[a]] -> DriverState
setMemStore s [a,x] =
  s {memStores = (0, map sigInt a, map sigInt x) : memStores s} -- ???????? c

clearMemStores :: (Signal a, Static a) =>
  DriverState -> [[a]] -> DriverState
clearMemStores s _ = s {memStores = []}

showMemStores :: DriverState -> String
showMemStores s = concat (map f (reverse (memStores s)))
  where f (c,a,x) =
          "mem[" ++ ints16hex4 a ++ "] := " ++  ints16hex4 x
            ++ " was stored in cycle " ++ show c ++ "\n"

-- Record and display jumps

setJump :: (Signal a, Static a) =>
   DriverState -> [[a]] -> DriverState
setJump s [[b],x] =
  s {jumps = (0, sigInt b, map sigInt x) : jumps s}   -- ????????? c

clearJumps :: (Signal a, Static a) =>
  DriverState -> [[a]] -> DriverState
clearJumps s _ = s {jumps = []}

showJumps :: DriverState -> String
showJumps s = concat (map f (reverse (jumps s)))
  where
    f (c,b,x) =
      (if b==1
        then "jumped to " ++ ints16hex4 x
        else "did not jump")
        ++ " in cycle " ++ show c ++ "\n"

-- When the driver discovers that a trap has executed, it uses setTrap
-- to record this in the driver state.  The termination predicate uses
-- this value to decide when to stop the simulation.

setTrap :: DriverState -> [a] -> DriverState
setTrap s _ = s {trap = True}

setDisplacement :: (Signal a, Static a) =>
   DriverState -> [[a]] -> DriverState
setDisplacement s [w] =
  s {displacement = (0, map sigInt w)}

showDisplacement :: DriverState -> String
showDisplacement s =
  let (c,d) = displacement s
--  in "displacement " ++ ints16hex4 d
--       ++ " loaded in cycle " ++ show c ++ "\n"
  in ints16hex4 d

-- The termination predicate, termpred, stops the simulation when a
-- trap instruction is executed, or after 1000 cycles.

mytermpred :: DriverState -> Bool
mytermpred s = trap s  -- || c > 1000

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

-- Deprecated...

{- This module defines simulation drivers that run the M1 circuit for
the Sigma16 instruction set architecture. A separate main program
should be defined that imports this module, and that defines the
machine language program.  It uses run_M1_program, defined below, to
execute the program. -}

-- import System.Environment
-- import System.FilePath
-- import Control.Monad.State
-- {-# LANGUAGE NamedFieldPuns #-}

--  code <- readObject fname
--  code <- liftIO $ readObject objpath

{-
runDriver :: String -> IO ()
runDriver xs = do
  putStrLn ("running simulationDriver " ++ xs)
  execStateT simulationDriver initState
  return ()
-}
-- runM1 :: String -> IO ()
-- runM1 fname = do
--      runM1 objFile



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


----------------------------------------------------------------------
-- Simulation model
----------------------------------------------------------------------

{- The simulations are carried out using the Stream Bool model.  The
types Bit and Word are defined for clarity; a Bit is represented as a
Stream of Bools for the simulations. -}

{- The run_M1_program function takes a program (in the form of a list
of strings), loads it into the machine, and executes it, using the
sim_m1 simulation driver. -}

{-
-- prog is a list of hex strings giving object code
run_Sigma16_program :: [String] -> Int -> IO ()
run_Sigma16_program prog n =
  let inps = zipWith f [0..] prog ++ [[1,0,0,0]] ++ repeat [0,0,0,0]
      f i x = [0, 1, i, hexbin 16 x]
      m = n + length prog
  in  sim_m1 (take m inps)
-}
--      liftIO $ run_Sigma16_executable code limit
{-
-- prog is a list of ints giving object code
run_Sigma16_executable :: [Int] -> Int -> IO ()
run_Sigma16_executable prog n = do
  let f i x = [0, 1, i, x]
  let inps = zipWith f [0..] prog ++ [[1,0,0,0]] ++ repeat [0,0,0,0]
  let m = n + length prog
  putStrLn ("run executable limit = " ++ show m)
  putStrLn ("input = " ++ show (take m inps))
  sim_m1 (take m inps)
-}
-- main :: IO ()
-- main = do


--  operand <- liftIO getCmdOperand
--  printLine ("operation = " ++ show operand)
--  case operand of
--    Nothing -> return ()
--    Just objFile -> do
--      code <- liftIO $ getObject objFile
  

--  code <- liftIO $ readObject objpath
--  code <- readObjectFile
--   s <- get
--   let xs = args s
--   let fname = splitPath (head xs ++ ".obj.txt")
--   liftIO $ putStrLn ("fname = " ++ show fname)
--   let corePath = ["..", "..", "examples", "Core"]
--   let objParts = corePath ++ fname
--   liftIO $ putStrLn ("objParts = " ++ show objParts)
--   let objpath = joinPath objParts
--  liftIO $ putStrLn ("fname = " ++ show fname)
--   liftIO $ putStrLn ("objpath = " ++ objpath)


--      reset = getbit    input 0  -- :: Bit
--      dma   = getbit    input 1  -- :: Bit
--      dma_a = getbin 16 input 2  -- :: Word
--      dma_d = getbin 16 input 3  -- :: Word
-- sim_m1 :: [[Int]] -> StateT (SysState a) IO ()
-- sim_m1 input = do

--------------------------------------------------------------------------------
-- Simulation driver
--------------------------------------------------------------------------------

-- sim_m1 :: StateT (SysState a) IO ()
-- sim_m1 = do
  
--   let instrlimit = 200
--  let m = instrlimit + length bootData
--  printLine ("run executable limit = " ++ show m)
--  printLine ("input = " ++ show (take m inps))
--  liftIO $ sim_m1 (take m inps)
--      string "hello"
--    , string " condcc=", bit condcc
--    , string " st_instr_fet = ", bit (st_instr_fet ctlstate)
--    , string "\n"
--     , string "  x = ", bits (x dp)
--     , string "  cc = ", bits (cc dp)
--     , string "  ir_d = ", bits (ir_d dp)
--     , string "  r = ", bits (r)
--     , string "  ccnew = ", bits (ccnew)
--     , string "  condcc = ", bit (condcc)
--     string "  m_real_addr = ", bits m_real_addr,
--     string "  m_data = ", bits m_data,
--     string "  m_data = ", bits m_data,
--     string "  m_out =", bits m_out,
--  let  (ctlstate, ctl_start, ctlsigs, dp,
--  let  (CtlState {..}, ctl_start, ctlsigs, dp,
--  out_st_instr_fet <- outPortBit "st_instr_fet" (st_instr_fet ctlstate)
--  out_m_real_addr <- outPortWord "m_real_addr" m_real_addr showBin
--  out_m_out       <- outPortWord "m_out" m_out showBin
--  let ft = flagTable s
--  let key = breakpointKey s
--  let check = checkFlag (flagTable s) (breakpointKey s)
--  printLine ("********** Looper key = " ++ key ++ " check=" ++ show check)
--  let mds = userState s
--  case mds of
--    Nothing -> printLine "DriverState not defined"
--    Just ds -> do
--      liftIO $ putStr (".  " ++ show (processorMode ds))
