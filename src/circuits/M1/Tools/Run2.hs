-- Sigma16: Run.hs simulates a processor circuit to run a machine language program
-- Copyright (C) 2021 John T. O'Donnell
-- email: john.t.odonnell9@gmail.com
-- License: GNU GPL Version 3 or later. See Sigma16/README.md, LICENSE.txt

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

-- See Installation section of the Sigma16 User Guide, and
-- Sigma16/src/circuits/README.txt

-- Usage
--   cd to src/circuits  -- must be in this directory
--   ghci                -- start ghci and initialize using .ghci
--   :main Simple/Add    -- run M1 circuit on examples/Core/Simple/Add.obj.txt
--   :r                  -- reload, after you have changed a circuit source file
--   :q                  -- quit ghci, return to shell

--------------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad.State
import Control.Concurrent
import Data.IORef
import System.Environment
import System.FilePath
import System.IO
import System.IO.Unsafe
import System.Console.ANSI

import Sigma16.ReadObj
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational
import HDL.Hydra.Circuits.Register
import M1.Tools.M1driver

--------------------------------------------------------------------------------
-- Main program
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Main starting"
  args <- getArgs
  putStrLn ("args = " ++ show args)
  testTextColors
  execStateT commandLoop (mkInitState args)
  return ()

commandLoop :: StateT SysState IO ()
commandLoop = do
  liftIO $ putStrLn "Command loop starting"
  initializeCircuit

  doNtimes 10 cliReadClockCycle
  liftIO $ putStrLn "Command loop finishing"

--------------------------------------------------------------------------------
-- Circuit definition
--------------------------------------------------------------------------------

-- The circut definition is written in pure functional style, just the
-- same as it always has been.  This definition is named myreg1 to
-- avoid confusion with the reg1 circuit defined in the Hydra
-- libraries.  Normally reg1 would only output r, as q is just an
-- internal signal.  However, it is enlightening for a student to
-- experiment interactively with the circuit, and to do that it's
-- helpful to see the value of q.

myreg1 :: CBit a => a -> a -> (a,a)
myreg1 ld x = (r, q)
  where r = dff q
        q = or2 (and2 (inv ld) r) (and2 ld x)

--------------------------------------------------------------------------------
-- Define interface to the circuit
--------------------------------------------------------------------------------

-- An interface is defined to connect the circuit definition with the
-- simulation driver.

-- The top level circuit interface uses a record with named fields.
-- We could use a tuple instead, but the named fields work better for
-- larger circuits, so we will always use the approach for top level
-- circuits.  The signal names in these records begin with in_ or out_
-- which helps to avoid top level name clashes.  There should be only
-- one input to the circuit named x, but there might be other top
-- level entities named x.  So we use the input name "in_x" to avoid
-- any possibility of confusion.

-- The user needs to define CircuitInputs, CircuitOutputs,
-- initializeCircuit

data CircuitInputs = CircuitInputs
  { in_ld :: Inport   -- load control
  , in_x  :: Inport   -- data input
  , inputlist :: [Inport]
  }

data CircuitOutputs = CircuitOutputs
  { out_r ::  Outport   -- current state of the register
  , out_q ::  Outport   -- internal signal: input to the dff
  , outputlist :: [Outport]
  }

advanceInputs :: CircuitInputs -> CircuitInputs
advanceInputs inp = inp
  { in_ld = mkInBitContinue inp in_ld
  , in_x =  mkInBitContinue inp in_x
  }

advanceOutputs :: CircuitOutputs -> CircuitOutputs
advanceOutputs outp = outp
  { out_r = mkOutBitContinue outp out_r
  , out_q =  mkOutBitContinue outp out_q
  }
  
initializeCircuit :: StateT SysState IO ()
initializeCircuit = do
-- Inputs
  in_ld <- liftIO $ mkInportBit "ld"
  in_x <- liftIO $ mkInportBit "x"
  let inputs = CircuitInputs
        {in_ld, in_x,                -- access by name
         inputlist = [in_ld, in_x]}     -- for iteration

-- Circuit  
  let (r, q) = myreg1 (inSig in_ld) (inSig in_x)

-- Outputs  
  let out_r = mkOutportBit r "r"
  let out_q = mkOutportBit q "q"
  let outputs = CircuitOutputs
        {out_r, out_q,              -- access by name
         outputlist=[out_r, out_q]}  -- for iteration
-- Circuit state
  s <- get
  put $ s { circstate = Just $ CircuitState {cycleCount = 0, inputs, outputs}}

--------------------------------------------------------------------------------
-- Circuit state
--------------------------------------------------------------------------------

-- The simulation driver uses a fixed "clocked bit" type CB, while
-- circuits are normally defined using a more general type variable a
-- for a signal, and constrain a to be in the CBit class.

type CB = Stream Bool

data CircuitState = CircuitState
  { cycleCount :: Int
  , inputs :: CircuitInputs
  , outputs :: CircuitOutputs
  }

-- The user defines a specific CircuitInputs record an CircuitOutputs
-- record that specifies the ports for the specific circuit being
-- simulated.  The definition will contain something like:

-- circstate  = Just cst
--    cst = CircuitState {..., inputs, outputs
--    inputs = CircuitInputs {xyz, abc, [xyz,abc]}
--    outputs = CircuitInputs {xyz, abc, [xyz,abc]}

-- An Inport is a record containing several pieces of information,
-- including the signal value, but an Outport is just a signal value.

data Outport
  = OutportBit
      { outPortName :: String
      , outSig :: Stream Bool
      }

mkOutportBit :: CB -> String -> Outport
mkOutportBit outSig outPortName = OutportBit {outPortName, outSig}

data Inport
  = InportBit
      { inPortName :: String
      , inbuf :: IORef Bool
      , fetcher :: [IO Bool]
      , inSig :: Stream Bool
      }
  | InportWord
      { inPortName :: String
      , wordsize :: Int
      , inbufs :: [IORef Bool]
      , fetchers :: [[IO Bool]]
      , inSigs :: [Stream Bool]
      }

mkInportBit :: String -> IO (Inport)
mkInportBit inPortName = do
  inbuf <- newIORef False
  let fetcher = repeat (readIORef inbuf)
  let inSig = listeffects fetcher
  return $ InportBit { inPortName, inbuf, fetcher, inSig }

{-  need to build words...
mkInportWord :: String -> Int -> IO (Inport)
mkInportBit portname wordsize = do
  inbufs <- newIORef False
  let fetcher = repeat (readIORef inbuf)
  let xs = listeffects fetcher
  return $ InportBit { portname, wordsize, inbuf, fetcher, sigval }
-}

  
data SysState = SysState
  { args :: [String]
  , circstate :: Maybe CircuitState
  }

mkInitState :: [String] -> SysState
mkInitState args =
  SysState
    { args
    , circstate = Nothing
    }


--------------------------------------------------------------------------------
-- Command interpreter using IORef to allow establishing input dynamically
--------------------------------------------------------------------------------
  
cliReadClockCycle :: StateT SysState IO ()
cliReadClockCycle = do
  s <- get
  case circstate s of
    Nothing -> do
      liftIO $ putStrLn "Circuit state not initialized"
    Just circst -> do
      let i = cycleCount circst
      let inp = inputs circst
      let outp = outputs circst
      liftIO $ putStrLn (take 70 (repeat '-'))
      liftIO $ putStrLn ("Cycle " ++ show i)

-- Establish values of input ports for current clock cycle
      readAllInputs inp

-- Print signal values during the current cycle
      let ldval = current (inSig (in_ld inp))
      let xval = current (inSig (in_x inp) )
      let rval = current (outSig (out_r outp))
      let qval = current (outSig (out_q outp))
      liftIO $ putStrLn ("Cycle " ++ show i)
      liftIO $ putStrLn ("   Inputs   ld = " ++ show ldval
                         ++ "   x = " ++ show xval)
      liftIO $ putStrLn ("   Outputs  r = " ++ show rval ++ "    q = " ++ show qval)

      printInports i inp [in_ld inp, in_x inp]
--      printInports i inp [f inp | f <- inputlist inp]
      printInports i inp (inputlist inp)
      printOutports i outp [out_r outp, out_q outp]
      liftIO $ putStrLn "individual printInport"
      printInport inp (in_ld inp)
      printInport inp (in_x inp)
      printOutport outp (out_r outp)
      printOutport outp (out_q outp)
      
      let ldval = current (inSig (in_ld inp))
      let xval = current (inSig (in_x inp) )
      let rval = current (outSig (out_r outp))
      let qval = current (outSig (out_q outp))
      liftIO $ putStrLn ("Cycle " ++ show i)
      liftIO $ putStrLn ("   Inputs   ld = " ++ show ldval
                         ++ "   x = " ++ show xval)
      liftIO $ putStrLn ("   Outputs  r = " ++ show rval ++ "    q = " ++ show qval)
      let outpNext = outp {out_r = mkOutBitContinue outp out_r,
                           out_q = mkOutBitContinue outp out_r}

-- Clock tick
      let newst =
            circst { inputs = advanceInputs inp
                   , outputs = advanceOutputs outp
                   , cycleCount = i+1
                   }
      put (s {circstate = Just newst})
    
--      put $ s {circstate = Just $ circst
--        { inputs = advanceInputs inp
--        , outputs = advanceOutputs outp
--        , outputs = outpNext
--        , cycleCount = i+1 } }

showBSig :: Bool -> String
showBSig False  = "0"
showBSig True = "1"

printInports :: Int -> CircuitInputs -> [Inport] -> StateT SysState IO ()
printInports i inp ports = do
--  let fs = inputlist inp
  liftIO $ putStrLn ("Input signals during cycle " ++ show i)
  mapM_ (printInport inp) ports

printInport :: CircuitInputs -> Inport -> StateT SysState IO ()
printInport inp p = do
  let x = current (inSig p)
  let xs = "    " ++ inPortName p ++ " = " ++ showBSig x
  liftIO $ putStrLn xs

printOutports :: Int -> CircuitOutputs -> [Outport] -> StateT SysState IO ()
printOutports i outp ports = do
--  let fs = outputlist outp
--  let ports = [f outp | f <- fs]
--  liftIO $ putStrLn ("printOutports " ++ concat (map outPortName fs))
  liftIO $ putStrLn ("printOutports " ++ concat (map outPortName ports))
  liftIO $ putStrLn ("Output signals during cycle " ++ show i)
  mapM_ (printOutport outp) ports

printOutport :: CircuitOutputs -> Outport -> StateT SysState IO ()
printOutport outp p = do
  let x = current (outSig p)
  let xs = "    " ++ outPortName p ++ " = " ++ showBSig x
  liftIO $ putStrLn xs

readAllInputs :: CircuitInputs -> StateT SysState IO ()
readAllInputs inputs = do
  mapM_ (readInput inputs) (inputlist inputs)
  return ()
  
readInput :: CircuitInputs -> Inport -> StateT SysState IO ()
readInput inputs inport = do
  readval <- liftIO $ readSigBool ("  enter " ++ inPortName inport)
  liftIO $ writeIORef (inbuf inport) readval

mkInBitContinue :: CircuitInputs -> (CircuitInputs -> Inport) -> Inport
mkInBitContinue inp f = (f inp) {inSig = future (inSig (f inp))}

mkOutBitContinue :: CircuitOutputs -> (CircuitOutputs -> Outport) -> Outport
mkOutBitContinue outp f = (f outp) {outSig = future (outSig (f outp))}

--------------------------------------------------------------------------------
-- Boot
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Formatting simulation output
--------------------------------------------------------------------------------

data Fmt a
  = FmtString String
  | FmtBit a
  | FmtBits [a]

--------------------------------------------------------------------------------
-- Commands
--------------------------------------------------------------------------------

data Cmd
  = Quit
  | NoCmd
  | StepCycle
  | StepInstr
  | UnkownCmd
  deriving (Eq, Show)

parseCmd :: String -> Cmd
parseCmd ('q' : _) = Quit
parseCmd ('c' : _) = StepCycle
parseCmd ('i' : _) = StepInstr
parseCmd _ = UnkownCmd

getCmd :: IO Cmd
getCmd = do
  x <- readIfReady
  case x of
    Nothing -> return NoCmd
    Just xs -> do
      putStrLn ("got xs = " ++ xs)
      return (parseCmd xs)

--------------------------------------------------------------------------------
-- Terminal I/O
--------------------------------------------------------------------------------

-- Quick version of a function to read a signal value.  Requires first
-- char to be either '1' or '0'.  Later, should parse the input
-- properly

readSigBool :: String -> IO Bool
readSigBool xs = do
--  putStrLn ("Enter value of " ++ xs ++ " (either 0 or 1, then enter)")
  putStrLn xs
  ys <- getLine
  return (if head ys == '1' then True else False)

-- Check whether there is input available.  If so do a blocking line
-- read, and when the entire line is available read and return it.  If
-- there is no input, return immediately

readIfReady :: IO (Maybe String)
readIfReady = do
  b <- hReady stdin
  case b of
    False -> return Nothing
    True -> do
      xs <- hGetLine stdin
      return (Just xs)

testTextColors :: IO ()
testTextColors = do
  setSGR [SetColor Foreground Vivid Red]
--  setSGR [SetColor Background Vivid Blue]
  putStrLn "This text is red"
  setSGR [Reset]  -- Reset to default colour scheme
  putStrLn "This text appears in the default colors"
  setSGR [SetColor Foreground Vivid Blue]
  putStrLn "This text is blue"
  setSGR [Reset]  -- Reset to default colour scheme
  putStrLn "Back to the default colors"

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- Take a list of actions, perform them on demand, and return the result

listeffects :: [IO a] -> Stream a
listeffects = mapListStream unsafePerformIO

mapListStream :: (a->b) -> [a] -> Stream b
mapListStream f x
  = Cycle (f (head x))
          (mapListStream f (tail x))

doNtimes :: Int -> StateT SysState IO () -> StateT SysState IO ()
doNtimes i f
  | i == 0   = return ()
  | otherwise = do
--      liftIO $ putStrLn ("repeating " ++ show i ++ " more times")
      f
      doNtimes (i-1) f

-- deprecated

-- replaced by readAllInputs...      
--      establishInput (in_ld inp)
--      establishINput (in_x inp)
--      let ldPort = in_ld inp
--      ld_readval <- liftIO $ readSigBool "  enter ld"
--      liftIO $ writeIORef (inbuf (in_ld inp)) ld_readval
--      let xPort = in_x inp
--      x_readval <- liftIO $ readSigBool "  enter x"
--      liftIO $ writeIORef (inbuf (in_x inp) ) x_readval
--      let s' = s {circstate = Just newcircst}
--      put s'
--      let rContinue = future (out_r outp)
--      let qContinue = future (out_q outp)
--      let outpNext = outp {out_r = rContinue, out_q = qContinue}
-- inContinue :: CircuitInputs -> (CircuitInputs -> Inport) -> CB
-- inContinue inp f = future (sigval (f inp))

--      let ldContinue = inContinue inp in_ld
--      let xContinue = inContinue inp in_x
--      let inpNext = inp {in_ld = (in_ld inp) {sigval = ldContinue},
--                         in_x = (in_x inp)  {sigval = xContinue}}
--      let ldContinue = future (sigval (in_ld inp))
--      let xContinue = future (sigval (in_x inp) )
--      let rs = out_r outp
--      let rval = current rs
--      let qs = out_q outp
---      let qval = current qs
--      let rContinue = future rs
--      let qContinue = future qs

{-
calculate ::
  CircuitInputs -> (CircuitInputs -> Inport) -> StateT SysState IO ()
calculate inputs f = do
  let inport = f inputs
  let val = current (inSigVal inport)
  let continue = future (inSigVal inport)
  return ()

oldmain = do
  putStrLn "M1 Run starting"
  let limit = 200
  args <- getArgs
  putStrLn ("args = " ++ show args)

  let fname = head args
  putStrLn ("fname = " ++ fname)
  code <- readObject fname
  putStrLn ("Object code = " ++ show code)
  run_Sigma16_executable code limit
  putStrLn "M1 Run finished"

--      let inpNext = inp {in_ld = mkInBitContinue inp in_ld,
--                         in_x =  mkInBitContinue inp in_x}
--      let inpNext = updateInputs inp

--      let outpNext = outp {out_r = mkOutBitContinue outp out_r,
--                           out_q = mkOutBitContinue outp out_r}
--      let outpNext = advanceOutputs outp


--      let newcircst = circst {inputs = advanceInputs inp,
--                              outputs = advanceOutputs outp
--                              cycleCount = i+1 }
--      put $ s {circstate = Just newcircst}

--      let ldval = current (inSig (in_ld inp))
--      let xval = current (inSig (in_x inp) )
--      let rval = current (outSig (out_r outp))
--      let qval = current (outSig (out_q outp))
--      liftIO $ putStrLn ("Cycle " ++ show i)
--      liftIO $ putStrLn ("   Inputs   ld = " ++ show ldval
--                         ++ "   x = " ++ show xval)
--    liftIO $ putStrLn ("   Outputs  r = " ++ show rval ++ "    q = " ++ show qval)

-- printInport :: CircuitInputs -> Inport -> StateT SysState IO ()
-- printInport inp p = do
--   let x = current (inSig p)
--   let xs = "    " ++ inPortName p ++ " = " ++ showBSig x
--   liftIO $ putStrLn xs


-}
