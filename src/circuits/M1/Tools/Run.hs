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

  putStrLn "\bRunning CLI reading input"
  let init = mkInitState args
  execStateT commandLoop init

  return ()

commandLoop :: StateT SysState IO ()
commandLoop = do
  liftIO $ putStrLn "commandLoop starting"
  initializeCircuit
  doNtimes 10 cliReadClockCycle
  liftIO $ putStrLn "CLI finishing"
  return ()

--------------------------------------------------------------------------------
-- Circuit
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

data CircuitInputs = CircuitInputs
  { in_ld :: Inport   -- load control
  , in_x  :: Inport   -- data input
  }

data CircuitOutputs = CircuitOutputs
  { out_r ::  CB   -- current state of the register
  , out_q ::  CB   -- internal signal: input to the dff
  }

initializeCircuit :: StateT SysState IO ()
initializeCircuit = do
  in_ld <- liftIO $ mkInportBit "ld"
  in_x <- liftIO $ mkInportBit "x"
  let inputs = CircuitInputs {in_ld, in_x}
  let (out_r, out_q) = myreg1 (sigval in_ld) (sigval in_x)
  let outputs = CircuitOutputs {out_r, out_q}
  let cst = CircuitState {cycleCount = 0, inputs, outputs}
  s <- get
  put $ s { circstate = Just cst }

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

-- An Inport is a record containing several pieces of information,
-- including the signal value, but an Outport is just a signal value.

data Inport
  = InportBit
      { portname :: String
      , inbuf :: IORef Bool
      , fetcher :: [IO Bool]
      , sigval :: Stream Bool
      }
  | InportWord
      { portname :: String
      , wordsize :: Int
      , inbufs :: [IORef Bool]
      , fetchers :: [[IO Bool]]
      , sigvals :: [Stream Bool]
      }

mkInportBit :: String -> IO (Inport)
mkInportBit portname = do
  inbuf <- newIORef False
  let fetcher = repeat (readIORef inbuf)
  let sigval = listeffects fetcher
  return $ InportBit { portname, inbuf, fetcher, sigval }

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
  liftIO $ putStrLn "cliReadClockCycle starting"
  s <- get
  case circstate s of
    Nothing -> do
      liftIO $ putStrLn "Circuit state not initialized"
    Just circst -> do
      let i = cycleCount circst
      liftIO $ putStrLn ("Cycle " ++ show i)
      let inp = inputs circst

-- Read values of input ports for current clock cycle  
      let ldPort = in_ld inp
      ld_readval <- liftIO $ readSigBool "  enter ld"
      liftIO $ writeIORef (inbuf ldPort) ld_readval
      let xPort = in_x inp
      x_readval <- liftIO $ readSigBool "  enter x"
      liftIO $ writeIORef (inbuf xPort) x_readval

-- Calculate signals
      let ldval = current (sigval ldPort)
      let ldContinue = future (sigval ldPort)
      let xval = current (sigval xPort)
      let xContinue = future (sigval xPort)

-- Calculate input continuation
      let inpNext = CircuitInputs {in_ld = ldPort {sigval = ldContinue},
                                   in_x = xPort {sigval = xContinue}}

-- Extract outputs  
      let outp = outputs circst

      let rs = out_r outp
      let rval = current rs
      let rContinue = future rs

      let qs = out_q outp
      let qval = current qs
      let qContinue = future qs

-- Calculate output continuation
      let outpNext = CircuitOutputs {out_r = rContinue, out_q = qContinue}

-- Print signals for this clock cycle
      liftIO $ putStrLn ("Cycle " ++ show i)
      liftIO $ putStrLn ("   Inputs   ld = " ++ show ldval
                         ++ "   x = " ++ show xval)
      liftIO $ putStrLn ("   Outputs  r = " ++ show rval ++ "    q = " ++ show qval)

-- Update state
      let newcircst = circst {inputs = inpNext,
                              outputs = outpNext, cycleCount = i+1 }
      let s' = s {circstate = Just newcircst}
      put s'
      liftIO $ putStrLn "cliReadClockCycle finished"


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

oldmain :: IO ()
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
  putStrLn ("Enter value of " ++ xs ++ " (either 0 or 1, then enter)")
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
      liftIO $ putStrLn ("repeating " ++ show i ++ " more times")
      f
      doNtimes (i-1) f
