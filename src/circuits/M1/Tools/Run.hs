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

--------------------------------------------------------------------------------
-- See Installation section of the Sigma16 User Guide, and
-- Sigma16/src/circuits/README.txt

-- Usage
--   cd to src/circuits  -- must be in this directory
--   ghci                -- start ghci and initialize using .ghci
--   :main Simple/Add    -- run M1 circuit on examples/Core/Simple/Add.obj.txt
--   ^C                  -- stop and return to ghci prompt
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
  execStateT driver initState
  return ()

--------------------------------------------------------------------------------
-- Circuit definition
--------------------------------------------------------------------------------

-- The circut definition is written in pure functional style, just the
-- same as it always has been.  This example is named myreg1 to
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
-- Simulation driver
--------------------------------------------------------------------------------

driver :: StateT SysState IO ()
driver = do
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
  commandLoop
  liftIO $ putStrLn "Simulation terminated"

--------------------------------------------------------------------------------
-- State and data structures
--------------------------------------------------------------------------------

type CB = Stream Bool
      
data SysState = SysState
  { running :: Bool
  , cycleCount :: Int
  , inPortList :: [InPort]
  , outPortList :: [OutPort]
  }

initState :: SysState
initState = SysState
  { running = True
  , cycleCount = 0
  , inPortList = []
  , outPortList = []
  }


data InPort
  = InPortBit
      { inPortName :: String
      , inbsig  :: Stream Bool
      , inbuf :: IORef Bool
      , fetcher :: [IO Bool]
      }
  | InPortWord
      { inPortName :: String
      , inwsig  :: [CB]
      }

data OutPort      
  = OutPortBit
      { outPortName :: String
      , outbsig  :: CB
      }
  | OutPortWord
      { outPortName :: String
      , outwsig  :: [CB]
      }
  
inPortBit :: String -> StateT SysState IO InPort
inPortBit inPortName = do
  inbuf <- liftIO $ newIORef False
  let fetcher = repeat (readIORef inbuf)
  let inbsig = listeffects fetcher
  let port = InPortBit { inPortName, inbsig, inbuf, fetcher }
  s <- get
  put $ s {inPortList = inPortList s ++ [port]}
  return port

outPortBit :: String -> CB -> StateT SysState IO OutPort
outPortBit outPortName outbsig = do
  s <- get
  let port =  OutPortBit { outPortName, outbsig }
  s <- get
  put $ s {outPortList = outPortList s ++ [port]}
  return port

--------------------------------------------------------------------------------
-- Clock cycle
--------------------------------------------------------------------------------

clockCycle :: StateT SysState IO ()
clockCycle = do
  s <- get
  let i = cycleCount s
  let inp = inPortList s
  let outp = outPortList s
  liftIO $ putStrLn (take 70 (repeat '-'))
  liftIO $ putStrLn ("Cycle " ++ show i)
  readAllInputs
  printInPorts
  printOutPorts
  advanceInPorts
  advanceOutPorts
  s <- get
  put (s {cycleCount = i+1})

readAllInputs :: StateT SysState IO ()
readAllInputs = do
  s <- get
  let ports = inPortList s
  mapM_ readInput ports
  return ()
  
readInput :: InPort -> StateT SysState IO ()
readInput port = do
  readval <- liftIO $ readSigBool ("  enter " ++ inPortName port)
  liftIO $ writeIORef (inbuf port) readval

showBSig :: Bool -> String
showBSig False  = "0"
showBSig True = "1"

printInPorts :: StateT SysState IO ()
printInPorts = do
  s <- get
  let ports = inPortList s
  let i = cycleCount s
  liftIO $ putStrLn ("Input signals during cycle " ++ show i)
  mapM_ printInPort ports

printInPort :: InPort -> StateT SysState IO ()
printInPort port = do
  let x = current (inbsig port)
  let xs = "    " ++ inPortName port ++ " = " ++ showBSig x
  liftIO $ putStrLn xs

printOutPorts :: StateT SysState IO ()
printOutPorts = do
  s <- get
  let ports = outPortList s
  let i = cycleCount s
  liftIO $ putStrLn ("Output signals during cycle " ++ show i)
  mapM_ printOutPort ports

printOutPort :: OutPort -> StateT SysState IO ()
printOutPort port = do
  let x = current (outbsig port)
  let xs = "    " ++ outPortName port ++ " = " ++ showBSig x
  liftIO $ putStrLn xs

advanceInPorts :: StateT SysState IO ()
advanceInPorts = do
  s <- get
  let ports = inPortList s
  ports' <- mapM advanceInPort ports
  put $ s {inPortList = ports'}

advanceInPort :: InPort -> StateT SysState IO InPort
advanceInPort port =
  return $ port {inbsig = future (inbsig port)}

advanceOutPorts :: StateT SysState IO ()
advanceOutPorts = do
  s <- get
  let ports = outPortList s
  ports' <- mapM advanceOutPort ports
  put $ s {outPortList = ports'}

advanceOutPort :: OutPort -> StateT SysState IO OutPort
advanceOutPort port =
  return $ port {outbsig = future (outbsig port)}

--------------------------------------------------------------------------------
-- Command interpreter
--------------------------------------------------------------------------------

data Cmd
  = Quit        -- q
  | Help        -- h pr ?
  | ListInput   -- l
  | ReadInput   -- r
  | Boot        -- b
  | StepCycle   -- c  or space
  | Go          -- g
  | NoCmd       -- other
  deriving (Eq, Read, Show)

parseCmd :: String -> Cmd
parseCmd ('q' : _) = Quit
parseCmd ('h' : _) = Help
parseCmd ('?' : _) = Help
parseCmd ('l' : _) = StepCycle
parseCmd ('r' : _) = ReadInput
parseCmd ('b' : _) = Boot
parseCmd ('c' : _) = StepCycle
parseCmd (' ' : _) = StepCycle
parseCmd ('g' : _) = ReadInput
parseCmd _ = NoCmd

commandLoop :: StateT SysState IO ()
commandLoop = do
  s <- get
  case running s of
    False -> return ()
    True -> do
      doCommand
      commandLoop

doCommand :: StateT SysState IO ()
doCommand = do
  liftIO $ putStr "$ "
  xs <- liftIO $ hGetLine stdin
  let c = parseCmd xs
  case c of
    Quit -> do
      liftIO $ putStrLn "Quitting"
      s <- get
      put $ s {running = False}
      return ()
    NoCmd -> return ()
    StepCycle -> do
      clockCycle
      return ()
    _ -> return ()
    


--  args <- getArgs
--  putStrLn ("args = " ++ show args)

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

--------------------------------------------------------------------------------
-- Formatting simulation output
--------------------------------------------------------------------------------

data Fmt a
  = FmtString String
  | FmtBit a
  | FmtBits [a]

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

--  testTextColors
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
      f
      doNtimes (i-1) f
