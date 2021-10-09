-- Hydra: StepDriver.hs
-- Copyright (c) 2021 John T. O'Donnell
-- email: john.t.odonnell9@gmail.com
-- License: GNU GPL Version 3 or later. See Sigma16/README.md, LICENSE.txt

-- This file is part of Hydra.  Hydra is free software: you can
-- redistribute it and/or modify it under the terms of the GNU General
-- Public License as published by the Free Software Foundation, either
-- version 3 of the License, or (at your option) any later version.
-- Sigma16 is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.  You should have received
-- a copy of the GNU General Public License along with Sigma16.  If
-- not, see <https:--www.gnu.org/licenses/>.

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module HDL.Hydra.Core.StepDriver where

import Control.Monad.State
import Control.Concurrent
import Data.IORef
import System.Environment
import System.FilePath
import System.IO
import System.IO.Unsafe
import System.Console.ANSI

import HDL.Hydra.Core.PrimData
import HDL.Hydra.Core.Signal
import HDL.Hydra.Core.SigStream
import HDL.Hydra.Core.SigBool

--------------------------------------------------------------------------------
-- Utilities for  cli
--------------------------------------------------------------------------------

intToBits :: Int -> Int -> [Bool]
intToBits x n = reverse (intToBitsf x n)

intToBitsf :: Int -> Int -> [Bool]
intToBitsf x n
  | n == 0 = []
  | n > 0 = (x `mod` 2 /= 0) : intToBitsf (x `div` 2) (n-1)
  

data CmdOp
  = CmdOpEmpty
  | CmdOpBatch
  | CmdOpCLI
  deriving (Eq,Show)

getCmd :: IO (CmdOp, String)
getCmd = do
  putStrLn "getCmd"
  args <- getArgs
  putStrLn ("args = " ++ show args)
  let operand = if length args >= 2 then args!!1 else ""
  return $
    (if length args == 0 then CmdOpEmpty
        else if args!!0 == "batch" then CmdOpBatch
        else if args!!0 == "cli" then CmdOpCLI
        else CmdOpEmpty,
     operand)

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
      , inwsig  :: [Stream Bool]
      , inwbufs :: [IORef Bool]
      , wfetchers :: [[IO Bool]]
      , inwsize :: Int
      , readw :: (String -> Int -> IO [Bool])
      }

data OutPort      
  = OutPortBit
      { outPortName :: String
      , outbsig  :: CB
      }
  | OutPortWord
      { outPortName :: String
      , outwsig  :: [Stream Bool]
      , outwsize :: Int
      , showw :: [Bool] -> String
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

inPortWord :: String -> Int -> (String -> Int -> IO [Bool])
           -> StateT SysState IO InPort
inPortWord inPortName inwsize readw = do
  inwbufs <- liftIO $ mkInBufWord inwsize
  let wfetchers = map (\x -> repeat (readIORef x)) inwbufs
  let inwsig = map listeffects wfetchers
  liftIO $ putStrLn ("inPortWord len inwbufs = " ++ show (length inwbufs))
  liftIO $ putStrLn ("inPortWord len wfetchers = " ++ show (length wfetchers))
  liftIO $ putStrLn ("inPortWord len inwsig = " ++ show (length inwsig))
  let port = InPortWord { inPortName, inwsig, inwbufs, wfetchers, inwsize, readw }
  s <- get
  put $ s {inPortList = inPortList s ++ [port]}
  return port

mkInBufWord :: Int -> IO [IORef Bool]
mkInBufWord n
  | n == 0 = return []
  | n > 0  = do x <- newIORef False
                xs <- mkInBufWord (n-1)
                return (x:xs)

outPortBit :: String -> CB -> StateT SysState IO OutPort
outPortBit outPortName outbsig = do
  s <- get
  let port =  OutPortBit { outPortName, outbsig }
  s <- get
  put $ s {outPortList = outPortList s ++ [port]}
  return port

outPortWord :: String -> [CB] -> ([Bool]->String) -> StateT SysState IO OutPort
outPortWord outPortName outwsig showw = do
  s <- get
  let outwsize = length outwsig
  let port =  OutPortWord { outPortName, outwsig, outwsize, showw }
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

writeBufs :: [IORef Bool] -> [Bool] -> IO ()
writeBufs rs bs = mapM_ (\(r,b) -> writeIORef r b ) (zip rs bs)

readInput :: InPort -> StateT SysState IO ()
readInput (InPortBit {..}) = do
  readval <- liftIO $ readSigBool ("  enter " ++ inPortName)
  liftIO $ writeIORef inbuf readval
readInput (InPortWord {..}) = do
  xs <- liftIO $ readw inPortName inwsize
  liftIO $ putStrLn ("readInput xs = " ++ show xs)
  liftIO $ writeBufs inwbufs xs
  return ()
--  readval <- liftIO $ readSigString ("  enter " ++ inPortName)

-- Int -> Int -> [Int] ... intbin k x
-- [Int] -> Int...  binint tcint
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
printInPort (InPortBit {..}) = do
  let x = current inbsig
  let xs = "    " ++ inPortName ++ " = " ++ showBSig x
  liftIO $ putStrLn xs
printInPort (InPortWord {..}) = do
  let xs = map current inwsig
  let ys = "    " ++ inPortName ++ " = " ++ show xs
  liftIO $ putStrLn ys

printOutPorts :: StateT SysState IO ()
printOutPorts = do
  s <- get
  let ports = outPortList s
  let i = cycleCount s
  liftIO $ putStrLn ("Output signals during cycle " ++ show i)
  mapM_ printOutPort ports

printOutPort :: OutPort -> StateT SysState IO ()
printOutPort (OutPortBit {..}) = do
  let x = current outbsig
  let xs = "    " ++ outPortName ++ " = " ++ showBSig x
  liftIO $ putStrLn xs
printOutPort (OutPortWord {..}) = do
  let xs = showw (map current outwsig)
  let ys = "    " ++ outPortName ++ " = " ++ xs
  liftIO $ putStrLn ys

advanceInPorts :: StateT SysState IO ()
advanceInPorts = do
  s <- get
  let ports = inPortList s
  ports' <- mapM advanceInPort ports
  put $ s {inPortList = ports'}

advanceInPort :: InPort -> StateT SysState IO InPort
advanceInPort (InPortBit {..}) = do
  return $ InPortBit {inbsig = future inbsig, ..}
advanceInPort (InPortWord {..}) = do
  let xs  = map future inwsig
  return $ InPortWord {inwsig = xs, ..}

advanceOutPorts :: StateT SysState IO ()
advanceOutPorts = do
  s <- get
  let ports = outPortList s
  ports' <- mapM advanceOutPort ports
  put $ s {outPortList = ports'}

advanceOutPort :: OutPort -> StateT SysState IO OutPort
advanceOutPort (OutPortBit {..}) =
  return $ OutPortBit {outPortName, outbsig = future outbsig}
advanceOutPort (OutPortWord {..}) = do
  let xs = map future outwsig
  return $ OutPortWord {outPortName, outwsig=xs, outwsize, showw}

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
parseCmd "" = StepCycle
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

helpMessage :: String
helpMessage =
  "Type command character, then Enter\n"
  ++ "Enter, with other character, is same as c (step cycle)\n"
  ++ "q -- quit, return to ghci prompt\n"
  ++ "h -- help, display this message\n"
  ++ "c -- simulate one clock cycle"

doCommand :: StateT SysState IO ()
doCommand = do
  liftIO $ putStr "M1 circuit $ "
  xs <- liftIO $ hGetLine stdin
  let c = parseCmd xs
  case c of
    Quit -> do
      liftIO $ putStrLn "Quitting"
      s <- get
      put $ s {running = False}
      return ()
    NoCmd -> return ()
    Help -> do liftIO $ putStrLn helpMessage
    StepCycle -> do
      clockCycle
      return ()
    _ -> return ()
    



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

readSigString :: String -> IO String
readSigString xs = do
  putStrLn xs
  ys <- getLine
  putStrLn ("readSigString = <" ++ ys ++ ">")
  return ys

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
