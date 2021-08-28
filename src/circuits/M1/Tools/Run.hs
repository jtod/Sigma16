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

main :: IO ()
main = do
  putStrLn "Main starting"
  args <- getArgs
  putStrLn ("args = " ++ show args)

  setSGR [SetColor Foreground Vivid Red]
--  setSGR [SetColor Background Vivid Blue]
  putStrLn "This text is red"
  setSGR [Reset]  -- Reset to default colour scheme
  putStrLn "This text appears in the default colors"
  setSGR [SetColor Foreground Vivid Blue]
  putStrLn "This text is blue"
  setSGR [Reset]  -- Reset to default colour scheme
  putStrLn "Back to the default colors"

  init <- mkInitState args

{-
  putStrLn (repeat 70 '-')
  putStrLn "\bRunning CLI with input list"
  execStateT cliWithInputList init
-}

  putStrLn (take 70 (repeat '-'))
  putStrLn "\bRunning CLI reading input"
  execStateT cliReadInput init
  return ()

-- The main command line interface


--------------------------------------------------------------------------------
-- System state
--------------------------------------------------------------------------------

-- Using specific signal type CB = Stream Bool


data SysState = SysState
  { dffInit :: Bool
  , args :: [String]
  , cycleCount :: Int
  , running :: Bool
  , ldInpBuffer, xInpBuffer :: IORef Bool
  , ldFetchers, xFetchers :: [IO Bool]
  , lds, xs :: Stream Bool
  , inputs :: CircInputs
  , outputs :: CircOutputs
  }
--  , ldInpBuffer, xInpBuffer :: IORef Bool
--  , ldFetchers, xFetchers :: [IO Bool]

mkInitState :: [String] -> IO SysState
mkInitState args = do
  ldInpBuffer <- newIORef False
  xInpBuffer <- newIORef False
  let ldFetchers = repeat (readIORef ldInpBuffer)
  let xFetchers = repeat (readIORef xInpBuffer)
  let lds = listeffects ldFetchers
  let xs = listeffects  xFetchers
  let inputs = CircInputs { in_ld = lds, in_x = xs }
  let outputs = myCirc inputs
  let s =  SysState
             { dffInit = False
             , args
             , cycleCount = 0
             , running = True
             , ldInpBuffer
             , xInpBuffer
             , ldFetchers
             , xFetchers
             , lds
             , xs
             , inputs
             , outputs
             }
  return s

--  ldInpBuffer <- newIORef False
--  xInpBuffer <- newIORef False

-- Take a list of actions, perform them on demand, and return the result

listeffects :: [IO a] -> Stream a
listeffects = mapListStream unsafePerformIO

mapListStream :: (a->b) -> [a] -> Stream b
mapListStream f x
  = Cycle (f (head x))
          (mapListStream f (tail x))

{-
effects :: [IO a] -> Stream a
effects = map unsafePerformIO
-}

--------------------------------------------------------------------------------
-- Circuit
--------------------------------------------------------------------------------

-- We will use a fixed "clocked bit" type CB

type CB = Stream Bool

-- The circuit types could use CB, but we normally use a more general
-- type variable a for a signal, and constrain a to be in the CBit
-- class.

myreg1 :: CBit a => a -> a -> (a,a)
myreg1 ld x = (r, q)
  where r = dff q
        q = or2 (and2 (inv ld) r) (and2 ld x)

-- The top level circuit interface uses a record with named fields.
-- We could use a tuple instead, but the named fields work better for
-- larger circuits, so we will always use the approach for top level
-- circuits.  The signal names in these records begin with in_ or out_
-- which helps to avoid top level name clashes.  There should be only
-- one input to the circuit named x, but there might be other top
-- level entities named x.  So we use the input name "in_x" to avoid
-- any possibility of confusion.

data CircInputs = CircInputs
  { in_ld ::  CB   -- load control signal
  , in_x ::  CB    -- data input
  }

data CircOutputs = CircOutputs
  { out_r ::  CB   -- current state of the register
  , out_q ::  CB   -- input to the dff; will become state after next clock tick
  }

myCirc :: CircInputs -> CircOutputs
myCirc inp = CircOutputs {out_r, out_q}
  where (out_r, out_q) = myreg1 (in_ld inp) (in_x inp)

--------------------------------------------------------------------------------
-- Command interpreter using IORef to allow establishing input dynamically
--------------------------------------------------------------------------------

doNtimes :: Int -> StateT SysState IO () -> StateT SysState IO ()
doNtimes i f
  | i == 0   = return ()
  | otherwise = do
      liftIO $ putStrLn ("repeating " ++ show i ++ " more times")
      f
      doNtimes (i-1) f
        
cliReadInput :: StateT SysState IO ()
cliReadInput = do
  liftIO $ putStrLn "cliReadInput starting"
  doNtimes 10 cliReadStep
  liftIO $ putStrLn "CLI finishing"
  return ()

cliReadStep :: StateT SysState IO ()
cliReadStep = do
  liftIO $ putStrLn "cliReadStep starting"
  s <- get
  let i = cycleCount s
  liftIO $ putStrLn ("Cycle " ++ show i)
  
  ld_readval <- liftIO $ readSigBool "  enter ld"
  liftIO $ writeIORef (ldInpBuffer s) ld_readval

  x_readval <- liftIO $ readSigBool "  enter x"
  liftIO $ writeIORef (xInpBuffer s) x_readval
  
  let inp = inputs s

  let lds = in_ld inp
  let ldval = current lds
  let ldsContinue = future lds

  let xs = in_x inp
  let xval = current xs
  let xContinue = future xs

  let inpNext = CircInputs {in_ld = ldsContinue, in_x = xContinue}
  
  let outp = outputs s

  let rs = out_r outp
  let rval = current rs
  let rContinue = future rs

  let qs = out_q outp
  let qval = current qs
  let qContinue = future qs

  let outpNext = CircOutputs {out_r = rContinue, out_q = qContinue}

  liftIO $ putStrLn ("Cycle " ++ show i)
  liftIO $ putStrLn ("   Inputs   ld = " ++ show ldval ++ "   x = " ++ show xval)
  liftIO $ putStrLn ("   Outputs  r = " ++ show rval ++ "    q = " ++ show qval)

  put s { inputs = inpNext, outputs = outpNext, cycleCount = i+1 }
  liftIO $ putStrLn "cliReadStep finished"

--------------------------------------------------------------------------------
-- Command interpreter using fixed input list: ok for batch but not interactive
--------------------------------------------------------------------------------

{-
cliWithInputList :: StateT SysState IO ()
cliWithInputList = do
  liftIO $ putStrLn "cliWithInputList starting"
  size <- liftIO $ getTerminalSize
  case size of
    Nothing -> liftIO $ putStrLn ("Cannot determine terminal size")
    Just (x,y) -> liftIO $ putStrLn ("terminal size: " ++ show x ++ ", " ++ show y)

  let input_data =
        [ [0, 0]  -- 0
        , [1, 1]  -- 1
        , [0, 0]  -- 2
        , [0, 0]  -- 3
        , [1, 0]  -- 4
        , [0, 0]  -- 5
        , [0, 0]  -- 6
        , [1, 1]  -- 7
        , [0, 0]  -- 8
        , [0, 0]  -- 9
        , [1, 1]  -- 10
        , [0, 0]  -- 11
        , [0, 0]  -- 12
        , [1, 0]  -- 13
        , [0, 0]  -- 14
        , [0, 0]  -- 15
        , [0, 0]  -- 16
        ]
--  let ld_inputs = listStream . map ((==1) . (!! 0)) input_data
--  let x_inputs  = listStream . map ((==1) . (!! 1)) input_data

  let ld_data = map (==1) (map (!! 0) input_data)
  let ld_inputs = listStream ld_data
  liftIO $ putStrLn ("ld_inputs data = " ++ show ld_data)

  let x_data =  map (==1) (map (!! 1) input_data)
  let x_inputs = listStream x_data
  liftIO $ putStrLn ("x_inputs data = " ++ show x_data)
  
--  let ld_inputs = listStream  (map (==1) (map (!! 0) input_data))
--  let x_inputs = listStream   (map (==1) (map (!! 1) input_data))

  let cinp = CircInputs { in_ld = ld_inputs, in_x = x_inputs }
  let coutp = myCirc cinp

--  modify (\s -> s {inputs = cinp, outputs = coutp})

  s <- get
  let s2 = s {inputs = cinp, outputs = coutp}
  put s2

  liftIO $ putStrLn "CLI stepping"
  cliStep
  cliStep
  cliStep
  cliStep
  cliStep
  cliStep
  cliStep
  cliStep
  cliStep
  cliStep
  cliStep
  cliStep
  cliStep
  cliStep
  cliStep
  cliStep
  cliStep
  cliStep
  liftIO $ putStrLn "CLI finishing"
  return ()


cliStep :: StateT SysState IO ()
cliStep = do
  liftIO $ putStrLn "cliStep starting"
  s <- get
  let inp = inputs s
  let lds = in_ld inp
  let ldval = current lds
  let ldsContinue = future lds
  let xs = in_x inp
  let xval = current xs
  let xContinue = future xs
  let inpNext = CircInputs {in_ld = ldsContinue, in_x = xContinue}
  
  let outp = outputs s
  let rs = out_r outp
  let rval = current rs
  let rContinue = future rs
  let qs = out_q outp
  let qval = current qs
  let qContinue = future qs
  let outpNext = CircOutputs {out_r = rContinue, out_q = qContinue}

  let i = cycleCount s

  liftIO $ putStrLn ("Cycle " ++ show i)
  liftIO $ putStrLn ("   Inputs   ld = " ++ show ldval ++ "   x = " ++ show xval)
  liftIO $ putStrLn ("   Outputs  r = " ++ show rval ++ "    q = " ++ show qval)

  put s { inputs = inpNext, outputs = outpNext, cycleCount = i+1 }
  liftIO $ putStrLn "cliStep finished"
  
-}


{-
stepper :: StateT (SysState Bool) IO ()
stepper = do
  s <- get
  let i = cycleCount s
  if i < 3
    then do step
            stepper
    else return ()
-}

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

{-
step :: StateT (SysState Bool) IO ()
step = do
  s <- get
  let i = cycleCount s
  liftIO $ putStrLn ("stepper " ++ show i)
  liftIO $ writeIORef (xInpBuffer s) True
  liftIO $ writeIORef (yInpBuffer s) False
  let xxs = xs s
  let yys = ys s
  liftIO $ putStrLn ("x = " ++ show (head xxs))
  liftIO $ putStrLn ("y = " ++ show (head yys))
  put (s {cycleCount = i+1, xs = tail xxs, ys = tail yys})
  return ()
-}

--------------------------------------------------------------------------------
-- Formatting simulation output
--------------------------------------------------------------------------------

data Fmt a
  = FmtString String
  | FmtBit a
  | FmtBits [a]

{-
driver = foo
  where
    inp = [[0,1,0],
           [1,0,0],
           [0,0,0]]
    f = [FmtString "hello"
-}

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

-- For using text colors in terminal output...
--  setSGR [SetColor Foreground Vivid Red]
--  setSGR [SetColor Background Vivid Blue]
--  putStrLn "Red-On-Blue"
--  setSGR [Reset]  -- Reset to default colour scheme
--  putStrLn "Default colors."

{-
  setSGR [SetColor Foreground Vivid Red]
--  setSGR [SetColor Background Vivid Blue]
  putStrLn "This text is red"
  setSGR [Reset]  -- Reset to default colour scheme
  putStrLn "This text appears in the default colors"
  setSGR [SetColor Foreground Vivid Blue]
  putStrLn "This text is blue"
  setSGR [Reset]  -- Reset to default colour scheme
  putStrLn "Back to the default colors"
-}

--------------------------------------------------------------------------------
-- Testing
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Effects experiment
--------------------------------------------------------------------------------

-- Simple test case test1: create IORefs to hold input values, use
-- effects to perform the read actions, and print out elements of the
-- resulting lists.

{-
test1 :: IO ()
test1 = do
  xval <- newIORef 0
  yval <- newIORef 0
  let xs = repeat (readIORef xval)
  let ys = repeat (readIORef yval)
  let xvals = effects xs
  let yvals = effects ys

  writeIORef xval 2
  writeIORef yval 3
  putStrLn (show (xvals !! 0))
  putStrLn (show (yvals !! 0))

  writeIORef xval 7
  writeIORef yval 8
  putStrLn (show (xvals !! 1))
  putStrLn (show (yvals !! 1))

  writeIORef xval 34
  writeIORef yval 35
  putStrLn (show (xvals !! 2))
  putStrLn (show (yvals !! 2))
  
  putStrLn "test1 done"


-- more tests that can run in CLI...
-}

{-
  code <- boot
  liftIO $ putStrLn ("Object code = " ++ show code)
  let limit = 200
  liftIO $ run_Sigma16_executable code limit
  liftIO $ putStrLn "M1 Run finished"
-}

{-
  aa <- liftIO $ readSigBool "aa"
  bb <- liftIO $ readSigBool "bb"
  cc <- liftIO $ readSigBool "cc"
  liftIO $ putStrLn ("aa=" ++ show aa ++ "  bb=" ++ show bb ++ "   cc=" ++ show cc)
-}

{-
  liftIO $ putStrLn "running test1"
  liftIO test1
-}

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
-- test program
--------------------------------------------------------------------------------

looper :: Int -> IO ()
looper i
  | i > 10000 = do
      return ()
  | otherwise = do
      x <- readIfReady
      case x of
        Nothing -> do
          threadDelay 1000  -- do some computing
          looper (i+1)
        Just xs -> do
          putStrLn ("got xs = " ++ xs)
          looper (i+1)

--------------------------------------------------------------------------------
{- deprecated
buffer mode; just using line buffering seems most reliable and portable...
  bufmode0 <- hGetBuffering stdin
  putStrLn ("Original buffer mode is " ++ show bufmode0)
  hSetBuffering stdin NoBuffering
  bufmode1 <- hGetBuffering stdin
  putStrLn ("Set buffer mode to " ++ show bufmode1)


-- Can also do...
--   ghci -package mtl -package parsec
--     :set -package mtl
--     :set -package parsec
--     :load M1/Tools/Run
-- Problems with the following, awkward or can't get them to work...
--   ghc -package parsec -package mtl --make M1/Tools/Run
--   M1/Tools/Run $COREPROGS/Simple/Add.obj.txt

--   $ ghc -e main -package mtl M1/Tools/StreamIO.hs
--   $ ghc -package mtl -package parsec -e main M1/Tools/Run $COREPROGS/Simple/Add.obj.txt
-- $ ghc -package mtl -package parsec -e main M1/Tools/Run Add.obj.txt
-- it ignores the user command option, the file...

-- effects original version:
-- effects [] = []
-- effects (x:xs) = unsafePerformIO x : effects xs

-}
