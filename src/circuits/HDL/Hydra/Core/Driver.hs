{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module       : HDL.Hydra.Circuits.SimDriver
Description  : Tools for writing simulation drivers
Copyright    : (c) John O'Donnell 2016
License      : GPL-3
Maintainer   : john.t.odonnell9@gmail.com
Stability    : experimental

Tools for writing simulation drivers -}

module HDL.Hydra.Core.Driver
 (
-- * re-export  so user doesn't hvae to import Control.Monad.State
   StateT (..), execStateT, liftIO,

-- * New from step driver
   InputLists, selectInputList, storeInputList, getInputList,  testInputLists,
   initialize,
   printLine,
   readBit, readBits,
   getCmdOperand,
   Driver,  driver,  --  DriverMain,
   CmdOp (..), SysState (..), InPort (..), OutPort (..),
   initState,
   inPortBit, inPortWord,
   outPortBit, outPortWord,
   clockCycle,
   readAllInputs, writeBufs, readInput,
   showBSig, printInPorts, printInPort,
   printOutPorts, printOutPort,
   advanceInPorts, advanceOutPorts,
   getCmd, runSimulation,
   
-- * Number system conversions
  intsInt, ints16hex4,

-- * Extracting input signals from input data
-- | The "input side" of a testbench takes inputs provided by the user
-- | and converts them to the signals required as actual inputs to the
-- | circuit.  The user inputs can be written as decimal numbers, and
-- | the functions defined below perform number system conversions
-- | (e.g. converting a decimal number to a word of bits), as well as
-- | generating the internal data structures used to represent
-- | signals.

  getbit, getbit2, getbin, biti, gettc,

-- * Constructing output format
  Format, fmtWordsGeneral,
  clockcycle,
  run, runUntil, runAllInput,

-- * State of testbench
  simstate, setStateWs, fmtIf,

-- * Generating output text from output signals

-- | How to take the output signals from a circuit and format the text
-- | to be printed.

  string, bit, binhex, bindec, tcdec, hex, hexbin, bitstc, bits

 )
where

-- This module supplies a wide variety of data formatting services,
-- including conversions between number systems, converting input
-- strings into internal signal representations, and converting
-- signals to readable strings for output.

import HDL.Hydra.Core.PrimData
import HDL.Hydra.Core.Signal
import HDL.Hydra.Core.SigStream
import HDL.Hydra.Core.SigBool

import qualified Data.Map as Map

-- the following from StepDriver...
import Control.Monad.State
import Control.Concurrent
import Data.IORef
import System.Environment
import System.FilePath
import System.IO
import System.IO.Unsafe
import System.Console.ANSI

-- These used in old Driver, drop them?  Now using Control.Monad.State
-- import Control.Monad.Trans.State
-- import Control.Monad.Trans.Class

------------------------------------------------------------------------
-- Top level driver
------------------------------------------------------------------------

type Driver a = IO (SysState a)

driver ::  (StateT (SysState a) IO ()) ->IO (SysState a)
driver f = execStateT f initState

------------------------------------------------------------------------
-- Interface to driver
------------------------------------------------------------------------


{-
Functions that can be used in driver
  storePreparedInputData i data
  selectPreparedInputData i
  selectInteractiveInputData
  useInputData              convert strings to numbers for input data
  generateInputData         arbitrary function from string to list of inputs
  selectInputInteractive
  establishInputs
  currentFormat i  - do current actions for format i
  currentInPorts
  currentOutPorts
  advance

  clockCycle

  testInputLists
-}

------------------------------------------------------------------------
-- Interactive commands
------------------------------------------------------------------------

type Command a = [String] -> StateT (SysState a) IO ()
type Commands a = Map.Map String (Command a)

defineCommand :: String -> (Command a) -> StateT (SysState a) IO ()
defineCommand name cmd = do
  s <- get
  let cmds' = Map.insert name cmd (commands s)
  put (s {commands = cmds'})

lookupCommand :: String -> StateT (SysState a) IO (Maybe (Command a))
lookupCommand key = do
  s <- get
  return $ Map.lookup key (commands s)

-- Default commands.  The simulation driver can define additional
-- commands, or override these default ones.

initialize :: StateT (SysState a) IO ()
initialize = do
  defineCommand "step" clockCycle
  defineCommand "s"    clockCycle
  defineCommand "quit" quit
  defineCommand "q"    quit

{-
Interactive user  commands
  > step  (also just enter with blank line)    -- do one clock cycle
  empty/blank line                            -- same as > step
  > step i                                     -- do i clock cycles
  > run                                        -- repeat step until halt or ^C
  > while predicate                            -- repeat step while pred is True
  > until predicate                            -- repeat step while pred is False
  > quit                                       -- terminate simulation
  > foobar  command defined in the driver
-}

--------------------------------------------------------------------------------
-- Input lists
--------------------------------------------------------------------------------

-- An input list is a list of strings; each string can be parsed to
-- obtain the values of input signals for a clock cycle.  There may be
-- several input lists; this enables a driver to provide operations
-- that require several clock cycles, for example dumping a register
-- file.  The input lists are maintained in a finite map, with a
-- string as the key, and the value is a list of strings.

-- https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html

type InputLists = Map.Map String (Int, [String])

selectInputList :: String ->  StateT (SysState a) IO ()
selectInputList selectedKey = do
  s <- get
  put $ s {selectedKey}

storeInputList :: String -> [String] -> StateT (SysState a) IO ()
storeInputList key xs = do
  s <- get
  let fm = inputLists s
  let fm' = Map.insert key (0,xs) fm
  put $ s {inputLists = fm'}

getInputList :: StateT (SysState a) IO (Maybe String)
getInputList = do
  s <- get
  let fm = inputLists s
  let key = selectedKey s
  case Map.lookup key fm of
    Just (i,xs) -> do
      let fm' = Map.insert key (i+1,xs) fm
      put (s {inputLists = fm'})
      return $ if i < length xs then Just (xs!!i) else Nothing
    Nothing -> return Nothing

testInputLists :: StateT (SysState a) IO ()
testInputLists = do
  storeInputList "abc" ["cats", "and", "dogs"]
  storeInputList "def" ["hot", "cold"]
  storeInputList "ghi" ["blue", "red", "green"]
  selectInputList "def"
  a <- getInputList
  b <- getInputList
  selectInputList "abc"
  c <- getInputList
  selectInputList "ghi"
  d <- getInputList
  selectInputList "abc"
  e <- getInputList
  f <- getInputList
  g <- getInputList
  printLine ("a = " ++ show a)
  printLine ("b = " ++ show b)
  printLine ("c = " ++ show c)
  printLine ("d = " ++ show d)
  printLine ("e = " ++ show e)
  printLine ("f = " ++ show f)
  printLine ("g = " ++ show g)
  
--------------------------------------------------------------------------------
-- State
--------------------------------------------------------------------------------

type CB = Stream Bool
      
data SysState a = SysState
  { running :: Bool
  , commands :: Commands a
  , cycleCount :: Int
  , inPortList :: [InPort]
  , nInPorts :: Int
  , outPortList :: [OutPort]
  , formatList :: [[Format Bool a]]
  , inputLists :: InputLists
  , selectedKey :: String
  , userState :: Maybe a
  }

initState :: SysState a
initState = SysState
  { running = True
  , commands = Map.empty
  , cycleCount = 0
  , inPortList = []
  , nInPorts = 0
  , outPortList = []
  , formatList = []
  , inputLists = Map.empty
  , selectedKey = ""
  , userState = Nothing
  }

--------------------------------------------------------------------------------
-- Ports
--------------------------------------------------------------------------------

-- An input port must be defined for each input, because it holds the
-- buffer in which the input data is placed.  The system maintains a
-- list of all InPorts in the state.

data InPort
  = InPortBit
      { inPortName :: String
      , inbsig  :: Stream Bool
      , inbuf :: IORef Bool
      , fetcher :: [IO Bool]
      , inPortIndex :: Int
--      , inBitRead :: String -> Maybe Bool
      }
  | InPortWord
      { inPortName :: String
      , inwsig  :: [Stream Bool]
      , inwbufs :: [IORef Bool]
      , wfetchers :: [[IO Bool]]
      , inwsize :: Int
      , inPortIndex :: Int
--      , inWordRead :: String -> Int -> Maybe [Bool]
--      , readw :: (String -> Int -> IO [Bool])
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

-- make a new inport bit

inPortBit :: String -> StateT (SysState a) IO InPort
inPortBit inPortName = do
  inbuf <- liftIO $ newIORef False
  let fetcher = repeat (readIORef inbuf)
  let inbsig = listeffects fetcher
  s <- get
  let inPortIndex = nInPorts s
--printLine ("inPortBit " ++ inPortName ++ " " ++ show inPortIndex)
  let port = InPortBit { inPortName, inbsig, inbuf, fetcher, inPortIndex }
  put $ s {inPortList = inPortList s ++ [port], nInPorts = inPortIndex + 1}
  return port

-- make a new inport word

inPortWord :: String -> Int -> StateT (SysState a) IO InPort
inPortWord inPortName inwsize = do
  inwbufs <- liftIO $ mkInBufWord inwsize
  let wfetchers = map (\x -> repeat (readIORef x)) inwbufs
  let inwsig = map listeffects wfetchers
  s <- get
  let inPortIndex = nInPorts s
  let port = InPortWord { inPortName, inwsig, inwbufs, wfetchers,
                          inwsize, inPortIndex }
  put $ s {inPortList = inPortList s ++ [port], nInPorts = inPortIndex + 1}
  return port
--  liftIO $ putStrLn ("inPortWord len inwbufs = " ++ show (length inwbufs))
--  liftIO $ putStrLn ("inPortWord len wfetchers = " ++ show (length wfetchers))
--  liftIO $ putStrLn ("inPortWord len inwsig = " ++ show (length inwsig))

mkInBufWord :: Int -> IO [IORef Bool]
mkInBufWord n
  | n == 0 = return []
  | n > 0  = do x <- newIORef False
                xs <- mkInBufWord (n-1)
                return (x:xs)

-- make a new outport bit

outPortBit :: String -> CB -> StateT (SysState a) IO OutPort
outPortBit outPortName outbsig = do
  s <- get
  let port =  OutPortBit { outPortName, outbsig }
  s <- get
  put $ s {outPortList = outPortList s ++ [port]}
  return port

-- make a new outport word

outPortWord :: String -> [CB] -> ([Bool]->String) -> StateT (SysState a) IO OutPort
outPortWord outPortName outwsig showw = do
  s <- get
  let outwsize = length outwsig
  let port =  OutPortWord { outPortName, outwsig, outwsize, showw }
  put $ s {outPortList = outPortList s ++ [port]}
  return port

--------------------------------------------------------------------------------
-- Clock cycle
--------------------------------------------------------------------------------

quit :: Command a
quit args = do
  printLine "this is the quit command..."

clockCycle :: Command a
clockCycle args = do
  printLine "clockCycle..."
  s <- get
  let i = cycleCount s
  let inp = inPortList s
  let outp = outPortList s
  liftIO $ putStrLn (take 70 (repeat '-'))
  liftIO $ putStrLn ("Cycle " ++ show i)
  establishInputs
  printInPorts
  printOutPorts
  advanceInPorts
  advanceOutPorts
  s <- get
  put (s {cycleCount = i+1})

--------------------------------------------------------------------------------
-- Establish inputs
--------------------------------------------------------------------------------

-- decide whether the input data for the current clock cycle should be
-- taken from an input data list or from a user interaction.
-- Provisional approach: if there is input datta in the state, use it;
-- otherwise ask the user to provide it

establishInputs :: StateT (SysState a) IO ()
establishInputs = do
  d <- getInputList
  case d of
    Just xs -> do
      printLine ("Taking inputs from stored data: " ++ xs)
      takeInputsFromList xs
    Nothing -> do
      printLine "Reading inputs from user:"
      readAllInputs

takeInputsFromList :: String -> StateT (SysState a) IO ()
takeInputsFromList line = do
  s <- get
  let ports = inPortList s
  let xs = words line
  mapM_ takeInput (zip ports xs)
  return ()

-- for now, the input must be a number
takeInput :: (InPort, String) -> StateT (SysState a) IO ()
takeInput (p,x) = do
  s <- get
  case p of
    InPortBit {..} -> do
      let y = read x :: Int
      let b = y==1
      liftIO $ writeIORef inbuf b
    InPortWord {..} -> do
      let y = read x :: Int
      let bs = undefined -- intToWord inwsize x ?????????????????
      liftIO $ writeBufs inwbufs bs

readAllInputs :: StateT (SysState a) IO ()
readAllInputs = do
  s <- get
  let ports = inPortList s
  mapM_ readInput ports
  return ()

readInput :: InPort -> StateT (SysState a) IO ()
readInput (InPortBit {..}) = do
--  x <- liftIO $ readSigBool ("  enter " ++ inPortName)
  x <- liftIO $ interactiveReadBit inPortName
  liftIO $ writeIORef inbuf x
readInput (InPortWord {..}) = do
--  xs <- liftIO $ readw inPortName inwsize
  xs <- liftIO $ interactiveReadWord inPortName inwsize
--  liftIO $ putStrLn ("readInput xs = " ++ show xs)
  liftIO $ writeBufs inwbufs xs
  return ()
--  readval <- liftIO $ readSigString ("  enter " ++ inPortName)

writeBufs :: [IORef Bool] -> [Bool] -> IO ()
writeBufs rs bs = mapM_ (\(r,b) -> writeIORef r b ) (zip rs bs)

--------------------------------------------------------------------------------
-- Operations during cycle
--------------------------------------------------------------------------------

-- Int -> Int -> [Int] ... intbin k x
-- [Int] -> Int...  binint tcint
showBSig :: Bool -> String
showBSig False  = "0"
showBSig True = "1"

printInPorts :: StateT (SysState a) IO ()
printInPorts = do
  s <- get
  let ports = inPortList s
  let i = cycleCount s
  liftIO $ putStrLn ("Input signals during cycle " ++ show i)
  mapM_ printInPort ports

printInPort :: InPort -> StateT (SysState a) IO ()
printInPort (InPortBit {..}) = do
  let x = current inbsig
  let xs = "    " ++ inPortName ++ " = " ++ showBSig x
  liftIO $ putStrLn xs
printInPort (InPortWord {..}) = do
  let xs = map current inwsig
  let ys = "    " ++ inPortName ++ " = " ++ show xs
  liftIO $ putStrLn ys

printOutPorts :: StateT (SysState a) IO ()
printOutPorts = do
  s <- get
  let ports = outPortList s
  let i = cycleCount s
  liftIO $ putStrLn ("Output signals during cycle " ++ show i)
  mapM_ printOutPort ports

printOutPort :: OutPort -> StateT (SysState a) IO ()
printOutPort (OutPortBit {..}) = do
  let x = current outbsig
  let xs = "    " ++ outPortName ++ " = " ++ showBSig x
  liftIO $ putStrLn xs
printOutPort (OutPortWord {..}) = do
  let xs = showw (map current outwsig)
  let ys = "    " ++ outPortName ++ " = " ++ xs
  liftIO $ putStrLn ys

opCurrentPorts :: StateT (SysState a) IO ()
opCurrentPorts = do
  printInPorts
  printOutPorts

opCurrentFormat :: Int -> StateT (SysState a) IO ()
opCurrentFormat i = do
  s <- get
  let fmt = formatList s !! i
  return ()
  
--------------------------------------------------------------------------------
-- Clock tick: advance the streams
--------------------------------------------------------------------------------

advance :: StateT (SysState a) IO ()
advance = do
  advanceInPorts
  advanceOutPorts
--  advanceFormats


advanceInPorts :: StateT (SysState a) IO ()
advanceInPorts = do
  s <- get
  let ports = inPortList s
  ports' <- mapM advanceInPort ports
  put $ s {inPortList = ports'}

advanceInPort :: InPort -> StateT (SysState a) IO InPort
advanceInPort (InPortBit {..}) = do
  return $ InPortBit {inbsig = future inbsig, ..}
advanceInPort (InPortWord {..}) = do
  let xs  = map future inwsig
  return $ InPortWord {inwsig = xs, ..}

advanceOutPorts :: StateT (SysState a) IO ()
advanceOutPorts = do
  s <- get
  let ports = outPortList s
  ports' <- mapM advanceOutPort ports
  put $ s {outPortList = ports'}

advanceOutPort :: OutPort -> StateT (SysState a) IO OutPort
advanceOutPort (OutPortBit {..}) =
  return $ OutPortBit {outPortName, outbsig = future outbsig}
advanceOutPort (OutPortWord {..}) = do
  let xs = map future outwsig
  return $ OutPortWord {outPortName, outwsig=xs, outwsize, showw}

------------------------------------------------------------------------
-- Reading bits and words
------------------------------------------------------------------------

-- Given a port name, prompt the user to enter a string of bits
-- (e.g. 100101), confert to [Bool] and return

-- Should drop whitespace and validate input

readBit :: String -> Maybe Bool
readBit x =
  if head x == '1'
  then Just True
  else if head x == '0'
       then Just False
       else Nothing

interactiveReadBit :: String -> IO Bool
interactiveReadBit portname = do
  putStrLn "interactiveReadBit"
  putStrLn ("Enter value of " ++ portname ++ " (0 or 1, then enter)")
  ys <- getLine
  return (if head ys == '1' then True else False)

readSigBool :: String -> IO Bool
readSigBool xs = do
  putStrLn ("Enter value of " ++ xs ++ " (either 0 or 1, then enter)")
  putStrLn xs
  ys <- getLine
  return (if head ys == '1' then True else False)

-- Should drop whitespace and validate input

readBits :: String -> Int -> Maybe [Bool]
readBits xs n =
  if length xs == n
    then Just (map (== '1') xs)
    else Nothing

interactiveReadWord :: String -> Int -> IO [Bool]
interactiveReadWord portname n = do
  putStrLn "interactiveReadWord"
  putStrLn ("   Enter " ++ portname ++ " (" ++ show n ++ " bits)")
  xs <-getLine
  case readBits xs n of
    Just r -> return r
    Nothing -> interactiveReadWord portname n

-- Quick version of a function to read a signal value.  Requires first
-- char to be either '1' or '0'.  Later, should parse the input
-- properly

readSigString :: String -> IO String
readSigString xs = do
  putStrLn xs
  ys <- getLine
  putStrLn ("readSigString = <" ++ ys ++ ">")
  return ys

---------------------------------------------------------------------------
--		       Simulation Driver Input
---------------------------------------------------------------------------

-- The input section of a simulation driver is constructed using
-- getbit, getbin, gettc, and ...

------------------------------------------------------------------------
-- Extract input signals from integer lists
------------------------------------------------------------------------

-- | Convert an input number (which should be 0 or 1) into an input
-- | bit signal.

getbit :: (Signal a, Static a) => [[Int]] -> Int -> Stream a
getbit xs i =
  Cycle (intSig (head xs !! i)) (getbit (tail xs) i)

-- | Convert an input number (which should be between 0 and 3) to a
-- | pair of input bit signals.

getbit2
  :: (Signal a, StaticBit a)
  => [[Int]] -> Int -> (Stream a, Stream a)
getbit2 xs i = (getbini 2 0 xs i, getbini 2 1 xs i)

getbit20, getbit21
  :: (Signal a, StaticBit a)
  => [[Int]] -> Int -> Stream a
getbit20 xs i =
  Cycle (bit20 (head xs !! i)) (getbit20 (tail xs) i)
getbit21 xs i =
  Cycle (bit21 (head xs !! i)) (getbit20 (tail xs) i)

bit20, bit21 :: (Signal a, Static a) => Int -> a
bit20 x = intSig ((x `mod` 4) `div` 2)
bit21 x = intSig (x `div` 2)

-- Makes a stream consisting of the j'th bit of the k-bit binary
-- representation of the i'th column of the list xs.

getbini k j xs i =
  Cycle (biti k j (head xs !! i)) (getbini k j (tail xs) i)

-- Build a k-bit word (list of streams) which is the binary
-- representation of the i'th column of xs::[[Int]].

getbin :: (Signal a, StaticBit a)
  => Int -> [[Int]] -> Int -> [Stream a]
getbin k xs i = [getbini k j xs i | j <- [0..k-1]]

-- Build a k-bit word (list of streams) which is the two's complement
-- representation of the i'th column of xs::[[Int]].

gettc k xs i = [getbini k j (map (map (tc k)) xs) i | j <- [0..k-1]]

-- The biti function gives the i'th bit of the k-bit binary
-- representation of the integer x. The wordsize is k, i is the bit
-- index, x is the integer input Bits are numbered from the left
-- starting with 0, thus bit 0 is the most significant bit.

biti :: (Signal a, StaticBit a) => Int -> Int -> Int -> a
biti k i x = boolSig (odd (x `div` (2^(k-(i+1)))))

---------------------------------------------------------------------------
--		       Simulation Driver Output
---------------------------------------------------------------------------

format :: [Format Bool b] -> StateT (SysState b) IO ()
format xs = do
  s <- get
  let fs = formatList s
  put (s {formatList = fs ++ [xs]})

runFormat :: StateT (SysState a) IO ()
runFormat = do
  printLine "runFormat starting"
  printLine "runFormat finished"

------------------------------------------------
-- Representation of output format specification
------------------------------------------------

-- The output section of a simulation driver uses format specifiers to
-- control how various signals are printed.  These format specifiers
-- are string, bit, bindec, binhex, tcdec.  The base signal type is a
-- (e.g. Bool), and b is the driver state type

data Format a b
  = FmtCycle (Int -> String)
  | FmtString String
  | FmtState ((Int,b) -> String)
  | FmtSetStateWords ((Int,b)->[[a]]->b) [[Stream a]]
  | FmtBit (String -> Stream a -> (String, Stream a))
      String (Stream a)
  | FmtWord
      (String -> Int -> Int -> [Stream a]
         -> (String, [Stream a]))
      String Int Int [Stream a]
  | FmtHex (Int -> [Stream a] -> (String, [Stream a])) Int [Stream a]
  | FmtIf (Stream a) [Format a b] [Format a b]
  | FmtWordsGeneral ([[Int]]->String) [[Stream a]]

--------------------------------------------------------
-- User functions for constructing format specifications
--------------------------------------------------------

bit x = FmtBit fmtBit [] x
bits x = FmtWord fmtWord [] n n x
  where n = length x

-- | string xs is a format specifier that will put the literal string
-- | xs into the output.  This is useful for inserting labels into the
-- | output.

string :: String -> Format a b
string x = FmtString x

simstate f = FmtState f

clockcycle f = FmtCycle f
setStateWs f xs = FmtSetStateWords f xs

bindec w x = FmtWord fmtBin [] w (length x) x

binhex x = FmtHex fmtHex (length x) x  -- deprecated, use hex instead
hex x = FmtHex fmtHex (length x) x
bitstc w x = FmtWord fmtTC  [] w (length x) x  -- deprecated, use tcdec

tcdec w x = FmtWord fmtTC  [] w (length x) x

fmtWordsGeneral f xs = FmtWordsGeneral f xs

----------------------------------------
-- Convert current cycle value to string
----------------------------------------

-- The fmtBit function converts a bit signal for output.  The argument
-- x is a bit signal, and a character is output each clock cycle
-- giving the value of the signal.

-- ?? todo remove label

fmtBit :: (Signal a, Static a) => String -> Stream a -> (String, Stream a)
fmtBit lbl x = (showSig (current x), future x)

-- A word signal (a list of bit signals) can be formatted for output
-- in a variety of ways.  To display it as a sequence of bits, specify
-- the format with fmtWord.

fmtWord
 :: (Signal a, Static a)
  => String -> Int -> Int -> [Stream a] -> (String, [Stream a])
fmtWord lbl w k x =
  let z = setlength w (concat (map (showSig . current) x))
  in (z, map future x)

-- A word signal can be converted to a decimal integer for output.  If
-- the word is binary, use the fmtBin function, but if it's two's
-- complement, use fmtTC.

fmtBin, fmtTC
  :: (Signal a, Static a)
  => String -> Int -> Int -> [Stream a] -> (String, [Stream a])

fmtBin lbl w k x =
  let z = map (sigInt . current) x
      n = sum [b * 2^(k-(i+1)) | (b,i) <- zip z [0..]]
  in (setlength w (show n), map future x)

fmtTC lbl w k x =
  let z = map (sigInt . current) x
      n = sum [b * 2^(k-(i+1)) | (b,i) <- zip z [0..]]
      m = if head z == 0
          then n
          else n - 2^k
  in (setlength w (show m), map future x)

-- A sequential word signal x can be converted to k-digit hexadecimal
-- using fmtHex k x.

fmtHex :: Static a => Int -> [Stream a] -> (String, [Stream a])
fmtHex k x =
  let z = map (sigInt . current) x
      j = k `mod` 4
      z' = take j (repeat 0) ++ z
      s = [hexDig (field z' i 4) | i <- [0,4..k-1]]
  in (s, map future x)

fmtIf x fs1 fs0 = FmtIf x fs1 fs0

--------------------------------
-- Produce output cycle by cycle
--------------------------------
-- Monadic version of run

-- (i,st) is state; i is cycle number, st is user's defined state

-- | Run a simulation until the end of the list of inputs is reached.

run :: (Signal a, StaticBit a)
  => b -> [[Int]] -> [Format a b] -> IO ()
run initst inps outs =
  do _ <- execStateT (mrun' (length inps) 0 outs) (0,initst)
     return ()

mrun' :: (Signal a, StaticBit a) =>
  Int -> Int -> [Format a b] -> StateT (Int,b) IO ()
mrun' limit i xs =
  if i<limit
  then do xs' <- mstep i xs
          mrun' limit (i+1) xs'
  else return ()

-- | Run a simulation until a termination predicate is satisfied.

runUntil :: (Signal a, StaticBit a) =>
  b -> ((Int,b)->Bool) -> [[Int]] -> [Format a b] -> IO ()
runUntil initst p inps outs =
  do _ <- execStateT (mrunUntil p outs) (0,initst)
     return ()

-- Tools for using runUntil, especially with simple format

termpred :: Int -> (Int,a) -> Bool
termpred inputSize (c,s) = (c >= inputSize)

-- | Run the simulation as long as the input list contains more input
-- | data.

runAllInput :: [[Int]] -> [Format Bool ()] -> IO ()
runAllInput input format =
  runUntil () (termpred (length input)) input format


-- version of mrun' with a termination predicate, rather than simply
-- running for a fixed number of cycles

mrunUntil :: (Signal a, StaticBit a) =>
  ((Int,b)->Bool) -> [Format a b] -> StateT (Int,b) IO ()
mrunUntil p xs =
  do s <- get
     if p s
       then return ()
       else do xs' <- mstep 0 xs  -- 0 is dummy, get rid	
               mrunUntil p xs'


mstep :: (Signal a, StaticBit a) =>
  Int -> [Format a b] -> StateT (Int,b) IO [Format a b]
mstep i xs =
  do -- lift $ putStr (setlength 4 (show i))
     -- lift $ putStr ".  "
     ys <- mdofmts xs
     lift $ putStr "\n"
     (cycle,s) <- get
     put (cycle+1,s)
     return ys

-- run mdofmt on each format in a list of formats, printing the
-- results as we go, and returning a new list of formats that will
-- contain the futures of all the streams

mdofmts :: (Signal a, StaticBit a) =>
  [Format a b] -> StateT (Int,b) IO [Format a b]
mdofmts [] = return []
mdofmts (x:xs) =
  do x' <- mdofmt x
     xs' <- mdofmts xs
     return (x':xs')

------------------------------------------------------------------------
-- Process one fmt item
------------------------------------------------------------------------

-- process a format item by printing the value for the current clock
-- cycle, and return the format for the future clock cycles
-- (essentially, this means to print the head of the stream, and
-- return the tail)

-- this is the key operation in the old-style driver.  It processes
-- the current value and returns the future value

mdofmt :: (Signal a, StaticBit a)
  => Format a b -> StateT (Int,b) IO (Format a b)

mdofmt (FmtCycle f) =
  do (cycle,_) <- get
     lift $ putStr (f cycle)
     return (FmtCycle f)

mdofmt (FmtState f) =
  do s <- get
     lift $ putStr (f s)
     return (FmtState f)

mdofmt (FmtSetStateWords f xs) =
  do (c,s) <- get
     let ws = map (map current) xs
     let s' = f (c,s) ws
     put (c,s')
     return (FmtSetStateWords f (map (map future) xs))

mdofmt (FmtBit f lbl x) =
  do let (s,x') = f lbl x
     lift $ putStr s
     return (FmtBit f lbl x')
mdofmt (FmtWord f lbl w k x) =
  do let (s,x') = f lbl w k x
     lift $ putStr s
     return (FmtWord f lbl w k x')
mdofmt (FmtHex f k x) =
  do let (s,x') = f k x
     lift $ putStr s
     return (FmtHex f k x')

mdofmt (FmtString s) =
  do lift $ putStr s
     return (FmtString s)

mdofmt (FmtIf x fs_then fs_else) =
  do let b = is1 (current x)
     fs_then' <- (if b then mdofmts else mskipfmts) fs_then
     fs_else' <- (if b then mskipfmts else mdofmts) fs_else
     return (FmtIf (future x) fs_then' fs_else')

mdofmt (FmtWordsGeneral f xs) =
  do let ys = map (map (sigInt . current)) xs
     let zs = map (map future) xs
     let s = f ys
     lift $ putStr s
     return (FmtWordsGeneral f zs)

-- Skip current cycle infor for a list of formats, return futures

mskipfmts :: (Signal a, Static a) =>
  [Format a b] -> StateT (Int,b) IO [Format a b]
mskipfmts [] = return []
mskipfmts (x:xs) =
  do x' <- mskipfmt x
     xs' <- mskipfmts xs
     return (x':xs')

mskipfmt :: (Signal a, Static a) => Format a b -> StateT (Int,b) IO (Format a b)

mskipfmt (FmtCycle f) = return (FmtCycle f)
mskipfmt (FmtString s) = return (FmtString s)
mskipfmt (FmtState f) = return (FmtState f)
mskipfmt (FmtSetStateWords f xs) =
  return (FmtSetStateWords f (map (map future) xs))
mskipfmt (FmtBit f lbl x) =
  return (FmtBit f lbl (future x))
mskipfmt (FmtWord f lbl w k x) =
  return (FmtWord f lbl w k (map future x))
mskipfmt (FmtHex f k x) =
  return (FmtHex f k (map future x))
mskipfmt (FmtIf x fs_then fs_else) =
  do fs_then' <- mskipfmts fs_then
     fs_else' <- mskipfmts fs_else
     return (FmtIf (future x) fs_then' fs_else')
mskipfmt (FmtWordsGeneral f xs) =
  return (FmtWordsGeneral f (map (map future) xs))

---------------------------------------------------------------------------
--		      Number System Conversions
---------------------------------------------------------------------------

intToBits :: Int -> Int -> [Bool]
intToBits x n = reverse (intToBitsf x n)

intToBitsf :: Int -> Int -> [Bool]
intToBitsf x n
  | n == 0 = []
  | n > 0 = (x `mod` 2 /= 0) : intToBitsf (x `div` 2) (n-1)
  
-- convert a list of bits (represented as Ints) to an Int,
-- using binary representation

intsInt :: [Int] -> Int
intsInt xs =
  sum [x * 2^(n-(i+1)) | (x,i) <- zip xs [0..]]
  where n = length xs

-- convert a list of 16 bits (represented as Ints) to a 4-character
-- hex string

ints16hex4 :: [Int] -> String
ints16hex4 xs =
  map (int4hex . intsInt)
   [field xs 0 4, field xs 4 4, field xs 8 4, field xs 12 4]

-- | Convert a hexadecimal number, represented as a string of hex
-- | digits, to an Int.


hexbin :: Int -> String -> Int
hexbin k cs =
  let k' = k `div` 4 
      i = sum [hexDigVal c * 16^(k'-(i+1)) | (c,i) <- zip cs [0..]]
  in i

hextc :: Int -> String -> Int
hextc k cs =
  let i = sum [hexDigVal c * 16^(k-(i+1)) | (c,i) <- zip cs [0..]]
  in if hexDigVal (cs!!0) >=8
    then i - 2^(4*k)
    else i

hexDigVal :: Char -> Int
hexDigVal '0' =  0
hexDigVal '1' =  1
hexDigVal '2' =  2
hexDigVal '3' =  3
hexDigVal '4' =  4
hexDigVal '5' =  5
hexDigVal '6' =  6
hexDigVal '7' =  7
hexDigVal '8' =  8
hexDigVal '9' =  9
hexDigVal 'a' = 10
hexDigVal 'b' = 11
hexDigVal 'c' = 12
hexDigVal 'd' = 13
hexDigVal 'e' = 14
hexDigVal 'f' = 15

int4hex :: Int -> Char
int4hex i = "0123456789abcdef" !! i

hexDig :: [Int] -> Char
hexDig bs =
  let i = sum [b * 2^(4-(i+1)) | (b,i) <- zip bs [0..]]
  in "0123456789abcdef" !! i

{- ?? todo - remove this
hexDig' :: [Int] -> String
hexDig' bs =
  let i = sum [b * 2^(4-(i+1)) | (b,i) <- zip bs [0..]]
  in  show i --"0123456789abcdef" !! i
-}

-- Let xs be a k-bit word whose binary interpretation is n.  Then tc k
-- n gives the two's complement representation of the word.  For
-- example,

--  tc 8 5 = 5    (because 0000 0101 is nonnegative)
--  tc 8 255 = -1 (because 1111 1111 is negative and represents -1)

tc :: Int -> Int -> Int
tc k n =
  if n >= 2^(k-1)
    then n - 2^k
    else n

{-
tc :: Integral a => Word16 -> a
tc x =
  let s = testBit x 15
      y = if s
            then complement x + 1
            else x
      z = fromIntegral y
  in z
-}

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


getCmdOperand :: IO (Maybe String)
getCmdOperand = do
  putStrLn "getCmdOperand"
  args <- getArgs
  putStrLn ("args = " ++ show args)
  let operand = if length args >= 1 then Just (args!!0) else Nothing
  return operand

-- old version, deprecating
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


-- Deprecated???
data CmdOp
  = CmdOpEmpty
  | CmdOpBatch
  | CmdOpCLI
  deriving (Eq,Show)


runSimulation :: StateT (SysState a) IO ()
runSimulation = do
  printLine "Enter command after prompt, h for help"
  s <- get
  case running s of
    False -> return ()
    True -> do
      doCommand
      runSimulation



doCommand :: StateT (SysState a) IO ()
doCommand = do
  liftIO $ putStr "$ "
  xs <- liftIO $ hGetLine stdin
  let ws = words xs
  printLine ("doCommand: ws = " ++ show ws)
  case ws of
    [] -> do
      clockCycle []
    (c:_) -> do
      printLine ("doCommand c = " ++ c)
      mcmd <- lookupCommand c
      case mcmd of
        Nothing -> do
          printLine ("invalid command: " ++ c)
          doCommand
        Just cmd -> do
          printLine "executing command..."
          cmd ws
          printLine "command done..."

helpMessage :: String
helpMessage =
  "Type command character, then Enter\n"
  ++ "help  h     help, display this message\n"
  ++ "step  s     perform one clock cycle"
  ++ "quit  q     return to ghci prompt\n"
  ++ "Enter, with other character, is same as step\n"

{-
doCommand :: StateT (SysState a) IO ()
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
    Help -> do liftIO $ putStrLn helpMessage
    StepCycle -> do
      clockCycle []
      return ()
    _ -> return ()
-}

--------------------------------------------------------------------------------
-- Terminal I/O
--------------------------------------------------------------------------------

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

-- Text field adjustment

setlength :: Int -> String -> String
setlength w xs =
  let n = length xs
  in take (w-n) (repeat ' ') ++ xs

center :: Int -> String -> String
center i s =
  let k = length s
      j = (i - length s) `div` 2
  in constring ' ' j ++ s ++ constring ' ' (i-k-j)

constring :: Char -> Int -> String
constring c i = take i (repeat c)

indent :: Int -> String
indent k = take (2*k) (repeat ' ')

separatorLine :: IO ()
separatorLine = putStrLn (take 60 (repeat '-'))

justs :: [Maybe x] -> [x]
justs [] = []
justs (Nothing : xs) = justs xs
justs (Just x : xs) = x : justs xs

-- Take a list of actions, perform them on demand, and return the result

listeffects :: [IO a] -> Stream a
listeffects = mapListStream unsafePerformIO

mapListStream :: (a->b) -> [a] -> Stream b
mapListStream f x
  = Cycle (f (head x))
          (mapListStream f (tail x))

doNtimes :: Int -> StateT (SysState a) IO () -> StateT (SysState a) IO ()
doNtimes i f
  | i == 0   = return ()
  | otherwise = do
      f
      doNtimes (i-1) f

printLine :: String -> StateT (SysState a) IO ()
printLine xs = liftIO (putStrLn xs)

------------------------------------------------------------------------
-- Deprecated or deleted
------------------------------------------------------------------------
-- From old Driver.hs...
--------------------------------

-- Stream version of run, original approach, no longer in use.  This
-- won't work any more, because of the change from Format a to Format
-- a b, as well as the introduction of new Fmt constructors.

{-
run inps outs = run' (length inps) 0 outs

run' :: (Signal a, Static a) => Int -> Int -> [Format a] -> IO ()
run' limit i xs =
 if i<limit
 then do xs' <- step i xs
         run' limit (i+1) xs'
 else return ()

step :: (Signal a, Static a) => Int -> [Format a] -> IO [Format a]
step i xs =
  do putStr (setlength 4 (show i))
     putStr ".  "
     let ys = map dofmt xs
     let s = concat (map fst ys)
     putStr s
     putStr "\n"
     let xs' = map snd ys
     return xs'

dofmt :: (Signal a, Static a) => Format a -> (String, Format a)
dofmt (FmtBit f lbl x) =
  let (s,x') = f lbl x
  in (s, FmtBit f lbl x')
dofmt (FmtWord f lbl w k x) =
  let (s,x') = f lbl w k x
  in (s, FmtWord f lbl w k x')
dofmt (FmtHex f k x) =
  let (s,x') = f k x
  in (s, FmtHex f k x')
dofmt (FmtString s) = (s, FmtString s)

dofmt (FmtIf x fs1 fs0) =
  let (out1, fs1') = dofmts fs1
      (out0, fs0') = dofmts fs0
      out = case is1 (current x) of
              True -> out1
              False -> out0
  in (out, FmtIf (future x) fs1' fs0')

dofmt (FmtWordsGeneral f xs) =
  let ys = map (map (sigInt . current)) xs
      zs = map (map future) xs
      s = f ys
  in (s, FmtWordsGeneral f zs)

-- used for FmtIf...
dofmts :: (Signal a, Static a) => [Format a] -> (String, [Format a])
dofmts fs =
  let ys = map dofmt fs
      s = concat (map fst ys)
      fs' = map snd ys
  in (s,fs')

-} -- end of stream version of run


-- Generic simulation drivers
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~

-- simdriver
{-
-- module HDL.Hydra.Core.StepDriver where


-- import HDL.Hydra.Core.PrimData
-- import HDL.Hydra.Core.Signal
-- import HDL.Hydra.Core.SigStream
-- import HDL.Hydra.Core.SigBool
-}
--------------------------------------------------------------------------------
-- Formatting simulation output
--------------------------------------------------------------------------------

{-
-- Not used... was going to use these in new StepDriver
data NewFmt a
  = NewFmtString String
  | NewFmtBit a
  | NewFmtBits [a]
-}
