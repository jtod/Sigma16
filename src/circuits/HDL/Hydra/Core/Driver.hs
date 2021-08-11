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

import Control.Monad.Trans.State
import Control.Monad.Trans.Class


---------------------------------------------------------------------------
--		      Number System Conversions
---------------------------------------------------------------------------

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

------------------------------------------------
-- Representation of output format specification
------------------------------------------------

{- The output section of a simulation driver uses format specifiers to
control how various signals are printed.  These format specifiers are
string, bit, bindec, binhex, tcdec. -}

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

-- process a format item by printing the value for the current clock
-- cycle, and return the format for the future clock cycles
-- (essentially, this means to print the head of the stream, and
-- return the tail)

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

-- String Formatting
-- ~~~~~~~~~~~~~~~~~

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


-- Miscellaneous
-- ~~~~~~~~~~~~~

justs :: [Maybe x] -> [x]
justs [] = []
justs (Nothing : xs) = justs xs
justs (Just x : xs) = x : justs xs



------------------------------------------------------------------------
-- Deprecated or deleted
------------------------------------------------------------------------

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
