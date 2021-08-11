{-|
Module       : HDL.Hydra.Circuits.PrimData
Description  : Data types for signal values
Copyright    : (c) John O'Donnell 2016
License      : GPL-3
Maintainer   : john.t.odonnell9@gmail.com
Stability    : experimental

Data types for signal values. -}

module HDL.Hydra.Core.PrimData where

-- This module defines basic datatypes and structures used throughout
-- the program, as well as functions for converting to and from
-- strings.

import Data.Word
import Data.Bits


----------------------------------------------------------------------

-- from the old module Container.hs which has been moved to experiment


-- intbin k x returns a list of k bits giving the binary
-- representation of x. The bits are represented as Ints,
-- either 0 or 1.

intbin :: Int -> Int -> [Int]
intbin k x = loop k x []
  where
    loop i x bs =
      if i==0
        then bs
        else loop (i-1) (div x 2) (mod x 2 : bs)

-- binint bs gives the integer represented by bs, assuming binary
-- tcint bs gives integer represented by bs, assuming two's complement

binint :: [Int] -> Int
binint bs = sum [b * 2^i | (i,b) <- zip [k-1, k-2 .. 0] bs]
  where
    k = length bs

tcint :: [Int] -> Int
tcint bs = x
  where
    k = length bs
    i = binint bs
    x = if i < 2^k
          then i
          else i - 2^k


field :: [a] -> Int -> Int -> [a]
field x a n = [x!!i | i <- [a .. a+n-1]]

msb :: [a] -> a
msb xs = xs !! 0

lsb :: [a] -> a
lsb xs = xs !! (length xs - 1)




----------------------------------------------------------------------


my_print_header_line xs = putStrLn xs -- ??? should import
     
------------------------------------------------------------------------
-- Hexadecimal

hexChars :: String
hexChars = "0123456789abcdefABCDEF"

-- Cleanup, also in Emulator.hs ????

hexval :: Char -> Int
hexval '0' = 0
hexval '1' = 1
hexval '2' = 2
hexval '3' = 3
hexval '4' = 4
hexval '5' = 5
hexval '6' = 6
hexval '7' = 7
hexval '8' = 8
hexval '9' = 9
hexval 'a' = 10
hexval 'b' = 11
hexval 'c' = 12
hexval 'd' = 13
hexval 'e' = 14
hexval 'f' = 15
hexval 'A' = 10
hexval 'B' = 11
hexval 'C' = 12
hexval 'D' = 13
hexval 'E' = 14
hexval 'F' = 15
hexval _   = 0  -- invalid characters are equivalent to 0


------------------------------------------------------------------------
-- Lexical notation

-- should this be in the assembler module?  maybe it's better here...

type Name = String


------------------------------------------------------------------------
-- Representation of words
------------------------------------------------------------------------

-- Convention is to follow "big-endian" notation, with bit 0 in the
-- leftmost (most significant) position.

-- The architecture uses 16 bit words for registers, and 4-bit words
-- for the fields of an instruction and as addresses into the register
-- file.

-- Word16 is defined in module Data.Word
type Word4 = Int

type Address16 = Word16


------------------------------------------------------------------------
-- Constructing and deconstructing words
------------------------------------------------------------------------

-- Build a 16-bit word from four 4-bit words

buildword :: (Word4,Word4,Word4,Word4) -> Word16
buildword (a,b,c,d) =
 fromIntegral a * 16^3 + fromIntegral b * 16^2 +
  fromIntegral c * 16^1 + fromIntegral d * 16^0

-- Split a 16-bit word into four 4-bit words

splitword :: Word16 -> (Word4,Word4,Word4,Word4)
splitword w =   --  redo this using shifting and test it's ok ... ???????
  let w1 = w `div` 16
      w2 = w1 `div` 16
      w3 = w2 `div` 16
  in (fromIntegral (w3 `mod` 16),
      fromIntegral (w2 `mod` 16),
      fromIntegral (w1 `mod` 16),
      fromIntegral (w `mod` 16))

-- Extract a field from a word, given the start and end positions

-- extract a b x gives a field of length b-a+1 bits from x,
-- starting in bit position a and ending in bit position b.

extract :: (Integral a, FiniteBits a) => Int -> Int -> a -> a
extract a b x =
  let n = finiteBitSize x
      y = shiftL x a
      z = shiftR y (a + (n-1) - b)
  in z

bytep, byteq, byter, bytes :: Word32 -> Word8
bytep w = fromIntegral (extract  0  7 w)
byteq w = fromIntegral (extract  8 15 w)
byter w = fromIntegral (extract 16 23 w)
bytes w = fromIntegral (extract 24 31 w)


------------------------------------------------------------------------
-- Output conversions of words
------------------------------------------------------------------------
-- Conversion between integers and hex

-- inthex w i is the w-digit hex representation of i
inthex :: Int -> Int -> ([String], Maybe String)
inthex w i =
  let f xs = take (w - length xs) (repeat '0') ++ xs
      g i = if i==0
              then []
              else let a = i `mod` 16
                       b = i `div` 16
                   in g b ++ [hexDigits!!a]
  in if 0 <= i && i<16^w
       then ([], Just (f (g i)))
     else if (-(2^15)) <= i && i <0
       then ([], Just (f (g (2^16 + i))))
     else (["inthex integer out of range " ++ show i], Nothing)
       
-- inthexok allows larger than  w result
--  i is the w-digit hex representation of i

inthexok :: Integral a => Int -> a -> String
inthexok w i =
  let f xs = take (w - length xs) (repeat '0') ++ xs
      g i = if i==0
              then []
              else let a = fromIntegral i `mod` 16
                       b = fromIntegral i `div` 16
                   in g b ++ [hexDigits!!a]
  in f (g i)


------------------------------------------------------------------------
-- Formatting Integers


fmtdecint :: Int -> Int -> String
fmtdecint w n =
  let xs = show n
  in take (w - length xs) (repeat ' ') ++ xs

fmtInt :: Int -> Int -> String
fmtInt w i =
  let xs = show i
  in take (w - length xs) (repeat ' ') ++ xs


word16_hex4 :: Word16 -> ([String], Maybe Hex4)
word16_hex4 i = inthex 4 (fromIntegral i)


{- 
 --  ([], Just "abcd")
 if i>= 2^16
    then (["Constant too large: " ++ show i], Nothing)
    else if i<0
           then (["Negatives not supported yet: " ++ show i], Nothing)
           else ([], Just (inthex 4 i))
-}

  


------------------------------------------------------------------------
-- Converting word to hex strings
------------------------------------------------------------------------


fmthexint :: Int -> Int -> String
fmthexint w n =
  if n<0
    then error "hex number negative"
    else hexloop w n ""

hexloop :: Int -> Int -> String -> String
hexloop w n xs =
  if w==0
    then if n==0
           then xs
           else error "hex number too big"
    else let n' = n `div` 16
             d = hexdigit (n `mod` 16)
         in hexloop (w-1) n' (d:xs)

hexDigits = "0123456789abcdef"

hexdigit :: Int -> Char
hexdigit i =
  if 0<=i && i<16
    then hexDigits !! i
    else error "hex digit not between 0 and 15"

valHex4 :: String -> Either String Int
valHex4 [p,q,r,s] =
    case (valHexDigit p, valHexDigit q, valHexDigit r, valHexDigit s) of
      (Right p', Right q', Right r', Right s') ->
        Right (16^3 * p' + 16^2 * q' + 16^1 * r' + 16^0 * s')
      _ -> Left ("Bad 4-digit hex constant <" ++ [p,q,r,s] ++ ">")
valHex4 xs = Left ("Bad 4-digit hex constant <" ++ xs ++ ">")


hex4Word16 :: String -> Word16
hex4Word16 x =
  case valHex4 x of
    Right x -> fromIntegral x
    Left _ -> 0

test_valHex4 =
  do putStrLn dashes
     putStrLn "test_valHex4"
     putStrLn (showEither (valHex4 "0005"))
     putStrLn (showEither (valHex4 "0050"))
     putStrLn (showEither (valHex4 "0500"))
     putStrLn (showEither (valHex4 "5000"))
     putStrLn (showEither (valHex4 "002c"))
     putStrLn (showEither (valHex4 "12345"))
     putStrLn (showEither (valHex4 "123"))
     putStrLn (showEither (valHex4 "abcd"))
     putStrLn (showEither (valHex4 "abgd"))



showEither :: Show a => Either String a -> String
showEither (Left msg) = "Error: " ++ msg
showEither (Right x) = "OK: " ++ show x


valHexDigit :: Char -> Either String Word4
valHexDigit x =
  if      x=='0' then Right 0
  else if x=='1' then Right 1
  else if x=='2' then Right 2
  else if x=='3' then Right 3
  else if x=='4' then Right 4
  else if x=='5' then Right 5
  else if x=='6' then Right 6
  else if x=='7' then Right 7
  else if x=='8' then Right 8
  else if x=='9' then Right 8
  else if x=='a' then Right 10
  else if x=='A' then Right 10
  else if x=='b' then Right 11
  else if x=='B' then Right 11
  else if x=='c' then Right 12
  else if x=='C' then Right 12
  else if x=='d' then Right 13
  else if x=='D' then Right 13
  else if x=='e' then Right 14
  else if x=='E' then Right 14
  else if x=='f' then Right 15
  else if x=='F' then Right 15
  else Left ("Bad hex digit: " ++ [x])

                                   
hex4 :: (Integral a, FiniteBits a) => a -> Char
hex4 x =
  let i =  (fromIntegral x)
  in if i<16
       then "0123456789abcdef" !! i
       else error ("hex digit too large: " ++ show i)

-- CLEANUP hexd and showByte are older versions of hex4, hex8

-- Convert a hex digit represented as Int to the corresponding Char
hexd :: Int -> Char
hexd i
  | 0<=i && i<16 = "0123456789abcdef" !! i
  | otherwise = error "hex digit value not between 0 and 15"

-- Convert a byte, represented as an Int, to a string of hex digits
showByte :: Int -> String
showByte x
  | 0<=x && x<256 =
      let b = x `mod` 16
          a = x `div` 16
      in [hexd a, hexd b]
  | otherwise = error ("Byte value out of range " ++ show x)

hex8 :: Word8 -> String
hex8 x =
  let p = extract 0 3 x
      q = extract 4 7 x
  in [hex4 p, hex4 q]

hex16 :: Word16 -> String
hex16 x =
  let p = extract  0  3 x
      q = extract  4  7 x
      r = extract  8 11 x
      s = extract 12 15 x
  in [hex4 p, hex4 q, hex4 r, hex4 s]


-- CLEANUP older versions of hex32

showw :: Int -> String
showw x =
  let byted = x `mod` 16
      x1 = x `div` 16
      bytec = x1 `mod` 16
      x2 = x1 `div` 16
      byteb = x2 `mod` 16
      x3 = x2 `div` 16
      bytea = x3
  in [hexd bytea, hexd byteb, hexd bytec, hexd byted]

-- show i using an output string of width w
--showIntWid :: Int -> Int -> String
showIntWid i w =
  let xs = show i :: String
  in (take (w - length xs) (repeat ' ')) ++ xs

hex32 :: Word32 -> String
hex32 x =
  [hex4 (extract  0  3 x),
   hex4 (extract  4  7 x),
   hex4 (extract  8 11 x),
   hex4 (extract 12 15 x),
   ' ',
   hex4 (extract 16 19 x),
   hex4 (extract 20 23 x),
   hex4 (extract 24 27 x),
   hex4 (extract 28 31 x)]

bits4 :: (Integral a, FiniteBits a) => Char -> a
bits4 '0' = 0
bits4 '1' = 1
bits4 '2' = 2
bits4 '3' = 3
bits4 '4' = 4
bits4 '5' = 5
bits4 '6' = 6
bits4 '7' = 7
bits4 '8' = 8
bits4 '9' = 9
bits4 'a' = 10
bits4 'b' = 11
bits4 'c' = 12
bits4 'd' = 13
bits4 'e' = 14
bits4 'f' = 15
bits4 x = error ("Invalid hex digit: " ++ [x])


------------------------------------------------------------------------
-- Converting hex string to word
------------------------------------------------------------------------

type Hex4 = String
zero_hex4 :: Hex4
zero_hex4 = "0000"

bits8 :: String -> Word8
bits8 [a,b] =
  let x = bits4 a :: Word8
      y = bits4 b :: Word8
  in shiftL x 4 .|. y
bits8 _ = 0 -- cover invalid short input


bits16 :: String -> Word16
bits16 [a,b,c,d] =
  let p = bits4 a :: Word16
      q = bits4 b :: Word16
      r = bits4 c :: Word16
      s = bits4 d :: Word16
  in shiftL p 12 .|. shiftL q 8 .|. shiftL r 4 .|. s
bits16 _ = 0 -- cover invalid short input

bits32 :: String -> Word32
bits32 (a:b:c:d:' ':e:f:g:h:_) =
  let p = bits4 a :: Word32
      q = bits4 b :: Word32
      r = bits4 c :: Word32
      s = bits4 d :: Word32
      t = bits4 e :: Word32
      u = bits4 f :: Word32
      v = bits4 g :: Word32
      w = bits4 h :: Word32
  in shiftL p 28 .|. shiftL q 24 .|. shiftL r 20 .|. shiftL s 16 .|.
     shiftL t 12 .|. shiftL u  8 .|. shiftL v  4 .|. w
bits32 _ = 0 -- cover invalid inputs with too few characters


------------------------------------------------------------------------
-- Two's complement arithmetic


-- Interpret a a 16-bit word as a two's complement integer, and
-- represent the result as an Int.

tcval :: Word16 -> Int
tcval x =
  let y = (fromIntegral x) :: Int
  in if y >= 2^15
       then y - 2^16
       else y



-- add :: Word16 -> Word16 -> (Bool, Word16)
-- add x y =
--   let x' = fromIntegral x :: Int
--       y' = fromIntegral y :: Int


------------------------------------------------------------------------
-- Basic operations on 16-bit machine words
------------------------------------------------------------------------

-- Sigma16 uses 16 bit words.  The fundamental operations are defined
-- here.  It uses binary addition for calculating effective addresses,
-- and two's complement representation for integers.  Note: in the
-- Haskell Word types, such as Word16, the bits are numbered from 0,
-- where bit 0 is the least significant

-- Show the value of a 16-bit word in decimal, assuming it is a binary
-- representation

showWord16bin :: Word16 -> String
showWord16bin x =
  let y = (fromIntegral x) :: Word32
  in show y

unit_test_showWord16bin :: (Integral a, Show a) => a -> IO ()
unit_test_showWord16bin x =
  do let y = (fromIntegral x) :: Word16
     let z = showWord16bin y
     putStrLn ("Input " ++ show x ++ " shown as " ++ z)

run_unit_test_showWord16bin =
  do putStrLn "Unit test showWord16bin"
     unit_test_showWord16bin (50000 :: Integer)
     unit_test_showWord16bin (2^16 - 1 :: Integer)
     unit_test_showWord16bin (2^16 :: Integer)
     unit_test_showWord16bin (2^16 + 1 :: Integer)
     unit_test_showWord16bin (2^17 - 2 :: Integer)
     unit_test_showWord16bin (2^17 - 1 :: Integer)
     unit_test_showWord16bin (2^17 :: Integer)
     unit_test_showWord16bin (0 :: Integer)
     unit_test_showWord16bin ((-1) :: Integer)
     unit_test_showWord16bin ((-2) :: Integer)
     unit_test_showWord16bin ((-(2^15)) :: Integer)
     unit_test_showWord16bin ((-(2^15) + 1) :: Integer)
     unit_test_showWord16bin (3^50 :: Integer)

-- Show the value of a 16-bit word in decimal, assuming it is a two's
-- complement representation

showWord16tc :: Word16 -> String
showWord16tc x =
  let s = testBit x 15
      a = if s
            then complement x + 1
            else x
      b = show a
      c = if s then '-' : b else b
  in c

unit_test_showWord16tc :: (Integral a, Show a) => a -> IO ()
unit_test_showWord16tc x =
  do let y = (fromIntegral x) :: Word16
     let z = showWord16tc y
     putStrLn ("Input " ++ show x ++ " shown as " ++ z)

run_unit_test_showWord16tc =
  do putStrLn "Unit test showWord16tc"
     unit_test_showWord16tc (0 :: Integer)
     unit_test_showWord16tc (1 :: Integer)
     unit_test_showWord16tc (2 :: Integer)
     unit_test_showWord16tc (3 :: Integer)
     unit_test_showWord16tc ((-1) :: Integer)
     unit_test_showWord16tc ((-2) :: Integer)
     unit_test_showWord16tc ((-3) :: Integer)
     unit_test_showWord16tc (20000 :: Integer)
     unit_test_showWord16tc ((-20000) :: Integer)
     unit_test_showWord16tc (2^15 - 3 :: Integer)
     unit_test_showWord16tc (2^15 - 2 :: Integer)
     unit_test_showWord16tc (2^15 - 1 :: Integer)
     unit_test_showWord16tc (2^15 :: Integer)
     unit_test_showWord16tc (2^15 + 1 :: Integer)
     unit_test_showWord16tc (2^15 + 2 :: Integer)
     unit_test_showWord16tc ((-(2^15) - 2) :: Integer)
     unit_test_showWord16tc ((-(2^15) - 1) :: Integer)
     unit_test_showWord16tc ((-(2^15)) :: Integer)
     unit_test_showWord16tc ((-(2^15) + 1) :: Integer)
     unit_test_showWord16tc ((-(2^15) + 2) :: Integer)
     unit_test_showWord16tc ((-(2^15) + 3) :: Integer)


-- Give True iff any bit in a word is 1, give False if all bits are 0.

word16bool :: Word16 -> Bool
word16bool x = x /= 0

-- mask32: the leftmost 16 bits are 1, the rightmost 16 bits are 0
-- ie it looks like   111...1 000...0

mask32 =
  let x = 0 :: Word32
      y = (complement x) :: Word32
      z = (shiftL y 16) :: Word32
  in z


addWord16 :: Word16 -> Word16 -> Word16
addWord16 a b = a + b
{-
  let a' = (fromIntegral a) :: Integer
      b' = (fromIntegral b) :: Integer
      x = a' + b' :: Word32
      y = (fromIntegral x) :: Word16
  in y
-}


run_unit_test_addWord16 :: IO ()
run_unit_test_addWord16 =
  do my_print_header_line "Unit test addWord16"
     unit_test_addWord16 2 3
     unit_test_addWord16 (2^14) (2^14)
     unit_test_addWord16 (2^14 + 3) (2^14 + 5)

unit_test_addWord16 :: Int -> Int -> IO ()
unit_test_addWord16 x y =
  do let z = addWord16 (fromIntegral x) (fromIntegral y)
     putStrLn (show x ++ " + " ++ show y ++ " => " ++ show z)

subWord16 :: Word16 -> Word16 -> Word16
subWord16 a b = a - b

mulWord16 :: Word16 -> Word16 -> (Word16,Word16)
mulWord16 a b = (0, a * b) -- ??? finish this

divWord16 :: Word16 -> Word16 -> (Word16,Word16)
divWord16 a b = (a `div` b, a `mod` b)

cmpLTWord16, cmpEQWord16, cmpGTWord16 :: Word16 -> Word16 -> Word16
cmpLTWord16 a b = bool16 (tcval a < tcval b)
cmpEQWord16 a b = bool16 (a == b)
cmpGTWord16 a b = bool16 (tcval a > tcval b)

shiftl16, shiftr16 :: Word16 -> Word16 -> Word16
shiftl16 a b = shiftL a (fromIntegral b)
shiftr16 a b = shiftR a (fromIntegral b)

inv16 :: Word16 -> Word16
inv16 = complement

and16, or16, xor16 :: Word16 -> Word16 -> Word16
and16 = (.&.)
or16  = (.|.)
xor16 = xor

bool16 :: Bool -> Word16
bool16 False = 0
bool16 True = 1

shift :: Word16 -> Word16 -> Word16
shift a b = a -- finish ???

logic :: Word16 -> Word16 -> Word16
logic a b = a -- finish ???






------------------------------------------------------------------------



------------------------------------------------------------------------

-- Unit testing

dashes :: String
dashes = take 50 (repeat '-')
