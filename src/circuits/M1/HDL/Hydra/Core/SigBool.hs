module HDL.Hydra.Core.SigBool

  (
  ) where

-- Signal instance using Bool, suitable for combinational circuit
-- simulation and also used to help implement more complex signal
-- instances for clocked circuits.

import HDL.Hydra.Core.Signal

----------------------------------------------------------------------
-- Signal Bool
----------------------------------------------------------------------

-- A Bool is both a Signal and an Sgl (singleton); there are no
-- methods for these classes.

instance Signal Bool where

instance Sgl Bool where

{-
  bufT t a    = a
  inv_ t    = inv
  and2_ t   = and2
  nand2_ t  = nand2
  or2_ t    = or2
  nor2_ t   = nor2
  xor2_ t   = xor2
  xnor2_ t  = xnor2
  and3_ t   = and3
  nand3_ t  = nand3
  or3_ t    = or3
  nor3_ t   = nor3
  xor3_ t   = xor3
  xnor3_ t  = xnor3
  and4_ t   = and4
  nand4_ t  = nand4
  or4_ t    = or4
  nor4_ t   = nor4
  xor4_ t   = xor4
  xnor4_ t  = xnor4
-}  

----------------------------------------------------------------------
-- Bit Bool
----------------------------------------------------------------------

instance Logic Bool where
  zero = False
  one = True
  buf = id
  inv = not
  and2 = (&&)
  and3 a b c = a && b && c
  and4 a b c d = a && b && c && d
  or2 = (||)
  or3 a b c = a || b || c
  or4 a b c d  = a || b || c || d
  xor2 = xor2Bool
  xor3 a b c = xor2 a (xor2 b c)
  xor4 a b c d = xor2 (xor2 a b) (xor2 c d)
  xnor2 a b = not (xor2Bool a b)
  xnor3 a b c = not (xor3Bool a b c)
  xnor4 a b c d = not (xor4Bool a b c d)
  nand2 a b = not (a && b)
  nand3 a b c = not (and3 a b c)
  nand4 a b c d  = not (and4 a b c d)
  nor2 a b = not (a || b)
  nor3 a b c = not (or3 a b c)
  nor4 a b c d = not (or4 a b c d)


xor2Bool :: Bool -> Bool -> Bool
xor2Bool False x = x
xor2Bool True  x = not x

xor3Bool :: Bool -> Bool -> Bool -> Bool
xor3Bool a b c = xor2Bool a (xor2Bool b c)

xor4Bool :: Bool -> Bool -> Bool -> Bool -> Bool
xor4Bool a b c d = xor2Bool (xor2Bool a b) (xor2Bool c d)

----------------------------------------------------------------------
-- Static Bool
----------------------------------------------------------------------

instance Static Bool where
  initial = False
  intSig x
    | x==0      = False
    | otherwise = True
  sigInt x  =
    case x of
      False -> 0
      True  -> 1
  readSig = readBool
  showSig x = [showSigChar x]

instance StaticBit Bool where
  is0 = not
  is1 = id
  boolSig = id
  sigBool = id
  showSigChar x =
    case x of
      False -> '0'
      True  -> '1'
  readSigChar c
    | c=='0'    = False
    | otherwise = True

instance Bit Bool where


-- Should handle spaces, T/F, True/False, and possibly erroneous input

readBool :: String -> Bool
readBool ['0'] = False
readBool ['1'] = True
readBool _ = False

-- these should become redundant...

invStatic :: Bool -> Bool
invStatic = not

and2Static, or2Static, xor2Static :: Bool -> Bool -> Bool
and2Static = (&&)
or2Static = (||)
xor2Static a b = (a /= b)

{-
intSigBool :: Int -> Bool
intSigBool x
  | x==0 = False
  | otherwise = True


sigIntBool :: Bool -> Int
sigIntBool False = 0
sigIntBool True = 1

showSigBool False = '0'
showSigBool True  = '1'
-}
