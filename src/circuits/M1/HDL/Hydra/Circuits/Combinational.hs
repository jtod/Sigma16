{-|
Module       : HDL.Hydra.Circuits.Combinational
Description  : Standard combinational circuits
Copyright    : (c) John O'Donnell 2019
License      : GPL-3
Maintainer   : john.t.odonnell9@gmail.com
Stability    : experimental

Specifications of a collection of standard small combinational
building block circuits.  -}

module HDL.Hydra.Circuits.Combinational
  (

-- * Generating constant signals
   boolword,

-- * Logic on words
   invw, andw, orw,
   winv,  -- alternative name for invw (deprecated)

-- * Addressing

-- ** Multiplexers
   mux1, mux2, mux3,
   mux22,
   mux1w, mux2w,

-- ** Demultiplexers
   demux1, demux2,
   demux1w, demux2w, demux3w, demux4w,

-- * Shifting
   shr, shl,
   
-- * Arithmetic
   halfAdd, bsum, bcarry, fullAdd,
   rippleAdd,
   addSub,
   rippleCmp
  
  ) where


import HDL.Hydra.Core.Signal
import HDL.Hydra.Core.Pattern


----------------------------------------------------------------------
--			    Logic on words
----------------------------------------------------------------------

-- |Word inverter: invw takes a word and inverts each of its bits.

invw :: Bit a => [a] -> [a]
invw = map inv

-- |(Deprecated) same as invw; winv will be removed in future
--  versions.

winv :: Bit a => [a] -> [a]
winv = map inv



-- And/Or over a word: Determine whether there exists a 1 in a word,
-- or whether all the bits are 0.  A tree fold can do this in log
-- time, but for simplicity this is just a linear time fold.

-- |Logical and of all the bits in a word.  The result if 1 if and
-- only iff every bit in the word is 1.

andw :: Bit a => [a] -> a
andw = foldl and2 one

orw :: Bit a => [a] -> a
orw = foldl or2 zero

-- Building a constant integer word

-- Representing a boolean bit as a word: boolword takes a bit x, and
-- pads it to the left with 0s to form a word.  If the input x is
-- False (0), the result is the integer 0 (i.e. n 0-bits), and if x is
-- True (1) the result is the integer 1 (rightmost bit is 1, all
-- others are 0).

boolword :: Bit a => Int -> a -> [a]
boolword n x = fanout (n-1) zero ++ [x]


---------------------------------------------------------------------
--			     Conditionals
---------------------------------------------------------------------

-- Multiplexers

mux1 :: Bit a => a -> a -> a -> a
mux1 p a b = x
  where x = or2 (and2 (inv p) a) (and2 p b)


mux2 :: Bit a => (a,a) -> a -> a -> a -> a -> a
mux2 (c,d) p q r s =
  mux1 c  (mux1 d p q)
          (mux1 d r s)

mux3
  :: Bit a
  => (a,a,a)
  -> a -> a -> a -> a -> a-> a -> a -> a -> a

mux3 (c0,c1,c2) a0 a1 a2 a3 a4 a5 a6 a7 =
  mux1 c0
    (mux1 c1
      (mux1 c2 a0 a1)
      (mux1 c2 a2 a3))
    (mux1 c1
      (mux1 c2 a4 a5)
      (mux1 c2 a6 a7))

mux22 :: Bit a => (a,a) -> (a,a) -> (a,a) -> (a,a) -> (a,a) -> (a,a)
mux22 (p0,p1) (a0,a1) (b0,b1) (c0,c1) (d0,d1) = (x,y)
  where x = mux2 (p0,p1) a0 b0 c0 d0
        y = mux2 (p0,p1) a1 b1 c1 d1

mux1w :: Bit a => a -> [a] -> [a] -> [a]
mux1w c x y = map2 (mux1 c) x y

mux2w cc = map4 (mux2 cc)


-- Demultiplexers

demux1 :: Bit a => a -> a -> (a,a)
demux1 c x = (and2 (inv c) x, and2 c x)

demux2 :: Bit a => (a,a) -> a -> (a,a,a,a)
demux2 (c0,c1) x = (y0,y1,y2,y3)
  where  (p,q) = demux1 c0 x
         (y0,y1) = demux1 c1 p
         (y2,y3) = demux1 c1 q

demux1w :: Bit a => [a] -> a -> [a]
demux1w [c0] x =
  let (a0,a1) = demux1 c0 x
  in [a0,a1]

demux2w :: Bit a => [a] -> a -> [a]
demux2w [c0,c1] x =
  let (a0,a1) = demux1 c0 x
      w0 = demux1w [c1] a0
      w1 = demux1w [c1] a1
  in w0++w1

demux3w :: Bit a => [a] -> a -> [a]
demux3w [c0,c1,c2] x =
  let (a0,a1) = demux1 c0 x
      w0 = demux2w [c1,c2] a0
      w1 = demux2w [c1,c2] a1
  in w0++w1

demux4w :: Bit a => [a] -> a -> [a]
demux4w [c0,c1,c2,c3] x =
  let (a0,a1) = demux1 c0 x
      w0 = demux3w [c1,c2,c3] a0
      w1 = demux3w [c1,c2,c3] a1
  in w0++w1


----------------------------------------------------------------------
--			      Arithmetic
----------------------------------------------------------------------

-- Combinational shifting

-- Shift a word to the right (shr) or to the left (shl).  In both
-- cases, this is just a wiring pattern.  A 0 is brought in on one
-- side, and the bit on the other side is just thrown away.

shr x = zero : [x!!i | i <- [0..k-2]]
  where k = length x
shl x = [x!!i | i <- [1..k-1]] ++ [zero]
  where k = length x


-- Bit addition

-- A half adder adds two bits x and y, and produces a 2-bit result
-- (carry,sum).  The value of x+y can be any of 0, 1, or 2, and this
-- is the binary value of the 2-bit result.

halfAdd :: Bit a => a -> a -> (a,a)
halfAdd x y = (and2 x y, xor2 x y)

-- A full adder adds three bits x, y, and c, and produces a 2-bit
-- result (carry,sum).  This is a building block for circuits that add
-- words, where a carry input c must be added to the x and y bits.
-- The value of x+y+c can be any of 0, 1, 2, or 3, and this is the
-- binary value of the 2-bit result.  It is convenient to group the x
-- and y bits into a pair, and to treat the carry input c as a
-- separate input; this makes it easier to use fullAdd with the mscanr
-- combinator.

fullAdd :: Bit a => (a,a) -> a -> (a,a)
fullAdd (x,y) c = (bcarry (x,y) c, bsum (x,y) c)

-- The carry and sum results from adding x+y+c are calculated by bsum
-- and bcarry.  For most purposes it would be more concise just to
-- define the fullAdd circuit directly, but for fast adders it is
-- helpful to organize the full adder using separate bcarry and bsum
-- subcircuits.  The bit sum is a parity calculation, while the carry
-- is a majority.

bsum, bcarry :: Bit a => (a,a) -> a -> a
bsum (x,y) c = xor3 x y c
bcarry (x,y) c = or3 (and2 x y) (and2 x c) (and2 y c)


-- Ripple carry addition

rippleAdd :: Bit a => a -> [(a,a)] -> (a,[a])
rippleAdd = mscanr fullAdd


-- Addition and subtraction

addSub :: Bit a => a -> [(a,a)] -> (a,[a])
addSub sub xy = rippleAdd sub (map f xy)
  where f (x,y) = (x, xor2 sub y)


-- Binary comparison

-- Ripple comparison

rippleCmp :: Bit a => [(a,a)] -> (a,a,a)
rippleCmp = foldl cmp1 (zero,one,zero)

-- Building block for comparing bits

cmp1 :: Bit a => (a,a,a) -> (a,a) -> (a,a,a)
cmp1 (lt,eq,gt) (x,y) =
  (or2 lt (and3 eq (inv x) y),
   and2 eq (inv (xor2 x y)),
   or2 gt (and3 eq x (inv y))
  )


