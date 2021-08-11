module HDL.Hydra.Core.SigStream where

import HDL.Hydra.Core.Signal

-- A Stream of static values is a signal

instance (Signal a, Static a) => Signal (Stream a) where

-- A stream of bits represents a bit varying over time, so logic
-- operations can be performed on it

instance Sgl a => Sgl (Stream a) where

instance (Static a, Bit a) => Bit (Stream a) where

instance (Static a, Bit a) => CBit (Stream a) where
  dff x = Cycle zero x


instance (Static a, Sgl a, Logic a) => Logic (Stream a) where
  zero  = forever zero
  one   = forever one
  buf   = mapStream buf
  inv   = mapStream inv
  and2  = map2Stream and2
  and3  = map3Stream and3
  and4  = map4Stream and4
  or2   = map2Stream or2
  or3   = map3Stream or3
  or4   = map4Stream or4
  nand2 = map2Stream nand2
  nand3 = map3Stream nand3
  nand4 = map4Stream nand4
  nor2  = map2Stream nor2
  nor3  = map3Stream nor3
  nor4  = map4Stream nor4
  xor2  = map2Stream xor2
  xor3  = map3Stream xor3
  xor4  = map4Stream xor4
  xnor2 = map2Stream xnor2
  xnor3 = map3Stream xnor3
  xnor4 = map4Stream xnor4

-- A stream represents a clocked signal
  
{-
instance (Static a, Tagged c) => Clocked (Stream a) c where
  delay x = Cycle initial x
-}
-- instance (Bit a, Static a) => Clocked (Stream a) where
--   dff x = Cycle zero x

-- A dff component delays a bit signal
{-
instance (Signal a, Bit a, Static a) => CBit (Stream a)
  where dff = delay
-}
-- dff :: (Signal a, Bit a, Clocked a) => a -> a
-- dff x  = delay x

-- A list of Int can be converted to a stream of bits

intsSig :: (Sgl a, Static a) => [Int] -> Stream a
intsSig (x:xs) = Cycle (intSig x) (intsSig xs)

-- The most general way to show a stream is to use a simulation
-- driver, but the Show instance provides a quick method.

instance (Show a, Static a) => Show (Stream a) where
  show (Cycle x xs) = showSig x ++ "," ++ show xs




---------------------------------------------------------------------
-- Representation of Streams

data Stream a = Cycle a (Stream a)

listStream :: [a] -> Stream a
listStream [] = error "no more stream data from list"
listStream (x:xs) = Cycle x (listStream xs)

input_ints :: [Int] -> Stream Bool
input_ints = listStream . map f
  where f :: Int -> Bool
        f x = if x==0 then False else True

current :: Stream a -> a
current (Cycle x xs) = x

future :: Stream a -> Stream a
future (Cycle x xs) = xs

forever :: a -> Stream a
forever x = Cycle x (forever x)


---------------------------------------------------------------------
-- Operations on streams

mapStream :: (a->a) -> Stream a -> Stream a
mapStream f x
  = Cycle
      (f (current x))
      (mapStream f (future x))

map2Stream :: (a->b->c) -> Stream a -> Stream b -> Stream c
map2Stream f x y =
  Cycle
    (f (current x) (current y))
    (map2Stream f (future x) (future y))

map3Stream :: (a->b->c->d) -> Stream a -> Stream b -> Stream c -> Stream d
map3Stream f x y z =
  Cycle
    (f (current x) (current y) (current z))
    (map3Stream f (future x) (future y) (future z))

map4Stream ::
  (a->b->c->d->e) -> Stream a -> Stream b -> Stream c -> Stream d -> Stream e
map4Stream f w x y z =
  Cycle
    (f (current w) (current x) (current y) (current z))
    (map4Stream f (future w) (future x) (future y) (future z))
