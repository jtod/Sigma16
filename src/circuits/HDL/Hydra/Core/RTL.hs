module HDL.Hydra.Core.RTL where

import Data.Word
import Data.Bits


------------------------------------------------------------------------
--		     Abstract Addressable Memory
------------------------------------------------------------------------

-- Represent memory as a binary tree of locations.  The entire address
-- space is 2^s locations.  Each location holds a polymorphic value.

data MemTree a
  = MemLocation a
  | MemNode (MemTree a) (MemTree a)
  deriving Show

initMemTree :: Integral a => a -> b -> MemTree b
initMemTree k x =
  if k==0
    then MemLocation x
    else let y = initMemTree (k-1) x
         in MemNode y y

memlist :: MemTree a -> [a]
memlist (MemLocation x) = [x]
memlist (MemNode x y) = memlist x ++ memlist y

memtest1 =
  let m = initMemTree 3 (0::Int)
      xs = memlist m
  in xs

memTreeFetch :: Integral a => a -> Word32 -> MemTree b -> b
memTreeFetch k addr mem =
  let f :: Integral a => a -> MemTree b -> b
      f j m =
        case m of
          MemLocation x -> x
          MemNode t0 t1 ->
             f (j-1) (if testBit addr (fromIntegral j) then t1 else t0)
  in f (k-1) mem

-- to handle an 8-bit address, use memTreeStore 8 addr mem x
-- f uses bits from j down to 0, address size  = j+1
-- indices are j, j-1, j-2, ..., 0

memTreeStore :: Integral a => a -> Word32 -> MemTree b -> b -> MemTree b
memTreeStore k addr mem x =
  let f j m =
        case m of
          MemLocation y ->
            if j == (-1)
              then MemLocation x
              else error "address size doesn't match memory size"
          MemNode t0 t1 ->
            case testBit addr j of
              False -> MemNode (f (j-1) t0) t1
              True ->  MemNode t0 (f (j-1) t1)
  in f (fromIntegral (k-1)) mem

memtest2 =
  let m = initMemTree 3 (0::Int)
      m1 = memTreeStore 3 5 m 500
      m2 = memTreeStore 3 2 m1 200
      m3 = memTreeStore 3 7 m2 700
  in (memlist m3,
      [(a, memTreeFetch 3 a m3) | a <- [0,1..7]])

{-
burstStore :: Word16 -> [a] -> Mem a -> (Word16, Mem a)
burstStore a [] m = (a,m)
burstStore a (x:xs) m =
  burstStore (a+1) xs (memStore 16 a x m)
-}

