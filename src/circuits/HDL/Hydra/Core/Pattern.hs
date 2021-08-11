{-|
Module       : HDL.Hydra.Circuits.Pattern
Description  : Combinators for regular circuit patterns
Copyright    : (c) John O'Donnell 2016
License      : GPL-3
Maintainer   : john.t.odonnell9@gmail.com
Stability    : experimental

Combinators for regular circuit patterns -}

module HDL.Hydra.Core.Pattern
  (

-- * Wiring patterns
   fanout2, fanout3, fanout4, fanout,
   fanoutbuf2, fanoutbuf3, fanoutbuf4,
   bitslice2, unbitslice2,

-- * Linear patterns

-- ** Map

   map2, map4, mapn,
   
-- ** Fold

-- ** Scan
   mscanr, mscan

-- * Tree patterns

  ) where

import HDL.Hydra.Core.Signal

-- Summary of patterns:

-- map2   :: (a->b->c) -> [a] -> [b] -> [c]
-- map4   :: (a->b->c->d->e) -> [a] -> [b] -> [c] -> [d] -> [e]
-- mapn   :: (a->b) -> Int -> [a] -> [b]

-- foldl  :: (a->b->a) -> a -> [b] -> a

-- foldr  :: (b->a->a) -> a -> [b] -> a
-- wscanr :: (b->a->a) -> a -> [b] -> [a]
-- ascanr :: (b->a->a) -> a -> [b] -> (a,[a])
-- mscanr :: (a->b->(b,c)) -> b -> [a] -> (b,[c])
-- mscan  :: (a->b->c->(b,a,d)) -> a -> b -> [c] -> (b,a,[d])


-- Bit Slice organization

bitslice2 :: [a] -> [a] -> [(a,a)]
bitslice2 = zip

unbitslice2 :: [(a,b)] -> ([a],[b])
unbitslice2 [] = ([],[])
unbitslice2 ((x,y):zs) =
  let (xs,ys) = unbitslice2 zs
  in (x:xs, y:ys)

zipn :: Int -> [a] -> [b] -> [(a,b)]
zipn n x y =
  [(x!!i,y!!i) | i <- [0..n-1]]

unzipn n xs =
  ([fst (xs!!i) | i <- [0..n-1]],
   [snd (xs!!i) | i <- [0..n-1]])



---------------------------------------------------------------------------
--			   Linear Patterns
---------------------------------------------------------------------------


-- Words
-- ~~~~~

-- Least/Most Significant Bit of a Word

lsb, msb :: [a] -> a
lsb w = w !! (length w -1)
msb w = w !! 0

--lsb x = x!!(k-1) where k = length x


-- Mapping
----------

map2 :: (a->b->c) -> [a] -> [b] -> [c]
map2 = zipWith

map4 f a b c d =
  if null a || null b || null c || null d
    then []
    else f (head a) (head b) (head c) (head d)
           : map4 f (tail a) (tail b) (tail c) (tail d)

mapn :: (a->b) -> Int -> [a] -> [b]
mapn f i xs
  | i==0  = []
  | otherwise = f (head xs) : mapn f (i-1) (tail xs)


-- Folding
-- ~~~~~~~

-- The folding patterns combine the elements of a word using a
-- building block f.  It corresponds to a linear computation from one
-- end of the word to the other, starting with an initial value a
-- (sometimes called an accumulator, but this is not to be confused
-- with accumulator registers!).  Types: a is the type of the
-- horizontal signal, which goes across the word from left to right,
-- and b is the type of the element of the word input.

xfoldl :: (a->b->a) -> a -> [b] -> a
xfoldl f a [] = a
xfoldl f a (x:xs) = xfoldl f (f a x) xs

xfoldr :: (b->a->a) -> a -> [b] -> a
xfoldr f a [] = a
xfoldr f a (x:xs) = f x (foldr f a xs)

-- Scanning
-- ~~~~~~~~

{- from TreeAdder
foldr :: (b->a->a) -> a -> [b] -> a
foldr f a [] = a
foldr f a (x:xs) = f x (foldr f a xs)

wscanr :: (b->a->a) -> a -> [b] -> [a]
wscanr f a xs =
  [foldr f a (drop (i+1) xs) | i <- [0 .. length xs -1]]
-}



wscanr :: (b->a->a) -> a -> [b] -> [a]
wscanr f a xs =
  [foldr f a (drop (i+1) xs) | i <- [0 .. length xs -1]]

ascanr :: (b->a->a) -> a -> [b] -> (a,[a])
ascanr f a [] = (a,[])
ascanr f a (x:xs) =
  let (a',xs') = ascanr f a xs
  in (f x a', a':xs')

{- from TreeAdder
ascanr ::  (b->a->a) -> a -> [b] -> (a,[a])
ascanr f a [] = (a,[])
ascanr f a (x:xs) =
  let (a',xs') = ascanr f a xs
      a'' = f x a'
  in (a'', a':xs')
-}


mscanr :: (a->b->(b,c)) -> b -> [a] -> (b,[c])
mscanr f a [] = (a,[])
mscanr f a (x:xs) =
  let (a',ys) = mscanr f a xs
      (a'',y) = f x a'
  in (a'',y:ys)

mscan :: (a->b->c->(b,a,d)) -> a -> b -> [c] -> (b,a,[d])
mscan f a b [] = (b,a,[])
mscan f a b (x:xs) =
  let (b'',a',y) = f a b' x
      (b',a'',ys) = mscan f a' b xs
  in (b'',a'',y:ys)



---------------------------------------------------------------------------
--			    Mesh Patterns
---------------------------------------------------------------------------



---------------------------------------------------------------------------
--			    Tree Patterns
---------------------------------------------------------------------------



----------------------------------------------------------------------
--			   Wiring patterns
----------------------------------------------------------------------


-- Unbuffered Fanout

fanout2 :: a -> (a,a)
fanout2 x = (x,x)

fanout3 :: a -> (a,a,a)
fanout3 x = (x,x,x)

fanout4 :: a -> (a,a,a,a)
fanout4 x = (x,x,x,x)

-- Duplicating a bit to form a word: fanout takes a wordsize k and a
-- signal x, and produces a word of size k each of whose bits takes
-- the value of x.

fanout :: Sgl a => Int -> a -> [a]
fanout k x = take k (repeat x)


-- Buffered Fanout

fanoutbuf2 :: (Logic a, Sgl a) => a -> (a,a)
fanoutbuf2 x = (y,y)
  where y = buf x

fanoutbuf3 :: (Logic a, Sgl a) => a -> (a,a,a)
fanoutbuf3 x = (y,y,y)
  where y = buf x

fanoutbuf4 :: (Logic a, Sgl a) => a -> (a,a,a,a)
fanoutbuf4 x = (y,y,y,y)
  where y = buf x
