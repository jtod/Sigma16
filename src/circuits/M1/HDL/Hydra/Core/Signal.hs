{-|

The Signal module defines several type classes for signals.  The
signal type determines how a circuit is executed; there are classes
for logic, for synchronous simulation using either streams or state
machines, and for structural analysis and netlist generation.  The
concrete signal representations and implementations are defined in
other modules.

-}

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
      DeriveDataTypeable, RankNTypes #-}

module HDL.Hydra.Core.Signal

  ( -- * Signals
    -- $signal-class
    Signal (..)
  , Sgl (..)
    -- * Clocking
    -- $clocking
  , Static (..)
  , StaticBit (..)
  , Logic (..)
  , Bit (..)
  , CBit (..)
  , TBit (..)

   -- * Describing and identifying signals
   -- $metadataTags
  , UnitTag
  , Tag
  , eqTag
  , showTag
  , stringToTag
  , addTag
  , indexTag
  , Metadata (..)
  , showMD
  ) where

import Data.Typeable
import Data.Data

----------------------------------------------------------------------
-- The Signal class
----------------------------------------------------------------------

-- $signal-class
--
-- A signal is an abstract wire that transmits information between
-- circuits.  It could be a bit, but could also be a word, a natural
-- number, or some other higher level object.  It's so abstract it has
-- no operations, but there are various specific kinds of circuit
-- defined as subclasses of Signal which provide operations.

class Signal a where

-- A singleton signal consists of one value, for example one bit.
-- Other signals may contain many values, for example a word.

class Sgl a where

----------------------------------------------------------------------
-- Logic signals
----------------------------------------------------------------------

-- A Logic signal supports the functions of Boolean algebra.  Constant
-- signals (zero, one) take no inputs but output a value.  Logic gates
-- take one or more inputs and produce an output.

class Signal a => Logic a where
  zero, one :: a
  buf, inv :: a -> a
  and2, nand2, or2, nor2, xor2, xnor2 :: a -> a -> a
  and3, nand3, or3, nor3, xor3, xnor3 :: a -> a -> a -> a
  and4, nand4, or4, nor4, xor4, xnor4 :: a -> a -> a -> a -> a

----------------------------------------------------------------------
-- Static signals
----------------------------------------------------------------------

-- A static signal has a value during one clock cycle; its value
-- doesn't change over time.  It is suitable for combinational logic
-- but not for sequential circuits.

class Signal a => Static a where
  initial :: a
  intSig  :: Int -> a
  sigInt  :: a -> Int
  readSig :: String -> a
  showSig :: a -> String

-- A StaticBit signal represents a Boolean value during one clock
-- cycle

class (Static a, Sgl a) => StaticBit a where
  is0, is1 :: a -> Bool
  boolSig :: Bool -> a
  sigBool :: a -> Bool
  showSigChar :: a -> Char
  readSigChar :: Char -> a

----------------------------------------------------------------------
-- Tags and metadata
----------------------------------------------------------------------

-- Signals may be decorated with two kinds of information: a tag that
-- identifies a particular signal uniquely, and metadata that gives
-- generic information about the signal.  Tags are used by the core
-- tools to traverse a circuit and generate a netlist.  Metadata is
-- not used by the core tools but may be used to make output more
-- readable, for example by indicating that a signal is produced by an
-- xor2 gate.

-- Tags are used to identify components uniquely.  A tag identifies a
-- specific instance of a component, not just the type of component.
-- Their primary purpose is to enable a traversal of a circuit to
-- determine whether a sequential component has already been
-- encountered, so the traversal can recognise feedback loops and
-- avoid going into an infinite loop.  Tags can also be useful for
-- labeling values in simulation outputs, and for traversing a circuit
-- graph in order to locate a signal of interest.

-- A Tag is represented as a list of basic UnitTags c.  This allows
-- each level of hierarchy in a circuit specification to provide part
-- of the tag: thus a register could have a tag "pc" and its
-- individual bits could have further tags 0, 1, 2...

data UnitTag
  = TagString String
  | TagInt Int
  deriving (Eq, Show, Typeable, Data)

type Tag = [UnitTag]

eqTag :: Tag -> Tag -> Bool
eqTag = (==)

showTag :: Tag -> String
showTag xs = show xs

-- A common case is to give a unique global identifier to a component,
-- without relying on information from surrounding black boxes in the
-- specification.  The strTag function converts a string to a tag.

stringToTag :: String -> Tag
stringToTag xs = [TagString xs]

-- A tag may be built up following the hierarch of the circuit,
-- with

addTag :: Tag -> UnitTag -> Tag
addTag xs t = t : xs

-- A bit index may be used to decorate an existing tag.  For example,
-- a register might have the tag "pc", and its individual bits would
-- have tags containing "pc" and the bit indices 0, 1, ...

indexTag :: Tag -> Int -> Tag
indexTag xs i = TagInt i : xs

-- Metadata gives information about the kind of component; e.g. it
-- could give a name like "and2" for an and2 gate.  This differs from
-- a tag, which identifies one specific instance of a component.  Many
-- components may have identical metadata, but no two distinct
-- components may have identical tags.

data Metadata
  = MDstring String
  deriving (Show, Typeable, Data)

showMD :: Metadata -> String
showMD m = show m

----------------------------------------------------------------------
-- Tagged signals
----------------------------------------------------------------------

-- Each logic gate has a corresponding version that takes a tag, which
-- should be used to give a unique identifier to the instance of the
-- gate.  These components have an underscore at the end of their
-- name; thus and2_ is the tagged version of and2. Tags are optional
-- for combinational components, but mandatory for flip flops.  They
-- are sometimes useful for combinational components because they can
-- be used to provide more readable simulation output, and they are
-- also useful for traversing a circuit to find a particular component
-- (e.g. placing a "logic probe" during simulation).

{-
class Sgl a => TBit a where
  dff_ :: Tag -> a -> a
-}

class Logic a => TBit a where
  buf_, inv_ :: Tag -> a -> a
  and2_, nand2_, or2_, nor2_, xor2_, xnor2_ :: Tag -> a -> a -> a
  and3_, nand3_, or3_, nor3_, xor3_, xnor3_ :: Tag -> a -> a -> a -> a
  and4_, nand4_, or4_, nor4_, xor4_, xnor4_
    :: Tag -> a -> a -> a -> a -> a


----------------------------------------------------------------------
-- Combinational Bit signals
----------------------------------------------------------------------

class (Logic a, Sgl a) => Bit a where

----------------------------------------------------------------------
-- Clocked signals
----------------------------------------------------------------------


-- A clocked signal has a value that varies over time, taking on
-- distinct static values on each clock cycle.  The variation is not
-- continuous; it occurs at points in time called clock ticks.
-- Clocked signals represent values in a synchronous circuit.  A
-- clocked bit signal can be used with flip flops as well as logic
-- gates.  The value during one cycle must be static, so you can't
-- take a clocked signal and clock it again.  Before the first clock
-- tick, a clocked circuit has an initial value, and delay transforms
-- the signal to the next clock cycle.

class Bit a => CBit a where
  dff :: a -> a


{-
class (Static s, Sgl a) => CBit a s | a -> s where
  dffinit_ :: Tag -> s -> a -> a
  dff_ :: Tag -> a -> a
-}



----------------------------------------------------------------------
-- Lattice signals
----------------------------------------------------------------------

-- A lattice signal supports CMOS.  It represents weak and strong
-- signals, as well as bottom (undefined) and top (inconsistent).
-- When two lattice signals are connected, the result is the least
-- upper bound.

class Lattice a where
  defined, shorted :: a -> Bool
  weak, strong  :: a -> Bool
  lub :: a -> a -> a



----------------------------------------------------------------------
{- redundant?

class (Static s, Sgl s) => Clocked a s | a -> s where
  dff :: s -> a -> a
class Tagged c => CTBit a t | a -> t  where
  dff_ :: t -> a -> a
-- There is also a flip flop that has additional tag and metadata;
-- this requires additional class constraints.

-- class CBit a where
--   dff :: a -> a

data TagString = TagString -- ?????????
= TagString2 String
  | TagInt2 Int
  | TagList --  Tag
instance Tagged a => Tagged (Tag a) where
  eqTag (Tag x) (Tag y) = eqTag x y
  stringToTag xs = Tag (stringToTag xs)
  showTag (Tag x) = showTag x
--  , TagString (TagString)
--  , mkTag
--  , indexTag
  stringToTag xs = [stringToTag xs]
indexTag :: Int -> [TagString] -> [TagString]
indexTag i xs = xs ++ [TagString ("_" ++ show i)]
-- mkTag :: String -> TagString
-- mkTag xs = TagString xs
-- data TagString = TagString String
--   deriving (Show, Typeable, Data)
--  stringToTag  :: String -> c
instance Tagged TagString where
  eqTag (TagString xs) (TagString ys) = (xs == ys)
  stringToTag xs = TagString xs
  showTag (TagString xs) = "<Tag " ++ xs ++ ">"
data Tag a = Tag a
  deriving (Typeable, Data)
instance Tagged a => Tagged (Tag a) where
  eqTag (Tag x) (Tag y) = eqTag x y
  stringToTag xs = Tag (stringToTag xs)
  showTag (Tag x) = showTag x
-}

