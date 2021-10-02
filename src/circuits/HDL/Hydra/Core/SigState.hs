{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
      DeriveDataTypeable, RankNTypes #-}

module HDL.Hydra.Core.SigState where

-- Representing signals as state machines

import Data.IORef
import Data.List
import Data.Maybe
import Data.Typeable
import Data.Data
import Data.Generics
-- import Generics.SYB

import HDL.Hydra.Core.Signal
import HDL.Hydra.Core.SigBool

-- Two level approach: (1) A circuit is a pure graph containing
-- Components that are connected by links.  (2) A graph traversal
-- builds corresponding Nodes with mutable fields, which are used for
-- the circuit simulation.

----------------------------------------------------------------------
-- Signal and component representation
----------------------------------------------------------------------

-- The data structures that represent circuits and signals are based
-- on type a, which is the type of the value of signal during a cycle,
-- e.g. Bool

data Component a = Component Metadata (Maybe Tag) (CmpSource a)
  deriving (Data, Typeable)

-- The immutable circuit graph of Components is defined as the
-- semantics of the circuit specification, using data recursion
-- (a.k.a. "knot tying").

-- The CmpSource is a source of signals produced by a component.  This
-- is a data structure that contains OutPorts; each OutPort points to
-- the component that produces it, identifies which port it is.

data OutPort a =
  OutPort (Component a) OutPortId

data OutPortId
  = OutPortIndex Int
  | OutPortName String
  deriving (Eq, Show, Data, Typeable)

data CmpSource a
  = Inport (Metadata -> Tag -> Int -> IO a)
  | Comb11 (a->a) (Component a)
  | Comb21 (a->a->a) (Component a) (Component a)
  | Comb31 (a->a->a->a) (Component a)
      (Component a) (Component a)
  | Comb41 (a->a->a->a->a) (Component a) (Component a)
      (Component a) (Component a)
  | Dff a (Component a)
  deriving (Data, Typeable)

-- The mutable graph of nodes is constructed by traversing the
-- immutable Component graph.

data Node a = Node
  { nodeComponent  :: Component a
  , nodeCycle      :: IORef Int
  , nodeSource     :: IORef (NodeSource a)
  , nodeValue      :: IORef (Maybe a)
  }
  deriving (Data, Typeable)

data NodeSource a
  = NodeDummy
  | NodeInport (Metadata -> Tag -> Int -> IO a)
  | NodeComb11 (a->a) (Node a)
  | NodeComb21 (a->a->a) (Node a) (Node a)
  | NodeComb31 (a->a->a->a) (Node a) (Node a) (Node a)
  | NodeComb41 (a->a->a->a->a) (Node a) (Node a)
      (Node a) (Node a)
  | NodeDff (LatchStatus a) (Node a)
  deriving (Data, Typeable)

data LatchStatus a
  = LatchEmpty
  | LatchPending
  | LatchFull a
  deriving (Data, Typeable)

----------------------------------------------------------------------
-- Components are instances of signals
----------------------------------------------------------------------

-- A signal is produced by a component.  A component is a primitive
-- device, such as a logic gate or flip flop.  It may have any number
-- of inputs but it produces just one output.  Each signal corresponds
-- to a component.  (Larger circuits that produce multiple outputs are
-- represented as black box circuits, and these may produce any number
-- of outputs.)

instance Signal (Component a) where
instance Sgl (Component a) where

-- type Sig a b c = Component a

-- The CBit class is for signals that are clocked, so they can support
-- a flip flop.  The type c is the tag.

-- instance (Sgl a, Static a) => CBit (Component a) where
--   dff_ tag x = delayState initial (MDstring "dff") tag x

{-
instance (Sgl a, Static a) => CBit (Component a) a where
  dff_ tag x = delayState initial (MDstring "dff") tag x
-}


{- ********************* working here
instance (Static a, Metadata b, Tagged c)
  => TBit (Component a b c) where
--  zero   = undefined   -- ??????????????
--  one    = undefined   -- ??????????????
  buf_    = undefined   --  mkComb11 inv  "inv"   -- error, fix it
  inv_    = mkComb11 inv   "inv"
  and2_   = mkComb21 and2  "and2"
  nand2_  = mkComb21 nand2 "nand2"
  or2_    = mkComb21 or2   "or2"
  nor2_   = mkComb21 nor2  "nor2"
  xor2_   = mkComb21 xor2  "xor2"
  xnor2_  = mkComb21 xnor2 "xnor2"
  and3_   = mkComb31 and3  "and3"
  nand3_  = mkComb31 nand3 "nand3"
  or3_    = mkComb31 or3   "or3"
  nor3_   = mkComb31 nor3  "nor3"
  xor3_   = mkComb31 xor3  "xor3"
  xnor3_  = mkComb31 xnor3 "xnor3"
  and4_   = mkComb41 and4  "and4"
  nand4_  = mkComb41 nand4 "nand4"
  or4_    = mkComb41 or4   "or4"
  nor4_   = mkComb41 nor4  "nor4"
  xor4_   = mkComb41 xor4  "xor4"
  xnor4_  = mkComb41 xnor4 "xnor4"
-}


-- instance Clocked (Component a b c) where
--   delay tag x = undefined   -- actually should delay it ??????????


----------------------------------------------------------------------
-- Default reader
----------------------------------------------------------------------

-- An IO operation called a reader is used to provide the value of an
-- inport on each clock cycle.  The user interface can provide a
-- specialised reader, but the default reader can also be used, and
-- provides an example of how to write a reader.

defaultReader :: StaticBit a => Metadata -> Tag -> Int -> IO a
defaultReader md tag cycle =
  do putStr ("Cycle " ++ show cycle ++ " "
             ++ showMD md ++ " (" ++ showTag tag
             ++ ").  Enter input: ")
     xs <- getLine
     let c:_ = xs
--     putStrLn ("default reader using character <" ++ [c] ++ ">")
     let v = readSigChar c
--     putStrLn ("default reader returning " ++ showSig v)
     return v

----------------------------------------------------------------------
-- Show the data structures
----------------------------------------------------------------------

showComponent :: Static a => Component a -> String
showComponent (Component md mt src) =
  let tagstr =
        case mt of
          Just tag -> showTag tag
          Nothing -> "untagged"
  in "[ " ++ showMD md ++ tagstr ++ "]"

showNode :: Static a => Node a -> IO String
showNode nd = do
  let component = nodeComponent nd
  cycle <- readIORef (nodeCycle nd)
  value <- readIORef (nodeValue nd)
  let valString = case value of
                    Nothing -> "?"
                    Just v -> showSig v
  return $
    "{Cycle " ++ show cycle ++ ". "
    ++ " (Value =  " ++ valString ++ ") "
    ++ showComponent component
    ++ "}"

----------------------------------------------------------------------
-- Tags and metadata
----------------------------------------------------------------------

-- The component builders detect feedback loops by comparing the tag
-- of a component with the tags belonging to its inputs.  In general,
-- tags are optional, but a tag is required to be present in any
-- feedback loop.  For a synchronous circuit, this is guaranteed by
-- requiring a tag on each flip flop.  Tags are optional on logic
-- gates.

type Tags a = [(Tag, Node a)]

showTags :: Tags a -> String
showTags xs =
  concat (intersperse "," (map (showTag . fst) xs))

-- When a new component is encountered, its tag is compared with the
-- tags that are already known.  If there is a match, the existing
-- component is reused; otherwise a new component is created.

findTag
  :: Tag                         -- tag of component
  -> [(Tag, Node a)]         -- known tags of inputs
  -> Maybe (Node a)        -- signal corresponding to tag

findTag tag [] = Nothing
findTag tag ((t,s) : xs)
  | eqTag tag t    = Just s
  | otherwise = findTag tag xs


-- use MDstring
-- mkMD :: String -> MDstring
-- mkMD xs = MDstring xs

----------------------------------------------------------------------
-- dfs: traverse the pure circuit graph and build mutable graph
----------------------------------------------------------------------

-- Use a depth first search to traverse the immutable graph of
-- components to build mutable graph of nodes that can be used for
-- simulation

dfs
  :: Tags a
  -> Component a
  -> IO (Node a, Tags a)

dfs tags cmp@(Component md mtag src) =
  case mtag of
    Nothing -> traceback tags cmp
    Just tag ->
      case findTag tag tags of
        Nothing -> traceback tags cmp
        Just s -> return (s, tags)

-- If the component doesn't correspond to a known tag, trace back
-- through its inputs to find the rest of the circuit

traceback
  :: Tags a
  -> Component a
  -> IO (Node a, Tags a)

traceback tags1 cmp@(Component md mtag src) = do
  cycleRef <- newIORef 0
  nodeSourceRef <- newIORef NodeDummy
  valueRef <- newIORef Nothing
  let self = Node
        { nodeComponent      = cmp
        , nodeCycle          = cycleRef
        , nodeSource         = nodeSourceRef
        , nodeValue          = valueRef
        }
  let tags2 =
        case mtag of
          Nothing -> tags1
          Just tag -> (tag,self) : tags1
  (nodeSrc, tagsFinal, mValue) <- case src of
    Inport reader -> return (NodeInport reader, tags2, Nothing)
    Comb11 f a -> do
      (aNode,tags3) <- dfs tags2 a
      return (NodeComb11 f aNode, tags3, Nothing)
    Comb21 f a b -> do
      (aNode,tags3) <- dfs tags2 a
      (bNode,tags4) <- dfs tags3 b
      return (NodeComb21 f aNode bNode, tags4, Nothing)
    Comb31 f a b c -> do
      (aNode,tags3) <- dfs tags2 a
      (bNode,tags4) <- dfs tags3 b
      (cNode,tags5) <- dfs tags4 c
      return (NodeComb31 f aNode bNode cNode, tags5, Nothing)
    Comb41 f a b c d -> do
      (aNode,tags3) <- dfs tags2 a
      (bNode,tags4) <- dfs tags3 b
      (cNode,tags5) <- dfs tags4 c
      (dNode,tags6) <- dfs tags5 d
      return (NodeComb41 f aNode bNode cNode dNode, tags6, Nothing)
    Dff initial a -> do
      (aNode,tags3) <- dfs tags2 a
      return (NodeDff LatchEmpty aNode, tags3, Just initial)
  writeIORef (nodeSource self) nodeSrc
  writeIORef (nodeValue self) mValue
  return (self, tagsFinal)

----------------------------------------------------------------------
-- settle: propagate signals through logic during a clock cycle
----------------------------------------------------------------------

-- When a signal is settled, it performs all computations that in
-- needs for the current clock cycle.  The computations depend on the
-- type of signal: input ports read their input, combinational
-- components settle their inputs and calculate the function; flip
-- flops load their latch into their state and settle their input to
-- refresh the latch.

-- The results of the computation are recorded, and if the same signal
-- is settled again the result is returned.  Settling is idempotent:
-- if the same signal is settled several times, the subsequent actions
-- will have no effect.

-- When an Inport is settled the first time, it will execute its
-- reader action to obtain the input value.  This may trigger
-- interaction with the user, so it's a good idea for the user
-- interface to settle all the input ports at the beginning of a
-- cycle, so as to control the order in which the inputs are
-- requested.  The main interface should settle every output signal.

settle :: Static a => Node a -> IO a

settle s = do
  nsrc <- readIORef (nodeSource s)
  case nsrc of
    NodeDummy -> error "NodeDummy"
    NodeInport reader -> do
      value <- readIORef (nodeValue s)
      case value of
        Just sv -> return sv
        Nothing -> do
          let (Component md mt cmpsrc) = nodeComponent s
          case mt of
            Nothing -> error "tag required for inport"
            Just tag -> do
              cycle <- readIORef (nodeCycle s)
              sv <- reader md tag cycle
              writeIORef (nodeValue s) (Just sv)
              return sv
    NodeComb11 f a -> do
      nval <- readIORef (nodeValue s)
      case nval of
        Just v -> return v
        Nothing -> do
          av <- settle a
          let sv = f av
          writeIORef (nodeValue s) (Just sv)
          return sv
    NodeComb21 f a b -> do
      nval <- readIORef (nodeValue s)
      case nval of
        Just v -> return v
        Nothing -> do
          av <- settle a
          bv <- settle b
          let sv = f av bv
          writeIORef (nodeValue s) (Just sv)
          return sv
    NodeComb31 f a b c -> do
      nval <- readIORef (nodeValue s)
      case nval of
        Just v -> return v
        Nothing -> do
          av <- settle a
          bv <- settle b
          cv <- settle c
          let sv = f av bv cv
          writeIORef (nodeValue s) (Just sv)
          return sv
    NodeComb41 f a b c d -> do
      nval <- readIORef (nodeValue s)
      case nval of
        Just v -> return v
        Nothing -> do
          av <- settle a
          bv <- settle b
          cv <- settle c
          dv <- settle d
          let sv = f av bv cv dv
          writeIORef (nodeValue s) (Just sv)
          return sv
    NodeDff mlatch a -> do
      case mlatch of
        LatchFull lv -> return ()
        LatchPending -> return ()
        LatchEmpty -> do
          writeIORef (nodeSource s) (NodeDff LatchPending a)
          lv <- settle a
          writeIORef (nodeSource s) (NodeDff (LatchFull lv) a)
          return ()
      value <- readIORef (nodeValue s)
      case value of
        Nothing -> error "assertion: dff has no value"
        Just sv -> return sv

----------------------------------------------------------------------
-- Advance to the next clock cycle: a clock tick
----------------------------------------------------------------------

advance :: Node a -> IO ()

advance s = do
  nodeSrc <- readIORef (nodeSource s)
  case nodeSrc of
    NodeDummy -> error "advance: dummy"
    NodeInport reader -> do
      mval <- readIORef (nodeValue s)
      case mval of
        Nothing -> return ()
        Just v -> do
          writeIORef (nodeValue s) Nothing
          modifyIORef (nodeCycle s) (1+)
          return ()
    NodeComb11 f a -> do
      mval <- readIORef (nodeValue s)
      case mval of
        Nothing -> return ()
        Just v -> do
          writeIORef (nodeValue s) Nothing
          modifyIORef (nodeCycle s) (1+)
          advance a
          return ()
    NodeComb21 f a b -> do
      mval <- readIORef (nodeValue s)
      case mval of
        Nothing -> return ()
        Just v -> do
          writeIORef (nodeValue s) Nothing
          modifyIORef (nodeCycle s) (1+)
          advance a
          advance b
          return ()
    NodeComb31 f a b c -> do
      mval <- readIORef (nodeValue s)
      case mval of
        Nothing -> return ()
        Just v -> do
          writeIORef (nodeValue s) Nothing
          modifyIORef (nodeCycle s) (1+)
          advance a
          advance b
          advance c
          return ()
    NodeComb41 f a b c d -> do
      mval <- readIORef (nodeValue s)
      case mval of
        Nothing -> return ()
        Just v -> do
          writeIORef (nodeValue s) Nothing
          modifyIORef (nodeCycle s) (1+)
          advance a
          advance b
          advance c
          advance d
          return ()
    NodeDff mlatch a -> do
      case mlatch of
        LatchEmpty -> return ()
        LatchPending -> return ()
        LatchFull v -> do
          writeIORef (nodeValue s) (Just v)
          writeIORef (nodeSource s) (NodeDff LatchEmpty a)
          modifyIORef (nodeCycle s) (1+)
          advance a
          return ()

----------------------------------------------------------------------
-- Component builders
----------------------------------------------------------------------

--------------------------------------------------
-- Logic gates
--------------------------------------------------

mkComb11 :: Static a
  => (a->a) -> String
  -> Component a -> Component a
mkComb11 f md a =
  Component (MDstring md) Nothing (Comb11 f a)

mkComb21 :: Static a
  => (a->a->a) -> String
  -> Component a -> Component a -> Component a
mkComb21 f md a b =
  Component (MDstring md) Nothing (Comb21 f a b)

mkComb31 :: Static a
  => (a->a->a->a) -> String
  -> Component a -> Component a -> Component a
  -> Component a
mkComb31 f md a b c =
  Component (MDstring md) Nothing (Comb31 f a b c)

mkComb41 :: Static a
  => (a->a->a->a->a) -> String
  -> Component a -> Component a -> Component a
  -> Component a -> Component a
mkComb41 f md a b c d =
  Component (MDstring md) Nothing (Comb41 f a b c d)


instance (Static a, Logic a) => Logic (Component a) where
  and2  = mkComb21 and2  "and2"
  nand2 = mkComb21 nand2 "nand2"
  or2   = mkComb21 or2   "or2"
  nor2  = mkComb21 nor2  "nor2"
  xor2  = mkComb21 xor2  "xor2"
  xnor2 = mkComb21 xnor2 "xnor2"

--------------------------------------------------
-- Build input ports
--------------------------------------------------


inportState :: Static a
  => (Metadata -> Tag -> Int -> IO a)
  -> Metadata -> Tag -> Component a
inportState reader md tag =
  Component md (Just tag) (Inport reader)

inport tag = inportState defaultReader (MDstring "inport") tag

{-
myInp :: String -> S
myInp xs = undefined --   inportState defaultReader (mkMD "inport") (mkTag xs)
-}

--------------------------------------------------
-- Build flip flops
--------------------------------------------------

delayState :: Static a
  => a -> Metadata -> Tag -> Component a -> Component a
delayState initial md tag a =
  Component md (Just tag) (Dff initial a)

----------------------------------------------------------------------
-- Components
----------------------------------------------------------------------

-- The SYB tools work for Component, a constructor, but not for a type
-- definition.

-- type B = Component Bool MDstring [UnitTag]
-- type N = Node Bool MDstring [UnitTag]
-- type StaticBit = Bool
-- type MyMD = MDstring
-- type MyTag = [UnitTag]

----------------------------------------------------------------------
-- Handling lists of signals and nodes
----------------------------------------------------------------------

-- Normally the Format approach would be better than just using lists

-- myMkNodes takes a list of signals and constructs a node that
-- represents each signal.  It returns the full set of tags, where
-- each tag is paired with the node it refers to, and the nodes that
-- correspond to the signals in the argument.  Other nodes deep within
-- the circuit are not returned, but they can be found by traversal.
-- The nodes in the result appear in the same order as the components
-- in the argument.  The computation is essentially a fold; the tags
-- need to be passed on to the next computation.

myMkNodesList
  :: Tags a
  -> [Component a]
  -> IO ([Node a], [(Tag, Node a)])
myMkNodesList ts [] = return ([],ts)
myMkNodesList ts (x:xs) = do
  (xnode,ts') <- dfs ts x
  (ns,ts'') <- myMkNodesList ts' xs
  return (xnode:ns, ts'')

{-
myMkNodesList
  :: Tags a
  -> [Node a]
  -> [Component a]
  -> IO ([(Tag, Node a)], [Node a])
myMkNodes ts ns [] = return (ts,ns)
myMkNodes ts ns (x:xs) = do
  (n,ts') <- dfs ts x
  myMkNodes ts' (n:ns) xs
-}

myShowNodeList
  :: Static a
  => [Node a] -> IO ()
myShowNodeList [] = return ()
myShowNodeList (x:xs) = do
  ys <- showNode x
  putStrLn ys
  myShowNodeList xs

mySettleList
  :: Static a
  => [Node a]
  -> IO [a]
mySettleList [] = return []
mySettleList (x:xs) = do
  y <- settle x
  ys <- mySettleList xs
  return (y:ys)

myAdvanceList :: [Node a] -> IO ()
myAdvanceList [] = return ()
myAdvanceList (x:xs) = do
  advance x
  myAdvanceList xs

----------------------------------------------------------------------
-- Format for SigState
----------------------------------------------------------------------

-- This should be unified with Format

data MyFormat a
  = FormatString String
  | FormatBit a

settleFmt :: Static a => [MyFormat (Node a)] -> IO ()
settleFmt [] = return ()
settleFmt (x:xs) = do
  case x of
    FormatString str -> return ()
    FormatBit n -> do
      y <- settle n
      return ()
  settleFmt xs

advanceFmt :: Static a => [MyFormat (Node a)] -> IO ()
advanceFmt [] = return ()
advanceFmt (x:xs) = do
  case x of
    FormatString str -> return ()
    FormatBit n -> do
      y <- advance n
      return ()
  advanceFmt xs

printFmt
  :: Static a
  => [MyFormat (Node a)] -> IO ()
printFmt [] = return ()
printFmt (x:xs) = do
  case x of
    FormatString str -> putStr str
    FormatBit n -> do
      ys <- showNode n
      putStrLn ys
  printFmt xs


fmtMakeNodes
  :: [MyFormat (Component a)]   -- signals to process
  -> IO ([MyFormat (Node a)],   -- resulting Format with nodes
         [(Tag, Node a)])       -- tags and corresponding nodes

fmtMakeNodes = fmtMakeNodesLooper [] []

fmtMakeNodesLooper
  :: Tags a                   -- tags so far
  -> [MyFormat (Node a)]        -- nodes created so far
  -> [MyFormat (Component a)]   -- remaining signals to process
  -> IO ([MyFormat (Node a)],   -- resulting Format with nodes
         [(Tag, Node a)])       -- tags and corresponding nodes

fmtMakeNodesLooper ts fmts [] = return (reverse fmts, ts)
fmtMakeNodesLooper ts fmts (x:xs) =
  case x of
    FormatString str ->
      fmtMakeNodesLooper ts (FormatString str : fmts) xs
    FormatBit b -> do
      (bnode,ts') <- dfs ts b
      fmtMakeNodesLooper ts' (FormatBit bnode : fmts) xs

-- The normal procedure is to settle the signals, print them, and
-- advance to the next cycle

runFormat
  :: Static a
  => [MyFormat (Node a)]
  -> IO ()

runFormat fs = do
  settleFmt fs
  printFmt fs
  advanceFmt fs

----------------------------------------------------------------------
-- Experiments with syb
----------------------------------------------------------------------

data Dummy1 a =
  Dummy Int (IORef Int) a (IORef a) (a->a)
    deriving (Typeable, Data)

data Dummy2 a = Dummy2
  { d2I :: Int
  , d2RI :: IORef Int
  , d2a  :: a
  , d2Ra :: IORef a
  , d2f :: a->a
  }
  deriving (Typeable, Data)

{-
data Node2 a b c = Node2
  { asdfas :: Int -- nodeComponent  :: Component a b c
  , xnodeCycle      :: IORef Int
--  , nodeSource     :: IORef (NodeSource a b c)
--  , nodeValue      :: IORef (Maybe a)
  }
  deriving (Data, Typeable)

data NodeSrc2 a b c
  = NodeDum2
  | NodeInport2  (b -> c -> Int -> IO a)
  | NodeComb112 (a->a) (Node2 a b c)
  | NodeDff2 (LatchStatus a) (Node2 a b c)
  deriving (Data, Typeable)
-}
