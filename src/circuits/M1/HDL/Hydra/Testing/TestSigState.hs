module HDL.Hydra.Testing.TestSigState where

import Data.IORef
import Data.List
import Data.Maybe
import Data.Typeable
import Data.Data
import Data.Generics
import Generics.SYB

import HDL.Hydra.Core.Lib
import HDL.Hydra.Core.SigState

------------------------------------------------------------
-- Some helper functions for testing the SYB traversal
------------------------------------------------------------

{- need to fix node type

doNode
  :: Node Bool MDstring TagString
  -> IO (Node Bool MDstring TagString)
doNode n = do
  xs <- showNode n
  putStrLn ("doNode: " ++ xs)
  return n
-}


doInt :: Int -> IO Int
doInt n = do
  putStrLn ("doInt " ++ show n)
  return (n + 1000)

----------------------------------------------------------------------
-- test1: inports and logic gates, no feedback
----------------------------------------------------------------------

{-

test1 = do
  putStrLn "test1"

  let a = inport "a"
  (nodeA, tagsA) <- dfs [] a
--  putStrLn ("\nnodeA = " ++ showNode nodeA)
  putStrLn ("tagsA = " ++ showTags tagsA)

  let b = inport "b"
  (nodeB, tagsB) <- dfs tagsA b
--  putStrLn ("\nnodeB = " ++ showNode nodeB)
  putStrLn ("tagsB = " ++ showTags tagsB)

  let c = inv "c" a
  (nodeC, tagsC) <- dfs tagsB c
--  putStrLn ("\nnodeC = " ++ showNode nodeC)
  putStrLn ("tagsC = " ++ showTags tagsC)

  let d = and2 "d" b c
  (nodeD, tagsD) <- dfs tagsC d
--  putStrLn ("\nnodeD = " ++ showNode nodeD)
  putStrLn ("tagsD = " ++ showTags tagsD)

  let e = xor2 "e" b c
  (nodeE, tagsE) <- dfs tagsD e
--  putStrLn ("\nnodeE = " ++ showNode nodeE)
  putStrLn ("tagsE = " ++ showTags tagsE)

  putStrLn "finished"
-}

----------------------------------------------------------------------
-- test2: dff with no feedback
----------------------------------------------------------------------

{-

test2 = do
  putStrLn "test2"

  let a = inport "a"
  (nodeA, tagsA) <- dfs [] a
--  putStrLn ("\nnodeA = " ++ showNode nodeA)
  putStrLn ("tagsA = " ++ showTags tagsA)

  let b = dff False "b" a
  (nodeB, tagsB) <- dfs tagsA b
--  putStrLn ("\nnodeB = " ++ showNode nodeB)
  putStrLn ("tagsB = " ++ showTags tagsB)

  putStrLn "finished"
-}


----------------------------------------------------------------------
-- test3: feedback going through dff
----------------------------------------------------------------------

{-

circ3 :: MyBit -> MyBit
circ3 a = r
  where r = dff False "r" q
        q = xor2 "q" a r

test3 = do
  putStrLn "test3"

  let x = inport "x"
  let y = circ3 x
  
  (nodeY, tagsY) <- dfs [] y
--  putStrLn ("\nnodeY = " ++ showNode nodeY)
  putStrLn ("tagsY = " ++ showTags tagsY)

  putStrLn "finished"
-}


----------------------------------------------------------------------
-- test4: settle for inport and logic gate
----------------------------------------------------------------------

{-

test4 = do
  putStrLn "test4"

  let a = inport "a"
  (nodeA, tagsA) <- dfs [] a
--  putStrLn ("\nnodeA = " ++ showNode nodeA)
  putStrLn ("tagsA = " ++ showTags tagsA)

  let b = inport "b"
  (nodeB, tagsB) <- dfs tagsA b
--  putStrLn ("\nnodeB = " ++ showNode nodeB)
  putStrLn ("tagsB = " ++ showTags tagsB)
  
  let c = inv "c" a
  (nodeC, tagsC) <- dfs tagsB c
--  putStrLn ("\nnodeC = " ++ showNode nodeC)
  putStrLn ("tagsC = " ++ showTags tagsC)
  
  let d = xor2 "d" b c
  (nodeD, tagsD) <- dfs tagsC d
--  putStrLn ("\nnodeD = " ++ showNode nodeD)
  putStrLn ("tagsD = " ++ showTags tagsD)

  av <- settle nodeA
  putStrLn ("av = " ++ show av)
  bv <- settle nodeB
  putStrLn ("bv = " ++ show bv)
  cv <- settle nodeC
  putStrLn ("cv = " ++ show cv)
  dv <- settle nodeD
  putStrLn ("dv = " ++ show dv)

  putStrLn "finished"
-}


----------------------------------------------------------------------
-- test5: feedback going through dff, settle and advance
----------------------------------------------------------------------

{-

circ5 :: MyBit -> MyBit
circ5 a = r
  where r = dff False "r" q
        q = xor2 "q" a r

test5 = do
  putStrLn "test5"

  let x = inport "x"
  let y = circ3 x
  
  (nodeY, tagsY) <- dfs [] y
--  putStrLn ("\nnodeY = " ++ showNode nodeY)
  putStrLn ("tagsY = " ++ showTags tagsY)

  putStrLn "about to settle y0"
  y0 <- settle nodeY
  putStrLn "y0 settled"
  putStrLn ("y0 = " ++ show y0)

  putStrLn "about to advance y"
  advance nodeY
  putStrLn "y advanced"
  y1 <- settle nodeY
  putStrLn ("y1 = " ++ show y1)

  advance nodeY
  y2 <- settle nodeY
  putStrLn ("y2 = " ++ show y2)

  advance nodeY
  y3 <- settle nodeY
  putStrLn ("y3 = " ++ show y3)

  advance nodeY
  y4 <- settle nodeY
  putStrLn ("y4 = " ++ show y4)

  advance nodeY
  y5 <- settle nodeY
  putStrLn ("y5 = " ++ show y5)
  looper5 nodeY 0

  putStrLn "finished"

looper5 nodeY cycle
  | cycle >= 20 = return ()
  | otherwise = do
      y <- settle nodeY
      putStrLn ("cycle " ++ show cycle ++ ".  y = " ++ show y)
      advance nodeY
      looper5 nodeY (cycle+1)
-}

----------------------------------------------------------------------
-- test6: a chain of dff/logic/dff/logic without feedback
----------------------------------------------------------------------

{-

-- are all the internal dff found and handled by settle and advance?

circ6 :: MyBit -> MyBit
circ6 x = y
  where y = dff False "105" (dff False "104" (dff False "103"
              (dff False "102" (dff False "101"
                (dff False "100" x)))))

test6 = do
  putStrLn "test6"
  let x = inport "x"
  let y = circ6 x
  (nodeX, tagsX) <- dfs [] x
  (nodeY, tagsY) <- dfs tagsX y
--  putStrLn ("\nnodeX = " ++ showNode nodeX)
  putStrLn ("tagsX = " ++ showTags tagsX)
--  putStrLn ("\nnodeY = " ++ showNode nodeY)
  putStrLn ("tagsY = " ++ showTags tagsY)
  looper6 nodeX nodeY 0

looper6 nodeX nodeY cycle
  | cycle >= 40 = return ()
  | otherwise = do
      putStrLn ("\nCycle " ++ show cycle)
      x <- settle nodeX
      y <- settle nodeY
      putStrLn ("  x = " ++ show x ++ ",    y = " ++ show y)
      advance nodeY
      looper6 nodeX nodeY (cycle+1)
-}

----------------------------------------------------------------------
-- test 7: a chain with some feedback
----------------------------------------------------------------------

{-

circ7 :: MyBit -> MyBit
circ7 x = y
  where y = dff False "105" (inv "104" (dff False "103"
              (inv "102" (dff False "101" (xor2 "100" x y)))))

test7 = do
  putStrLn "test7"
  let x = inport "x"
  let y = circ7 x
  (nodeX, tagsX) <- dfs [] x
  (nodeY, tagsY) <- dfs tagsX y
--  putStrLn ("\nnodeX = " ++ showNode nodeX)
  putStrLn ("tagsX = " ++ showTags tagsX)
--  putStrLn ("\nnodeY = " ++ showNode nodeY)
  putStrLn ("tagsY = " ++ showTags tagsY)
  looper7 nodeX nodeY 0

looper7 nodeX nodeY cycle
  | cycle >= 40 = return ()
  | otherwise = do
      putStrLn ("\nCycle " ++ show cycle)
      x <- settle nodeX
      y <- settle nodeY
      putStrLn ("  x = " ++ show x ++ ",    y = " ++ show y)
      advance nodeY
      looper7 nodeX nodeY (cycle+1)

-}

----------------------------------------------------------------------
-- test8 is a register
----------------------------------------------------------------------

-- circ8 :: B -> B -> B
-- regCirc :: CBit a => a -> a -> a

{-
circ8 :: MyBit -> MyBit -> MyBit
circ8 ld x = r
  where r = dff False "dff"
             (or2 "or2"
                (and2 "and_r" (inv "inv_ld" ld) r)
                (and2 "and_x" ld x))
-}

{-
circ8a :: B -> B -> B
circ8a ld x = r
  where r = dffStateInitial False (mkMD "dff") (strTag "r")
             (or2
                (and2 (inv ld) r)
                (and2  ld x))

 -- It would be more general, though less convenient, to define dff so
--  it would take a general tag rather than a string; in this case a
--  tag of (TagString "r") rather than just "r".  But this is what
--  dfftag is for!  Maybe I should swap the names of dfftag and dff,
--  so the dff always requires a tag.

circ8b :: B -> B -> B
circ8b ld x = r
  where r = dfftag (strTag "r")
             (or2
                (and2 (inv ld) r)
                (and2  ld x))

test8 = do
  putStrLn "test8, reg1"
  let ld = inport (strTag "ld") :: B
  let x = inport (strTag "x")
  let r = circ8b ld x
  (nodeLd, tagsLd) <- dfs [] ld
  (nodeX, tagsX) <- dfs tagsLd x
  (nodeR, tagsR) <- dfs tagsX r
  xs <- showNode nodeLd
  putStrLn ("\nnodeLd = " ++ xs)
  putStrLn ("tagsLd = " ++ showTags tagsLd)
  xs <- showNode nodeX
  putStrLn ("\nnodeX = " ++ xs)
  putStrLn ("tagsX = " ++ showTags tagsX)
  xs <- showNode nodeR
  putStrLn ("\nnodeR = " ++ xs)
  putStrLn ("tagsR = " ++ showTags tagsR)
  looper8 nodeLd nodeX nodeR 0
  return ()


looper8 nodeLd nodeX nodeR cycle
  | cycle >= 40 = return ()
  | otherwise = do
      putStrLn ("\nCycle " ++ show cycle)
      ld <- settle nodeLd
      x <- settle nodeX
      r <- settle nodeR
      putStrLn ("   ld = " ++ showSig ld
                 ++ ",  x = " ++ showSig x
                 ++ ",  r = " ++ showSig r)
      advance nodeLd
      advance nodeX
      advance nodeR
      looper8 nodeLd nodeX nodeR (cycle+1)
-}

----------------------------------------------------------------------
-- test9 (testit2), syb traversal of a tuple
----------------------------------------------------------------------

-- Use syb to traverse a data structure containing some nodes.  From
-- testit4, it looks as if the IORefs will act as stopper, so I should
-- be able to use the ordinary everywhereM monadic traversal, and
-- don't need everywhereButM, which isn't defined by syb.

{-
testit2 :: IO ()
testit2 = do
  putStrLn "building a = inport"
  let a = inportState defaultReader (mkMD "inport_a") (mkTag "a")
            :: S
  (nodeA, tagsA) <- dfs [] a
  let nodeXX = nodeA :: N
  nodeAstr <- showNode nodeA
  putStrLn ("nodeA = " ++ nodeAstr)
  putStrLn ("tagsA = " ++ showTags tagsA)

  putStrLn "building b = inport"
  let b = inportState defaultReader (mkMD "inport_b") (mkTag "b")
            :: S
  (nodeB, tagsB) <- dfs tagsA b
  nodeBstr <- showNode nodeB
  putStrLn ("nodeB = " ++ nodeBstr)
  putStrLn ("tagsB = " ++ showTags tagsB)

  putStrLn "building c = inv a"
  let c = inv a
            :: S
  (nodeC, tagsC) <- dfs tagsB c
  nodeCstr <- showNode nodeC
  putStrLn ("nodeC = " ++ nodeCstr)
  putStrLn ("tagsC = " ++ showTags tagsC)

  putStrLn "building d = dff b"
  let d = dfftag (strTag "dff_d") b
            :: S
  (nodeD, tagsD) <- dfs tagsC d
  nodeDstr <- showNode nodeD
  putStrLn ("nodeD = " ++ nodeDstr)
  putStrLn ("tagsD = " ++ showTags tagsD)
  let components = (a,b,c,d)
  let nodes = ( -- nodeXX,
               2::Int, 3::Int,  nodeA, nodeB,
               4::Int, nodeC, nodeD)
        :: (Int, Int, Node Bool MDstring TagString,
             Node Bool MDstring TagString,
             Int,
             Node Bool MDstring TagString,
             Node Bool MDstring TagString)

--  let aa1 = everywhere (mkT noticeComponent) components
--  putStrLn (show aa1)
  
  putStrLn "starting syb traversal components"
  y <- everywhereM
         (mkM doNode)
         components
  putStrLn "starting syb traversal nodes looking for Ints"
  z <- everywhereM
         (mkM doInt)
         nodes
  putStrLn "starting syb traversal nodes looking for Nodes"
  z <- everywhereM
         (mkM doNode)
         nodes
  putStrLn "ending syb traversal"
  putStrLn "ending testit2 with Nodes"
  return ()
-}

----------------------------------------------------------------------
-- test10: circuit with tuples, manual lists of signals and nodes
----------------------------------------------------------------------

{-
myHalfAdd x y = (and2 x y, xor2 x y)

test10 :: IO ()
test10 = do
  putStrLn "test10: half adder, not using syb"
  let a = myInp "a"
  let b = myInp "b"
  let out@(c,s) = myHalfAdd a b

-- Here is the manual way to create nodes, one by one, but we won't
-- actually use these for the simulation...

  (nodeC,tagsC) <- dfs [] c
  (nodeS,tagsS) <- dfs tagsC s
  let nodesManual = [nodeC, nodeS]
  putStrLn "Check that the manually produced nodes"
  ys <- everywhereM (mkM doNode) (nodesManual :: [N])

-- Using the list traversal to create the nodes more easily, but again
-- these aren't actually used...

  let inputs = [a,b]
  let outputs = [c,s]
  (nodes,tags) <- myMkNodesList [] outputs
  putStrLn "Check the nodes produced by myMkNodes"
  zs <- everywhereM (mkM doNode) nodes

-- Use the format specification, which the user needs to write anyway
-- to get any output, so provide an easily traversable data structure
-- which can be used first to generate the nodes, and later to settle
-- and advance the signals

  let fmtSigs =
        [ FormatString "a =", FormatBit a
        , FormatString "b =", FormatBit b
        , FormatString "   =>  "
        , FormatString "c =", FormatBit c
        , FormatString "s =", FormatBit s
        ]
  (fmtNodes,tags) <- fmtMakeNodes fmtSigs
  looper10 0 fmtNodes
  putStrLn "test10 finished"
  return ()

looper10 cycle fmtNodes
  | cycle >= 10 = return ()
  | otherwise = do
      putStrLn ("\nCycle " ++ show cycle)
      settleFmt fmtNodes
      printFmt fmtNodes
      advanceFmt fmtNodes
      looper10 (cycle+1) fmtNodes
-}

--      rs <- mySettleList nodes  -- used with mkNodesList
--      putStrLn (show rs)  -- used with mkNodesList
--      myShowNodeList nodes  -- used with mkNodesList
--      myAdvanceList nodes  -- used with mkNodesList

----------------------------------------------------------------------
-- test11: inv4, no tags on the individual signals
----------------------------------------------------------------------

{-
myInv4 [x0,x1,x2,x3] = [inv x0, inv x1, inv x2, inv x3]

test11 :: IO ()
test11 = do
  putStrLn "test11: inv4 using Format for traversal"
  let x0 = myInp "x0"
  let x1 = myInp "x1"
  let x2 = myInp "x2"
  let x3 = myInp "x3"

  let out@[y0,y1,y2,y3] = myInv4 [x0,x1,x2,x3]

  let fmtSigs =
        [ FormatString "x = "
        , FormatBit x0, FormatBit x1, FormatBit x2, FormatBit x3
        , FormatString "   y = "
        , FormatBit y0, FormatBit y1, FormatBit y2, FormatBit y3
        ]
  (fmtNodes,tags) <- fmtMakeNodes fmtSigs
  looper11 0 fmtNodes
  putStrLn "test11 finished"

looper11 cycle fmtNodes
  | cycle >= 5 = return ()
  | otherwise = do
      putStrLn ("\nCycle " ++ show cycle)
      runFormat fmtNodes
      looper11 (cycle+1) fmtNodes
-}

----------------------------------------------------------------------
-- test12: reg4, tags on the individual signals
----------------------------------------------------------------------

myReg1 tag ld x = r
  where r = dff_ tag
              (or2 (and2 (inv ld) r)
                   (and2 ld x))
 
myReg4 tag ld [x0,x1,x2,x3] =
  [ myReg1 (t 0) ld x0
  , myReg1 (t 1) ld x1
  , myReg1 (t 2) ld x2
  , myReg1 (t 3) ld x3
  ]
  where t i = indexTag (stringToTag "myReg4") i

-- myInp :: String -> Component Bool
myInp xs = inport (stringToTag xs)

test12 :: IO ()
test12 = do
  putStrLn "test12: reg4, manually tagged, Format traversal"
  let ld = myInp "ld" :: Component Bool
  let x0 = myInp "x0"
  let x1 = myInp "x1"
  let x2 = myInp "x2"
  let x3 = myInp "x3"

  let out@[y0,y1,y2,y3] = myReg4 (stringToTag "rtag") ld [x0,x1,x2,x3]

  let fmtSigs =
        [ FormatString "x = "
        , FormatBit x0, FormatBit x1, FormatBit x2, FormatBit x3
        , FormatString "   y = "
        , FormatBit y0, FormatBit y1, FormatBit y2, FormatBit y3
        ]
  (fmtNodes,tags) <- fmtMakeNodes fmtSigs
  looper12 0 fmtNodes
  putStrLn "test11 finished"

looper12 cycle fmtNodes
  | cycle >= 5 = return ()
  | otherwise = do
      putStrLn ("\nCycle " ++ show cycle)
      runFormat fmtNodes
      looper12 (cycle+1) fmtNodes
