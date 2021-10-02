module M1.ReadObj where

{- Example of object code; this is the result of assembling Add.obj.txt.
For an executable program, where linking isn't needed, the module and
relocate statements can be ignored.  Each statement type begins with a
distinct letter: m, d, r.

module   Add
data     f101,0008,f201,0009,0312,f302,000a,5000
data     0017,000e,0000
relocate 0001,0003,0006
-}

import Data.List
import Control.Monad.State
import Text.ParserCombinators.Parsec
-- import HDL.Hydra.Core.Lib
import System.Environment
import System.FilePath
import HDL.Hydra.Core.Lib

data ObjStmt
  = ObjModule String
  | ObjData [Int]
  | ObjRelocate [Int]
  | ObjErr String
  deriving Show

data St = St {sta :: Int, stb :: Bool}
  deriving (Show)

initSt = St {sta = 100, stb = True}

-- Collect the list of object code words.  This assumes no org statement.

getData :: [ObjStmt] -> [Int]
getData [] = []
getData (ObjModule _ : ys) = getData ys
getData (ObjData xs : ys) = xs ++ getData ys
getData (ObjRelocate _ : ys) = getData ys
getData (ObjErr _ : ys) = getData ys
        

-- Interface to simulation driver
{-
readObjectFile :: String -> IO [Int]
readObjectFile fname = do
      code <- getObject objFile
      return code
--   f <- getCmdOperand  ?????
--  let f = Just "Simple/Add"
--  case f of
--    Nothing -> return []
--    Just objFile -> do
-}
{-
getObject :: String -> IO [Int]
getObject fname = do
  liftIO $ putStrLn ("fname = " ++ show fname)
--  let fnameExt = splitPath (fname ++ ".obj.txt")
--  liftIO $ putStrLn ("fnameExt = " ++ show fnameExt)
--  let corePath = ["..", "..", "examples", "Core"]
--  let objParts = corePath ++ fnameExt
--  liftIO $ putStrLn ("objParts = " ++ show objParts)
--  let objpath = joinPath objParts
--  liftIO $ putStrLn ("objpath = " ++ objpath)
--  code <- readObject objpath
  code <- readObject fname
  putStrLn ("Object code = " ++ show code)
  return code
-}

--------------------------------------------------------------------------------
-- Main program
--------------------------------------------------------------------------------

readObjectCode :: String -> IO [Int]
readObjectCode fname = do
  obj <- liftIO $ readFile fname
--  liftIO $ putStrLn obj
  let objLines = lines obj
  ys <- mapM parseLine objLines
--  putStrLn ("ys = " ++ show ys)
  let code = getData ys
--  putStrLn ("code = " ++ show code)
  return code
  
{-
readObjecCode :: StateT St IO ()
readObjecCode = do
  liftIO $ putStrLn "ReadObjecCode is running"
  return ()
-}

--------------------------------------------------------------------------------
-- Object code parser
--------------------------------------------------------------------------------

parseLine :: String -> IO ObjStmt
parseLine xs =
  do
--    putStrLn ("parseLine: " ++ xs)
     case (parse objStmt "" xs) of
       Left err -> do putStr "error "
                      print err
                      return (ObjErr (show err))
       Right x -> do -- putStr "  success: "
--                     print x
                     return x

skipWhiteSpace :: Parser ()
skipWhiteSpace =
  do many whiteSpaceChar
     return ()

whiteSpaceChar :: Parser ()
whiteSpaceChar =
  do oneOf  [' ', '\t', '\r', '\n']
     return ()

hexDig :: Parser Char
hexDig = oneOf "0123456789abcdef"

hexVal :: Char -> Int
hexVal c =
  case elemIndex c "0123456789abcdef" of
    Just i -> i
    Nothing -> 0

hexWord :: Parser Int
hexWord =
  do p <- hexDig
     q <- hexDig
     r <- hexDig
     s <- hexDig
     let xs = map hexVal [p,q,r,s]
     let ks = [16^3, 16^2, 16^1, 16^0]
     return (sum [x*y | (x,y) <- zip xs ks])

hexBlock :: Parser [Int]
hexBlock = do
  xs <- sepBy hexWord (char ',')
  return xs

objStmt :: Parser ObjStmt
objStmt = moduleStmt <|> dataStmt <|> relocateStmt

moduleStmt :: Parser ObjStmt
moduleStmt = do
  Text.ParserCombinators.Parsec.string "module"
  skipWhiteSpace
  xs <- many letter
  return (ObjModule xs)
  
dataStmt :: Parser ObjStmt
dataStmt = do
  Text.ParserCombinators.Parsec.string "data"
  skipWhiteSpace
  xs <- hexBlock
  return (ObjData xs)

relocateStmt :: Parser ObjStmt
relocateStmt = do
  Text.ParserCombinators.Parsec.string "relocate"
  skipWhiteSpace
  xs <- hexBlock
  return (ObjRelocate xs)


------------------------------------------------------------------------
-- Unit testing
------------------------------------------------------------------------

testparse p s =
  do putStrLn ("Input: " ++ s)
     case (parse p "" s) of
       Left err -> do putStr "error "
                      print err
       Right x -> do putStr "  success: "
                     print x

  {-
  testparse hexWord "1234"
  testparse hexBlock "3ab7,1234,abcd"
  testparse hexBlock "3ab7,*?,abcd"
  testparse dataStmt "data 45ab,3cef,987a"
  testparse relocateStmt "relocate 45ab,3cef,987a"

  testparse objStmt "module Add"
  testparse objStmt "data 45ab,3cef,987a"
  testparse objStmt "relocate 45ab,3cef,987a"
--  runStateT readObjecCode initSt
-}

-- Control.Monad.mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]

-- deprecated

{-
hexval :: Char -> Int
hexval _ = 0
hex4word16 :: Int -> Int -> Int -> Int -> Word16
hex4word16 a b c d = undefined
  let a1 = shiftL (fromIntegral a) 12
      b1 = shiftL (fromIntegral b)  8
      c1 = shiftL (fromIntegral c)  4
      d1 =        (fromIntegral d)
  in a1 .|. b1 .|. c1 .|. d1

objLine :: Parser ObjStmt
objLine xs =
  case (parse f "" xs) of
    Left err -> ObjErr err
    Right s -> s
-}

-- type Word16 = Int

