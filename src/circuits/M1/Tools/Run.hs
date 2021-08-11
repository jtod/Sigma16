-- Run a machine language program on the M1 circuit.  Run all commands
-- in the circuits directory

-- Compile and run
--   Compile:         $ ghc --make M1/Tools/Run
--   Run a program:   $ M1/Tools/Run M1/Programs/Add.obj.txt

-- Run without compilation (useful if you're making frequent changes
-- to any of the circuit files:)
--   $ runghc M1/Tools/Run M1/Programs/Add.obj.txt

-- See the Sigma16 User Guide for more information on the circuits
-- implementing Sigma16.  See the Hydra User Guide for documentation
-- of the hardware description language.

module Main where

import System.Environment
import Sigma16.ReadObj
import M1.Tools.M1driver

main :: IO ()
main = do
  putStrLn "M1 Run starting"
  let limit = 25
  args <- getArgs
  putStrLn ("args = " ++ show args)
  let fname = head args
  putStrLn ("fname = " ++ fname)
  code <- readObject fname
  putStrLn ("Object code = " ++ show code)
  run_Sigma16_executable code limit
  putStrLn "M1 Run finished"
