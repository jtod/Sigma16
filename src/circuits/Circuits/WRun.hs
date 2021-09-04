-- Sigma16: WRun.hs simulates a register circuit
-- Copyright (C) 2021 John T. O'Donnell
-- email: john.t.odonnell9@gmail.com
-- License: GNU GPL Version 3 or later. See Sigma16/README.md, LICENSE.txt

-- This file is part of Sigma16.  Sigma16 is free software: you can
-- redistribute it and/or modify it under the terms of the GNU General
-- Public License as published by the Free Software Foundation, either
-- version 3 of the License, or (at your option) any later version.
-- Sigma16 is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.  You should have received
-- a copy of the GNU General Public License along with Sigma16.  If
-- not, see <https://www.gnu.org/licenses/>.

--------------------------------------------------------------------------------
-- See Installation section of the Sigma16 User Guide, and
-- Sigma16/src/circuits/README.txt

-- Usage
--   cd to src/circuits  -- must be in this directory
--   ghci                -- start ghci and initialize using .ghci
--   :main Simple/Add    -- run M1 circuit on examples/Core/Simple/Add.obj.txt
--   ^C                  -- stop and return to ghci prompt
--   :r                  -- reload, after you have changed a circuit source file
--   uparrow             -- repeat previous command
--   :q                  -- quit ghci, return to shell


--   :main batch Simple/Add
--   :main cli
--   :main cli inputdata.txt

--------------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad.State
import Control.Concurrent
import Data.IORef
import System.Environment
import System.FilePath
import System.IO
import System.IO.Unsafe
import System.Console.ANSI

import Sigma16.ReadObj
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational
import HDL.Hydra.Circuits.Register
-- import M1.Tools.M1driver

import Circuits.Reg1

--------------------------------------------------------------------------------
-- Main program
--------------------------------------------------------------------------------

main :: IO ()
main = do
  (op, operand) <- getCmd
  putStrLn ("operation = " ++ show op)
  case op of
    CmdOpBatch -> return ()
    CmdOpCLI -> runDriver operand
    CmdOpEmpty -> return ()
  

runDriver :: String -> IO ()
runDriver xs = do
  putStrLn ("running simulationDriver " ++ xs)
  execStateT simulationDriver initState
  return ()


myreg1 :: CBit a => a -> a -> (a,a)
myreg1 ld x = (r, q)
  where r = dff q
        q = or2 (and2 (inv ld) r) (and2 ld x)


--------------------------------------------------------------------------------
-- Simulation driver
--------------------------------------------------------------------------------

simulationDriver :: StateT SysState IO ()
simulationDriver = do
  liftIO $ putStrLn "simulationDriver starting"
-- Input ports
  in_ld <- inPortBit "ld"
  in_x <- inPortWord "x" 4

-- Input signals  
  let ld = inbsig in_ld
  let x = inwsig in_x
  
-- Circuit defines output signals
  let (r,q) = myreg ld x

-- Output ports  
  out_r <- outPortWord "r" r
  out_q <- outPortWord "q" q

-- Run simulation
  liftIO $ putStrLn "Enter command after prompt, h for help"
  commandLoop
  liftIO $ putStrLn "Simulation terminated"

