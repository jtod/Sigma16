------------------------------------------------------------------------
--  LeaLoadAddStore: machine language program for the M1 architecture
------------------------------------------------------------------------

module Main where
import M1driver

main :: IO ()
main = run_Sigma16_program prog 100

------------------------------------------------------------------------

prog :: [String]
prog =
-- Machine Language  Addr    Assembly Language     Comment
-- ~~~~~~~~~~~~~~~~  ~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 [                -- 0000 ; Test lea, load, and store instructions
                  -- 0000
  "f100", "0018", -- 0000    lea   R1,24[R0]
  "f201", "0008", -- 0002    load  R2,x[R0]
  "0312",         -- 0004    add   R3,R1,R2
  "f302", "0009", -- 0005    store R3,y[R0]
  "d000",         -- 0007    trap  R0,R0,R0
                  -- 0008 
  "0005",         -- 0008 x  data  5
  "0006"]         -- 0009 y  data  6
