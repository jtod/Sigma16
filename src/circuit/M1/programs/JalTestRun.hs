------------------------------------------------------------------------
--  JalTest
------------------------------------------------------------------------

-- A machine language program for the M1 architecture that illustrates
-- the jal instruction, implementing a minimal subroutine.

module Main where
import M1driver

main :: IO ()
main = run_Sigma16_program prog 150


------------------------------------------------------------------------
prog :: [String]
prog =
-- Machine Language  Addr    Assembly Language     Comment
-- ~~~~~~~~~~~~~~~~  ~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 [
                  -- 0000 ; Simple subroutine call and return
                  -- 0000
  "f100", "0003", -- 0000       lea   R1,3[R0]
  "fe06", "0007", -- 0002       jal   R14,subr[R0]
  "fe06", "0007", -- 0004       jal   R14,subr[R0]
  "d000",         -- 0006       trap  R0,R0,R0
  "0111",         -- 0007 subr  add  R1,R1,R1
  "f0e3", "0000"] -- 0008       jump 0[R14]
