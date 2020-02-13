{---------------------------------------------------------------------------
	  add: assembly language program for M1 architecture
----------------------------------------------------------------------------}

module Main where

{- The program `progadd' does a few load and add instructions, to
provide a very small but complete program. -}

import Hydra
import M1
import RunM1

main :: IO ()
main = run_prog add 15

add :: [String]
add =
-- Machine Language  Addr    Assembly Language     Comment
-- ~~~~~~~~~~~~~~~~  ~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 ["1100", "0007", -- 0000       load  R1,x[R0]     % R1 := x = 21
  "3211",         -- 0002       add   R2,R1,R1     % R2 := x + x
  "7100", "0007", -- 0003       store R2,x[R0]     % x := R2
  "e000", "fffd", -- 0005       halt               % terminate execution
  "0015"]         -- 0007 x     data  $0015        % 21
