---------------------------------------------------------------------------
--  add: assembly language program for M1 architecture
----------------------------------------------------------------------------

-- The program `progadd' does a few load and add instructions, to
-- provide a very small but complete program.

-- Runs with M1 system

module Circuit.Programs.AddRun where
import Circuit.M1.M1driver

main :: IO ()
main = run_Sigma16_program addprogram 15

addprogram :: [String]
addprogram =
-- Machine Language  Addr    Assembly Language     Comment
-- ~~~~~~~~~~~~~~~~  ~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 ["f101", "0006", -- 0000       load  R1,x[R0]     % R1 := x = 21
  "0211",         -- 0002       add   R2,R1,R1     % R2 := x + x
  "f202", "0006", -- 0003       store R2,x[R0]     % x := R2
  "d000",         -- 0005       halt               % terminate execution
  "0015"]         -- 0007 x     data  $0015        % 21
