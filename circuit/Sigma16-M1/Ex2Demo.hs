------------------------------------------------------------------------
--  Ex2Demo: test program for CA4 Exercise 2
------------------------------------------------------------------------

{- A machine language program for the M1 architecture which
demonstrates the multiply and load with automatic increment
instructions.  This is similar to the FindMax program. -}

module Ex2Demo where
import RunM1

main :: IO ()
main = run_M1_program findmax 150

{- Register usage:
  R0 = constant 0
  R1 = maximum value found so far
  R2 = loop index i
  R3 = constant -1, indicates end of list
  R4 = x[i]
  R5 = constant 1, for incrementing i
  R6 = temp Bool value
-}

findmax :: [String]
findmax =
-- Machine Language  Addr    Assembly Language     Comment
-- ~~~~~~~~~~~~~~~~  ~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 ["1100", "0018", -- 0000 start load   R1,x[R0]     % max := x[0]
  "2200", "0001", -- 0002       ldval  R2,$0001     % i := 1
  "2300", "ffff", -- 0004       ldval  R3,$ffff     % R3 := -1
  "2500", "0001", -- 0006       ldval  R5,$0001     % R5 := 1 (for counter)
  "1420", "0018", -- 0008 loop  loadai R4,x[R2]     % R4 := x[i], i := i+1
  "a643",         -- 000a       cmpgt  R6,R4,R3     % R6 := (x[i] >= -1)
  "c600", "0014", -- 000b       jumpf  R6,done      % goto done if not
  "a641",         -- 000d       cmpgt  R6,R4,R1     % R6 := (x[i] > max)
  "c600", "0011", -- 000e       jumpf  R6,skip[R0]  % goto skip if not
  "3140",         -- 0010       add    R1,R4,R0     % max := x[i]
  "3225",         -- 0011 skip  add    R2,R2,R5     % i := i+1
  "d000", "0008", -- 0012       jump   loop[R0]     % goto loop
  "7100", "001e", -- 0014 done  store  R1,max[R0]   % save max
  "e000", "fffd", -- 0016       halt               % terminate execution
  "0002",         -- 0018 x     data   $0002        %   2
  "002a",         -- 0019       data   $002a        %  42
  "00e0",         -- 001a       data   $00e0        % 224
  "0013",         -- 001b       data   $0013        %  19
  "0004",         -- 001c       data   $0004        %   4
  "ffff",         -- 001d       data   $ffff        %  -1
  "0000"]         -- 001e max   data   $0000        %   0
