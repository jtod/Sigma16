------------------------------------------------------------------------
--			Arithmetic/Logic Unit
------------------------------------------------------------------------

module ALU where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational


{- The ALU performs addition, subtraction, negation, increment, and
comparision for <, =, and >.  It receives four control signals
(a,b,c,d) that determine which function it will compute.  These are
shown in the following table, where don't-care values are left blank.

  |~~~~~~~~~~~~~~~~~~~~~~~~|
  | Function |  a,b   c,d  |
  |~~~~~~~~~~|~~~~~~~~~~~~~|
  |   x+y    |  0 0        |
  |   x-y    |  0 1        |
  |    -x    |  1 0        |
  |   x+1    |  1 1   0 0  |
  |   x<y    |  1 1   0 1  |
  |   x=y    |  1 1   1 0  |
  |   x>y    |  1 1   1 1  |
  ~~~~~~~~~~~~~~~~~~~~~~~~~~

The control algorithm defines all control signals, regardless of what
operation is being performed.  The signal values for the ALU are:

  alu_add = 0000
  alu_sub = 0100
  alu_neg = 1000
  alu_inc = 1100
  alu_lt  = 1101
  alu_eq  = 1110
  alu_gt  = 1111
-}

alu n (a,b,c,d) x y = (cout,z)
  where
    wzero = fanout n zero
    wone = boolword n one
    negating = xor2 a b
    comparing = and3 a b (or2 c d)
    x' = mux2w (a,b) x x wzero x
    y' = mux2w (a,b) y (winv y) (winv x) (mux1w (or2 c d) wone y)
    xy = bitslice2 x' y'
    (cout,sum) = rippleAdd negating xy
    (lt,eq,gt) = rippleCmp xy
    lt_tc = mux2 (xy!!0) lt zero one lt
    eq_tc = eq
    gt_tc = mux2 (xy!!0) gt one zero gt
    comp_bool = mux2 (c,d) zero lt_tc eq_tc gt_tc
    comp_word = boolword n comp_bool
    z = mux1w comparing sum comp_word

