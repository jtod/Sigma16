----------------------------------------------------------------------
-- Binary multiplier circuit
----------------------------------------------------------------------

module Multiply where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational
import HDL.Hydra.Circuits.Register

{- Definition of a sequential functional unit that multiples two
binary integers, along with test data and simulation driver.

The circuit is a functional unit, which uses a start control signal to
initiate a multiplication and produces a ready output signal to
indicate completion.

The multiplier circuit uses the sequential shift-and-add algorithm to
multiply two k-bit binary numbers, producing a 2k-bit product.  The
specification is general, taking a size parameter k::Int.  The start
control signal tells the multiplier to begin computing the product
x*y, and any other multiplication in progress (if any) is aborted.  In
order to make the simulation output more interesting, the multiplier
outputs its internal register and sum values as well as the ready
signal and the product. -}

multiply
  :: CBit a               -- synchronous circuit
  => Int                  -- k = input word size; output is 2*k
  -> a                    -- start = control input
  -> [a]                  -- x = k-bit word
  -> [a]                  -- y = k-bit word
  -> (a,[a],[a],[a],[a])  -- (ready, product, ...internalsignals...)

multiply k start x y = (ready,prod,rx,ry,s)
  where
    rx = latch k (mux1w start (shr rx) x)
    ry = latch (2*k)
             (mux1w start
                (shl ry)
                (fanout k zero ++ y))
    prod = latch (2*k)
             (mux1w start
                (mux1w (lsb rx) prod s)
                (fanout (2*k) zero))
    (c,s) = rippleAdd zero (bitslice2 ry prod)
    ready = or2 (inv (orw rx)) (inv (orw ry))
