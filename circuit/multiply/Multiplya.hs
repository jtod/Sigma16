------------------------------------------------------------------------
-- Multiplier circuit
------------------------------------------------------------------------

{- Multiply.hs --- definition of a sequential functional unit that
multiples two binary integers, along with test data and simulation
driver. -}

module Multiply where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.StandardCircuits

{- The circuit is a functional unit, which uses a start control signal
to initiate a multiplication and produces a ready output signal to
indicate completion.

The multiplier circuit uses the sequential shift-and-add algorithm to
multiply two k-bit binary numbers, producing a 2k-bit product.  The
specification is general, taking a size parameter k::Int.  The start
control signal tells the multiplier to begin computing the product
x*y, and any other multiplication in progress (if any) is aborted.  In
order to make the simulation output more interesting, the multiplier
outputs its internal register and sum values as well as the ready
signal and the product. -}

------------------------------------------------------------------------

multiply :: Clocked a => Int
  -> a -> [a] -> [a] -> (a,[a],[a],[a],[a])

multiply k start x y = (ready,prod,rx,ry,s)
  where
    rx = wlatch k (mux1w start (shr rx) x)
    ry = wlatch (2*k)
             (mux1w start
                (shl ry)
                (fanout k zero ++ y))
    prod = wlatch (2*k)
             (mux1w start
                (mux1w (lsb rx) prod s)
                (fanout (2*k) zero))
    (c,s) = rippleAdd zero (bitslice2 ry prod)
    ready = or2 (inv (orw rx)) (inv (orw ry))
