-- Sigma16: Multiply.hs
-- Copyright (C) 2020 John T. O'Donnell
-- email: john.t.odonnell9@gmail.com
-- License: GNU GPL Version 3 or later
-- See Sigma16/COPYRIGHT.txt, Sigma16/LICENSE.txt

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

-----------------------------------------------------------------------
-- Binary multiplier circuit
-----------------------------------------------------------------------

module Circuit.M1.Multiply where
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
