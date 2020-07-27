-- Sigma: AbstractSyntax.hs
-- Copyright (C) 2020 John T. O'Donnell
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

module AbstractSyntax where

data Var = V String
  deriving (Read, Show, Eq)

--------------------------------------------------------------------------------
-- Values
--------------------------------------------------------------------------------

-- Values are tagged to allow dynamic typing and runtime error detection

data Value = I Int | N Int | B Bool
  deriving (Read, Show, Eq)

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

-- Expression denoting integer value

data IExp
  = IExpCon Value
  | IExpVar Var
  | IExpUnop IUnop IExp
  | IExpBinop IBinop IExp IExp
  deriving (Read, Show, Eq)

data IUnop = INeg
  deriving (Read, Show, Eq)

data IBinop = IAdd | ISub | IMul | IDiv | IMod
  deriving (Read, Show, Eq)

-- Expression denoting natural number

data NExp
  = NExpCon Value
  | NExpVar Var
  | NExpUnop NUnop NExp
  | NExpBinop NBinop NExp NExp
  deriving (Read, Show, Eq)

data NUnop = NInv
  deriving (Read, Show, Eq)

data NBinop = NAdd | NSub | NMul | NDiv| NMod | NAnd | NOr | NXor
  deriving (Read, Show, Eq)

-- Expression denoting a Boolean

data BExp
  = BExpCon Value
  | BExpVar Var
  | BExpRelI RelOp IExp IExp
  | BExpRelN RelOp NExp NExp
  | BExpUnop BUnop BExp
  | BExpBinop BBinop BExp BExp
  deriving (Read, Show, Eq)

data RelOp = RelLT | RelLE | RelNE | RelEQ | RelGE | RelGT
  deriving (Read, Show, Eq)

data BUnop = BNot
  deriving (Read, Show, Eq)

data BBinop = BAnd | BOr | BXor
  deriving (Read, Show, Eq)
