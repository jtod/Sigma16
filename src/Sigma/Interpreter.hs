-- Sigma: Interpreter.hs
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

module Interpreter where
import AbstractSyntax

--------------------------------------------------------------------------------
-- Main program
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Sigma interpreter"
  
--------------------------------------------------------------------------------
-- Environment
--------------------------------------------------------------------------------

type Env = [Frame]
type Frame = [(Var,Value)]

slookup :: Env -> Var -> Value
slookup [] v = error (show v ++ " is undefined")
slookup (f:e) v = slookupFrame f e v

slookupFrame :: Frame -> Env -> Var -> Value
slookupFrame [] env v = slookup env v
slookupFrame ((v1,a) : xs) env v
  | v1 == v  = a
  | otherwise = slookupFrame xs env v

{-
sUpdate :: Env -> Var -> Value -> Env
sUpdate [] v x = error (show v ++ " is undefined")
sUpdate (f:e) v x = sUpdateFrame f e v x

sUpdateFrame :: Frame -> Env -> Var -> Value
sUpdateFrame [] env v x = sUpdate env v x
sUpdateFrame ((y,a) : xs) env v x
  | x == y  = a
  | otherwise = sUpdateFrame xs env v
-}

--------------------------------------------------------------------------------
-- Eval
--------------------------------------------------------------------------------

-- Int expressions

evalI :: IExp -> Env -> Value
evalI (IExpCon c) e = c
evalI (IExpVar v) e = slookup e v
evalI (IExpUnop op x) e = applyIUnop op (evalI x e)
evalI (IExpBinop op x y) e = applyIBinop op (evalI x e) (evalI y e)

applyIUnop :: IUnop -> Value -> Value
applyIUnop INeg (I x) = I (-x)
applyIUnop INeg _ = error "INeg requires Int"

applyIBinop :: IBinop -> Value -> Value -> Value
applyIBinop IAdd (I x) (I y) = I (x+y)
applyIBinop IAdd _ _ = error "IAdd requires Int arguments"
applyIBinop ISub (I x) (I y) = I (x-y)
applyIBinop ISub _ _ = error "ISub requires Int arguments"
applyIBinop IMul (I x) (I y) = I (x*y)
applyIBinop IMul _ _ = error "IMul requires Int arguments"
applyIBinop IDiv (I x) (I y) = I (x `div` y)
applyIBinop IDiv _ _ = error "IDiv requires Int arguments"
applyIBinop IMod (I x) (I y) = I (x `div` y)
applyIBinop IMod _ _ = error "IDiv requires Int arguments"

-- Nat expressions

evalN :: NExp -> Env -> Value
evalN (NExpCon c) e = c
evalN (NExpVar v) e = slookup e v
evalN (NExpUnop op x) e = applyNUnop op (evalN x e)
evalN (NExpBinop op x y) e = applyNBinop op (evalN x e) (evalN y e)

applyNUnop :: NUnop -> Value -> Value
applyNUnop NInv (I x) = I (-x) -- ????? operator
applyNUnop NInv _ = error "NNot requires Nat"

applyNBinop :: NBinop -> Value -> Value -> Value
applyNBinop NAdd (N x) (N y) = N (x+y)
applyNBinop NAdd _ _ = error "NAdd requires Nat arguments"
applyNBinop NSub (N x) (N y) = N (x-y) -- ????? operator
applyNBinop NSub _ _ = error "NSub requires Nat arguments"
applyNBinop NMul (N x) (N y) = N (x*y)
applyNBinop NMul _ _ = error "NMul requires Nat arguments"
applyNBinop NDiv (N x) (N y) = N (x `div` y)
applyNBinop NDiv _ _ = error "NDiv requires Nat arguments"
applyNBinop NMod (N x) (N y) = N (x `div` y)
applyNBinop NMod _ _ = error "NDiv requires Nat arguments"
applyNBinop NAnd (N x) (N y) = N (x `div` y) -- ????? operator
applyNBinop NAnd _ _ = error "NAnd requires Nat arguments"
applyNBinop NOr (N x) (N y) = N (x `div` y) -- ????? operator
applyNBinop NOr _ _ = error "NOr requires Nat arguments"
applyNBinop NXor (N x) (N y) = N (x `div` y) -- ????? operator
applyNBinop NXor _ _ = error "NXor requires Nat arguments"

-- Bool expressions

evalB :: BExp -> Env -> Value
evalB (BExpCon b) e = b
evalB (BExpVar v) e = slookup e v
evalB (BExpRelI op x y) e = applyIRelop op (evalI x e) (evalI y e)
evalB (BExpRelN op x y) e = applyNRelop op (evalN x e) (evalN y e)
evalB (BExpUnop op x) e = applyBUnop op (evalB x e)
evalB (BExpBinop op x y) e = applyBBinop op (evalB x e) (evalB y e)

applyBUnop :: BUnop -> Value -> Value
applyBUnop BNot (B x) = B (not x)
applyBUnop BNot _ = error "BNot requires Bool argument"

applyBBinop :: BBinop -> Value -> Value -> Value
applyBBinop BAnd (B x) (B y) = B (x && y)
applyBBinop BAnd _ _ = error "BAnd requires Bool arguments"
applyBBinop BOr (B x) (B y) = B (x || y)
applyBBinop BOr _ _ = error "BOr requires Bool arguments"
applyBBinop BXor (B x) (B y) = B (x || y) -- fix operator ??????????
applyBBinop BXor _ _ = error "BXor requires Bool arguments"

applyIRelop RelLT (I x) (I y) = B (x <  y)
applyIRelop RelLE (I x) (I y) = B (x <= y)
applyIRelop RelNE (I x) (I y) = B (x /= y)
applyIRelop RelEQ (I x) (I y) = B (x == y)
applyIRelop RelGE (I x) (I y) = B (x >= y)
applyIRelop RelGT (I x) (I y) = B (x >  y)

applyNRelop RelLT (N x) (N y) = B (x <  y)
applyNRelop RelLE (N x) (N y) = B (x <= y)
applyNRelop RelNE (N x) (N y) = B (x /= y)
applyNRelop RelEQ (N x) (N y) = B (x == y)
applyNRelop RelGE (N x) (N y) = B (x >= y)
applyNRelop RelGT (N x) (N y) = B (x >  y)

--------------------------------------------------------------------------------
-- Testing
--------------------------------------------------------------------------------

-- Environments

env1 :: Env
env1 = [ [(V "x", I 13), (V "y", N 45)],
         [(V "p", I 29), (V "q", N 30),
          (V "b", B True), (V "c", B False)]
       ]

-- Variable lookup

t1 = slookup env1 (V "y")  -- N 45
t2 = slookup env1 (V "b")  -- B True

-- Evaluation

tevalInt1 = -- x+p = 13+29 = I 42
  evalI (IExpBinop IAdd (IExpVar (V "x")) (IExpVar (V "p"))) env1

tevalNat1 = -- y+q = 45+30 = N 75
  evalN (NExpBinop NAdd (NExpVar (V "y")) (NExpVar (V "q"))) env1

trelInt1 = -- x<p = 13<29 = B True
  evalB (BExpRelI RelLT (IExpVar (V "x")) (IExpVar (V "p"))) env1
trelInt2 = -- x>=p = 13>=29 = B False
  evalB (BExpRelI RelGE (IExpVar (V "x")) (IExpVar (V "p"))) env1
  
