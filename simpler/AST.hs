module AST where

import Data.List

data Constant

data Exp
  = ConstExp Constant
  | VarExp Var
  | App Exp Exp
  | Lam String Exp

data Var = Bound Int | Free String

debruijn :: [String] -> Exp -> Exp
debruijn _ (ConstExp c) = ConstExp c
debruijn vs (VarExp (Free v)) = case elemIndex v vs of
  Just n -> VarExp $ Bound n
  Nothing -> VarExp $ Free v
debruijn vs (App e x) = App (debruijn vs e) (debruijn vs x)
debruijn vs (Lam v e) = debruijn (v : vs) e


