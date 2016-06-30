-- A La "The Implementation of Functional Programming Languages"
-- by SLPJ

module AST where

data Exp
  = ConstExp Constant
  | VarExp Variable
  | DataExp Constructor
  | App Exp Exp
  | Lam Pattern Exp
  | Let Pattern Exp Exp
  | Letrec [(Pattern, Exp)] Exp
  | Case Variable [(Pattern, Exp)] deriving Show

data Pattern
  = ConstPat Constant
  | VarPat Variable
  | DataPat Constructor [Pattern] deriving Show

data Constant
  = NatC Integer deriving Show

data Variable
  = Bound Int
  | Free String deriving Show

type Constructor = String
