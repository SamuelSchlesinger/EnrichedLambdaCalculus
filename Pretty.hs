module Pretty where

import AST
import Data.List

class Pretty a where
  pretty :: a -> String

instance Pretty Constant where
  pretty (CharC c) = [c]
  pretty (StringC s) = show s
  pretty (IntegerC i) = show i

instance Pretty Variable where
  pretty (Bound i) =  '!' : show i
  pretty (Free x) = x

instance Pretty Pattern where
  pretty (ConstPat p) = pretty p
  pretty (VarPat v) = pretty v
  pretty (DataPat c ps) = c ++ " " ++ (intercalate " " $ map pretty ps)

instance Pretty Exp where
  pretty (ConstExp c) = pretty c
  pretty (VarExp v) = pretty v
  pretty (App e x) = '(' : (pretty e ++ (' ' : pretty x)) ++ ")"
  pretty (Lam p e) = ('\\' : pretty p) ++ ('.' : ' ' : pretty e)
  pretty (Let p e x) = "let " ++ pretty p ++ " = " ++ pretty e ++ " in " ++ pretty x
  pretty (Letrec ps x) = "let { " ++ (intercalate ";\n      " $ (map (\(p, e) -> pretty p ++ " = " ++ pretty e)) ps)
                                  ++ "} in " ++ pretty x
  pretty (Case v ps) = "case " ++ pretty v ++ " of { " ++ (intercalate ";\n      " $ (map (\(p, e) ->  pretty p ++ " -> " ++ pretty e)) ps) ++ "}"
