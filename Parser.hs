-- A La "The Implementation of Functional Programming Languages"
-- by SLPJ

{-# LANGUAGE RankNTypes #-}

module Parser where

import AST
import Text.Parsec.Char
import Text.Parsec
import Data.Char
import Control.Monad.Identity

type Parser x = ParsecT String () Identity x

-- | Constants are simply natural numbers.
constant :: Parser Constant
constant = many1 digit >>= pure . NatC . \ns -> toInteger $ sum $ zipWith (*) [10 ^ n | n <- [0..]] $ map digitToInt $ reverse ns

-- | A variable is a sequence of lowercase letters
variable :: Parser Variable
variable = do { xs <- many1 lower;
                if xs `elem` ["in", "let", "letrec"] then fail "Nothing good here" else pure $ Free xs }

-- | A constructor begins with a capital letter and ends with lowercase letters
constructor :: Parser String
constructor = do { x <- upper; xs <- many lower; pure (x : xs) }

-- | In order to parse patterns, I must be able to parse variables, constants,
--   constructors, and nested patterns.
pattern :: Parser Pattern
pattern = (char '(' *> spaces *> pattern <* spaces <* char ')') <|>
          (constant >>= pure . ConstPat) <|> 
          (variable >>= pure . VarPat) <|>
          (do { c <- constructor; xs <- many (spaces *> pattern <* spaces) ; pure $ DataPat c xs })

-- | In order to parse expressions, we can reuse much of the above:
--   Constants (ConstExp)
--   Variables (VarExp)
--   Patterns (in Lam, Let, Case)
--   
--   We also must be able to parse Applications (App), Lambdas (Lam),
--   Let bindings (Let), Letrec bindings (Letrec), and Case statements.
expression :: Parser Exp
expression = lambda <|> 
             letp <|> 
             letrec <|> 
             casep <|> 
             (constant >>= pure . ConstExp) <|>
             -- ^ All of the things that can't be applied without being in parens
             do {
                  x <- (variable >>= pure . VarExp) <|> (constructor >>= pure . DataExp) <|>
                       (char '(' *> spaces *> expression <* spaces <* char ')');
                  (try (do { e <- spaces *> expression; pure (App x e);}) <|> pure x)
                }
                

lambda :: Parser Exp
lambda = do { char '\\';
              p <- spaces *> pattern <* spaces;
              char '.';
              e <- spaces *> expression;
              pure $ Lam p e }


letp :: Parser Exp
letp = do { string "let";
            p <- spaces *> pattern <* spaces;
            char '=';
            e <- spaces *> expression <* spaces;
            string "in";
            e <- spaces *> expression;
            pure $ Let p e e }

letrec_bindings :: Parser [(Pattern, Exp)]
letrec_bindings = do { p <- spaces *> pattern <* spaces;
                       char '=';
                       e <- spaces *> expression <* spaces;
                       ((string "in" *> pure [(p, e)]) <|> 
                       (do { newline; 
                             ps <- letrec_bindings; 
                             pure ((p, e) : ps); })) }

letrec :: Parser Exp
letrec = do { string "letrec";
              bs <- letrec_bindings;
              e <- spaces *> expression;
              pure $ Letrec bs e }


case_sendings :: Parser [(Pattern, Exp)]
case_sendings = do { p <- spaces *> pattern <* spaces;
                     string "->";
                     e <- spaces *> expression <* spaces;
                     (try $ do { newline;
                                 ps <- case_sendings;
                                 pure ((p, e) : ps); }) <|>
                     (pure $ pure (p, e)) }

casep :: Parser Exp
casep = do { string "case";
             v <- spaces *> variable <* spaces;
             string "of";
             s <- case_sendings;
             pure $ Case v s }
