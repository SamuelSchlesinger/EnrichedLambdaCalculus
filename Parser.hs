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

schar :: Parser Char
schar = (char '\\' *> anyChar >>= \c -> case c of
           '\\' -> pure '\\'
           'n' -> pure '\n'
           't' -> pure '\t'
           'r' -> pure '\r'
           '"' -> pure '"'
           'x' -> do { a <- hexDigit; b <- hexDigit; pure $ intToDigit ((16 * digitToInt a) + digitToInt b) } ) <|>
        (satisfy (\x -> x /= '"'))

constant :: Parser Constant
constant = (char '"' *> (many (satisfy (\x -> x /= '"')) <* char '"') >>= \str -> pure $ StringC str) <|>
           (char '\'' *> (schar >>= \c -> pure $ CharC c) <* char '\'') <|>
           (do { neg <- (char '-' *> pure True) <|> pure False;
                n <- (many1 digit >>= pure 
                                . \ns -> (toInteger 
                                . sum  
                                . zipWith (*) [10 ^ n | n <- [0..]]) 
                                $ map digitToInt $ reverse ns);
                if neg then pure $ IntegerC (-n) else pure $ IntegerC n }) 

-- | A variable is a sequence of lowercase letters
variable :: Parser Variable 
variable = do { xs <- many1 (lower <|> char '\'');
                if xs `elem` ["in", "let", "letrec"] 
                  then fail "Nothing good here" 
                  else pure $ Free xs }

-- | A constructor begins with a capital letter and ends with lowercase letters
constructor :: Parser String
constructor = do { x <- upper;
                   xs <- many lower; 
                   pure (x : xs) }

-- | In order to parse patterns, I must be able to parse variables, constants,
--   constructors, and nested patterns.
pattern :: Parser Pattern
pattern = (char '(' *> whitespace *> pattern <* whitespace <* char ')') <|>
          (constant >>= pure . ConstPat) <|> 
          (variable >>= pure . VarPat) <|>
     --     (constructor >>= pure . DataExp) <|>
          (do { c <- constructor; 
                xs <- many (whitespace *> pattern <* whitespace); 
                pure $ DataPat c xs })

-- | In order to parse expressions, we can reuse much of the above:
--   Constants (ConstExp)
--   Variables (VarExp)
--   Patterns (in Lam, Let, Case)
--   
--   We also must be able to parse Applications (App), Lambdas (Lam),
--   Let bindings (Let), Letrec bindings (Letrec), and Case statements.
baseexpression :: Parser Exp
baseexpression = (char '(' *> expression <* char ')') <|>
                 lambda <|> 
                 lets <|> 
                 casep <|> 
                 (constant >>= pure . ConstExp) <|>
                 (constructor >>= pure . DataExp) <|>
                 (variable >>= pure . VarExp)
                 -- ^ All of the things that can't be applied without being in parens
                

expression :: Parser Exp
expression = many1 (whitespace *> baseexpression <* whitespace)
  >>= \(e:es) -> pure $ foldr (flip App) e es


lambda :: Parser Exp
lambda = do { char '\\';
              p <- whitespace *> pattern <* whitespace;
              char '.';
              e <- whitespace *> expression;
              pure $ Lam p e }

binding :: Parser (Pattern, Exp)
binding = do
  p <- pattern
  whitespace *> char '=' <* whitespace
  e <- expression
  pure (p, e)

bindings :: Parser [(Pattern, Exp)]
bindings = do
  b <- whitespace *> binding <* whitespace;
  bs <- many (whitespace *> char ';' *> whitespace *> binding <* whitespace);
  pure (b:bs)

lets :: Parser Exp
lets = string "let" *>
      (do {
           whitespace; 
           char '{';  
           bs <- bindings;
           char '}';
           whitespace;
           string "in";
           whitespace;
           e <- expression;
           pure $ Letrec bs e})

whitespace = many $ oneOf " \n\r\t"


case_sendings :: Parser [(Pattern, Exp)]
case_sendings = do { p <- whitespace *> pattern;
                     whitespace *> string "->" <* whitespace;
                     e <- whitespace *> expression <* whitespace;
                     (char '}' *> pure [(p, e)]) <|> 
                     (char ';' *> (case_sendings >>= \ps -> pure ((p, e) : ps))) }

casep :: Parser Exp
casep = do { string "case";
             v <- whitespace *> variable <* whitespace;
             string "of" *> whitespace *> char '{';
             s <- case_sendings;
             pure $ Case v s }


