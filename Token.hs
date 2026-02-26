module Token where

import Data.Bifunctor
import Data.Char
import Text.Printf

data Token
  = Impl
  | And
  | Or
  | Hyphen
  | Con
  | Lpar
  | Rpar
  | Comma
  | Name [Char]
  | Number Int
  | AndIntr
  | AndElim1
  | AndElim2
  | OrIntr1
  | OrIntr2
  | OrElim1
  | OrElim2
  | ImplIntr
  | ImplElim
  | NotIntr
  | NotElim
  deriving (Eq)

instance Show Token where
  show Impl = "->"
  show And = "&"
  show Or = "|"
  show Hyphen = "-"
  show Con = "_|_"
  show Lpar = "("
  show Rpar = ")"
  show Comma = ","
  show (Name name) = "\"" ++ name ++ "\""
  show (Number n) = show n
  show AndIntr = "&i"
  show AndElim1 = "&e1"
  show AndElim2 = "&e2"
  show OrIntr1 = "|i1"
  show OrIntr2 = "|i2"
  show OrElim1 = "|e1"
  show OrElim2 = "|e2"
  show ImplIntr = "->i"
  show ImplElim = "->e"
  show NotIntr = "-i"
  show NotElim = "-e"

tokenize :: [Char] -> Either String [Token]
tokenize [] = Right []
tokenize (' ' : tail) = tokenize tail
tokenize ('-' : '>' : 'i' : tail) = (ImplIntr :) <$> tokenize tail
tokenize ('-' : '>' : 'e' : tail) = (ImplElim :) <$> tokenize tail
tokenize ('&' : 'i' : tail) = (AndIntr :) <$> tokenize tail
tokenize ('&' : 'e' : '1' : tail) = (AndElim1 :) <$> tokenize tail
tokenize ('&' : 'e' : '2' : tail) = (AndElim2 :) <$> tokenize tail
tokenize ('|' : 'i' : '1' : tail) = (OrIntr1 :) <$> tokenize tail
tokenize ('|' : 'i' : '2' : tail) = (OrIntr2 :) <$> tokenize tail
tokenize ('|' : 'e' : '1' : tail) = (OrElim1 :) <$> tokenize tail
tokenize ('|' : 'e' : '2' : tail) = (OrElim2 :) <$> tokenize tail
tokenize ('-' : 'i' : tail) = (NotIntr :) <$> tokenize tail
tokenize ('-' : 'e' : tail) = (NotElim :) <$> tokenize tail
tokenize ('-' : '>' : tail) = (Impl :) <$> tokenize tail
tokenize ('-' : tail) = (Hyphen :) <$> tokenize tail
tokenize ('&' : tail) = (And :) <$> tokenize tail
tokenize ('|' : tail) = (Or :) <$> tokenize tail
tokenize ('_' : '|' : '_' : tail) = (Con :) <$> tokenize tail
tokenize ('(' : tail) = (Lpar :) <$> tokenize tail
tokenize (')' : tail) = (Rpar :) <$> tokenize tail
tokenize (',' : tail) = (Comma :) <$> tokenize tail
tokenize (head : tail)
  | isAlpha head = do
      (name, tokens) <- tokenizeName (head : tail)
      Right (Name name : tokens)
  | isDigit head = do
      (num, tokens) <- tokenizeNumber 0 (head : tail)
      Right (Number num : tokens)
  | otherwise = Left $ printf "Invalid character '%c'" head

tokenizeName :: [Char] -> Either String ([Char], [Token])
tokenizeName [] = Right ([], [])
tokenizeName (head : tail)
  | isAlpha head = do
      (restChars, restTokens) <- tokenizeName tail
      return (head : restChars, restTokens)
  | otherwise = do
      tokens <- tokenize (head : tail)
      return ([], tokens)

tokenizeNumber :: Int -> [Char] -> Either String (Int, [Token])
tokenizeNumber n [] = Right (n, [])
tokenizeNumber n (head : tail)
  | isDigit head = tokenizeNumber n' tail
  | otherwise = (n,) <$> tokenize (head : tail)
  where
    n' = n * 10 + digitToInt head
