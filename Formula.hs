module Formula where

import Data.Char (isAlpha, isSpace)
import Result
import Text.Printf

data Formula
  = Impl Formula Formula
  | Conj Formula Formula
  | Disj Formula Formula
  | Neg Formula
  | Con
  | Name [Char]
  | Wildcard

instance Eq Formula where
  (==) (Impl f g) (Impl f' g') =
    f == f' && g == g'
  (==) (Conj f g) (Conj f' g') =
    f == f' && g == g'
  (==) (Disj f g) (Disj f' g') =
    f == f' && g == g'
  (==) (Neg f) (Neg f') =
    f == f'
  (==) Con Con =
    True
  (==) (Name n) (Name n') =
    n == n'
  (==) Wildcard _ =
    True
  (==) _ Wildcard =
    True
  (==) _ _ =
    False

par :: Formula -> String
par s = "(" ++ show s ++ ")"

atomize :: Formula -> String
atomize f@(Impl _ _) = par f
atomize f@(Conj _ _) = par f
atomize f@(Disj _ _) = par f
atomize f = show f

instance Show Formula where
  show :: Formula -> String
  show (Impl l r) = atomize l ++ " → " ++ atomize r
  show (Conj l r) = atomize l ++ " ∧ " ++ atomize r
  show (Disj l r) = atomize l ++ " ∨ " ++ atomize r
  show (Neg f) = "¬" ++ atomize f
  show Con = "⊥"
  show (Name name) = name
  show Wildcard = "□"

syntaxError :: String -> Result t
syntaxError arg = errMsg $ printf "syntax error in formula: %s" arg

parse :: [Char] -> Result (Formula, [Char])
parse [] = syntaxError "empty formula"
parse (' ' : s) = parse s
parse ('\t' : s) = parse s
parse s = do
  (l, s') <- parseAtom s
  case dropWhile isSpace s' of
    ('-' : '>' : s'') -> do
      (r, s''') <- parseAtom s''
      return (Impl l r, s''')
    ('&' : s'') -> do
      (r, s''') <- parseAtom s''
      return (Conj l r, s''')
    ('|' : s'') -> do
      (r, s''') <- parseAtom s''
      return (Disj l r, s''')
    _ -> return (l, s')

parseAtom :: [Char] -> Result (Formula, [Char])
parseAtom [] = syntaxError "empty atom"
parseAtom (' ' : s) = parseAtom s
parseAtom ('\t' : s) = parseAtom s
parseAtom ('-' : s) = do
  (f, s') <- parseAtom $ dropWhile isSpace s
  return (Neg f, s')
parseAtom ('(' : s) = do
  (f, s') <- parse s
  case dropWhile isSpace s' of
    (')' : s'') -> return (f, s')
    _ -> syntaxError "missing closing parenthesis"
parseAtom ('_' : '|' : '_' : s) = return (Con, s)
parseAtom s@(c : _)
  | isAlpha c =
      let (name, rem) = span isAlpha s
       in return (Name name, rem)
  | otherwise =
      syntaxError s

(~=) :: Formula -> Formula -> Result ()
(~=) f f' =
  if f == f'
    then Ok ()
    else errMsg $ show f ++ " does not match " ++ show f'
