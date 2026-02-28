module Annotation where

import qualified Data.Bifunctor as Bifunctor (first)
import Data.Char (isDigit, isSpace)
import Formula (Formula)
import qualified Formula as F (parse)
import Result
import Text.Printf

data RefT
  = LineRT
  | BoxRT
  deriving (Eq)

instance Show RefT where
  show LineRT = "<line>"
  show BoxRT = "<box>"

data Reference
  = LineRef Int
  | BoxRef (Int, Int)
  deriving (Eq, Show)

data Rule
  = ImplIntr
  | ImplElim
  | ConjIntr
  | ConjElim1
  | ConjElim2
  | DisjIntr1
  | DisjIntr2
  | DisjElim
  | NegIntr
  | NegElim
  | Lem
  | Pbc
  deriving (Eq)

instance Show Rule where
  show ImplIntr = "->i"
  show ImplElim = "->e"
  show ConjIntr = "&i"
  show ConjElim1 = "&e1"
  show ConjElim2 = "&e2"
  show DisjIntr1 = "|i1"
  show DisjIntr2 = "|i2"
  show DisjElim = "|e"
  show NegIntr = "-i"
  show NegElim = "-e"
  show Lem = "LEM"
  show Pbc = "PBC"

data Annotation
  = Premise
  | Assumption
  | Rule Rule [Reference]
  deriving (Eq, Show)

syntaxError :: String -> Result t
syntaxError arg = errMsg $ printf "syntax error in annotation: %s" arg

parse :: [Char] -> Result (Annotation, [Char])
parse (' ' : str) = parse str
parse ('\t' : str) = parse str
parse s =
  let (start, s') = break isSpace s
   in case start of
        "premise" -> Ok (Premise, s')
        "assumption" -> Ok (Assumption, s')
        _ -> do
          rule <- parseRule start
          (refs, s'') <- parseRefs s'
          return (Rule rule refs, s'')

parseRule :: [Char] -> Result Rule
parseRule "->i" = Ok ImplIntr
parseRule "->e" = Ok ImplElim
parseRule "&i" = Ok ConjIntr
parseRule "&e1" = Ok ConjElim1
parseRule "&e2" = Ok ConjElim2
parseRule "|i1" = Ok DisjIntr1
parseRule "|i2" = Ok DisjIntr2
parseRule "|e" = Ok DisjElim
parseRule "-i" = Ok NegIntr
parseRule "-e" = Ok NegElim
parseRule "LEM" = Ok Lem
parseRule "PBC" = Ok Pbc
parseRule s = syntaxError $ "invalid rule " ++ s

parseRefs :: [Char] -> Result ([Reference], [Char])
parseRefs (' ' : str) = parseRefs str
parseRefs ('\t' : str) = parseRefs str
parseRefs [] = Ok ([], [])
parseRefs s = do
  (first, s') <- parseRef s
  case dropWhile isSpace s' of
    (',' : s'') -> do
      (refs, s''') <- parseRefs s''
      return (first : refs, s''')
    _ -> return ([first], s')

parseRef :: [Char] -> Result (Reference, [Char])
parseRef (' ' : str) = parseRef str
parseRef ('\t' : str) = parseRef str
parseRef s@(h : t) = do
  (start, s') <- parseNumber s
  case dropWhile isSpace s' of
    ('-' : s'') -> do
      (end, s''') <- parseNumber s''
      return (BoxRef (start, end), s''')
    _ -> return (LineRef start, s')

parseNumber :: [Char] -> Result (Int, [Char])
parseNumber (' ' : s) = parseNumber s
parseNumber ('\t' : s) = parseNumber s
parseNumber str =
  case reads str of
    [res] -> Ok res
    _ -> syntaxError str
