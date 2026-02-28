module Proof where

import Annotation (Annotation)
import qualified Annotation as A (Annotation (..), parse)
import qualified Data.Bifunctor as Bifunctor (first)
import Data.Char (isSpace)
import Formula (Formula)
import qualified Formula as F (Formula (..), parse)
import Result
import Text.Printf

data Proof
  = Box [Proof]
  | Line Formula Annotation
  deriving (Eq, Show)

len :: Proof -> Int
len (Line _ _) = 1
len (Box []) = 0
len (Box (hd : tl)) =
  len hd + len (Box tl)

get :: Proof -> Int -> Result (Formula, Annotation)
get (Line f a) 1 = Ok (f, a)
get (Box (hd : tl)) n =
  if l >= n
    then get hd n
    else get (Box tl) (n - l)
  where
    l = len hd
get _ _ = err

ref :: Proof -> Int -> Int -> Result Formula
ref (Box []) _ _ =
  errMsg "invalid reference"
ref (Box ((Line f _) : _)) 1 n =
  if 1 < n
    then Ok f
    else errMsg "self-reference"
ref (Box ((Line _ _) : tl)) r n =
  ref (Box tl) (r - 1) (n - 1)
ref (Box (hd : tl)) r n =
  if l >= n
    then ref hd r n
    else ref (Box tl) (r - l) (n - l)
  where
    l = len hd
ref _ _ _ = errMsg "bad"

refBox :: Proof -> (Int, Int) -> Int -> Result (Formula, Formula)
refBox (Box []) _ _ =
  errMsg "invalid reference"
refBox (Box ((Line f _) : _)) (1, _) n =
  errMsg "not a box"
refBox (Box ((Line _ _) : tl)) (s, e) n =
  refBox (Box tl) (s - 1, e - 1) (n - 1)
refBox (Box (hd@(Box p) : _)) (1, e) n =
  if len hd == e
    then
      if e == n
        then errMsg "self-reference"
        else case (head p, last p) of
          (Line f _, Line g _) -> Ok (f, g)
          _ -> errMsg "first and last elements of box are not lines"
    else errMsg "reference range does not match box"
refBox (Box (hd@(Box _) : tl)) r@(s, e) n =
  if l >= n
    then refBox hd r n
    else refBox (Box tl) (s - l, e - l) (n - l)
  where
    l = len hd
refBox _ _ _ = errMsg "bad"

data ProofToken
  = LinePt Formula Annotation
  | OpenPt
  | ClosePt
  deriving (Show, Eq)

parse :: String -> Result Proof
parse s =
  let ls = lines s
   in do
        toks <- scan ls 0 0
        fst <$> constr toks

scan :: [[Char]] -> Int -> Int -> Result [ProofToken]
scan [] _ _ = return []
scan (hd : tl) n d =
  case dropWhile isSpace hd of
    (';' : _) -> scan tl n d
    "" -> scan tl n d
    _ -> do
      (d', f, a) <- parseL hd (n + 1)
      rest <- scan tl (n + 1) d'
      case compare d' d of
        LT -> return $ ClosePt : LinePt f a : rest
        GT -> return $ OpenPt : LinePt f a : rest
        EQ -> return $ LinePt f a : rest

constr :: [ProofToken] -> Result (Proof, [ProofToken])
constr (OpenPt : tl) = do
  (box, tl') <- constr tl
  case tl' of
    (ClosePt : tl'') -> do
      (rest, tl''') <- constr tl''
      case rest of
        Box p -> return (Box (box : p), tl''')
        _ -> errMsg "!box"
    _ -> errMsg "!close"
constr (LinePt f a : tl) = do
  (rest, tl') <- constr tl
  case rest of
    Box p -> return (Box (Line f a : p), tl')
    _ -> errMsg "!box"
constr v = return (Box [], v)

parseL :: [Char] -> Int -> Result (Int, Formula, Annotation)
parseL s n = do
  (n', s') <- parseN s
  if n /= n'
    then errMsg "inconsistent line numbering"
    else
      let (d, s'') = parseD s'
       in do
            (f, s''') <- F.parse s''
            s'''' <- delim s'''
            (a, s''''') <- A.parse s''''
            case dropWhile isSpace s''''' of
              "" -> Ok (d, f, a)
              (';' : _) -> Ok (d, f, a)
              _ -> errMsg "trailing characters"

parseN :: [Char] -> Result (Int, [Char])
parseN (' ' : tl) = parseN tl
parseN ('\t' : tl) = parseN tl
parseN s = case reads s of
  [(n, '.' : ' ' : tl)] -> Ok (n, tl)
  _ -> errMsg "invalid line number"

parseD :: [Char] -> (Int, [Char])
parseD (' ' : tl) = parseD tl
parseD ('\t' : tl) = parseD tl
parseD ('|' : tl) = Bifunctor.first (+ 1) $ parseD tl
parseD s = (0, s)

delim :: [Char] -> Result [Char]
delim (' ' : s) = delim s
delim ('\t' : s) = delim s
delim (';' : s) = Ok s
delim _ = errMsg "missing delimiter"
