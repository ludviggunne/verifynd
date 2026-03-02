module Proof where

import Form (Form)
import Result
import Token (Tok)
import Types

data PRef
  = BoxR Loc LineNo LineNo
  | LineR Loc LineNo

type Rule = Tok

data PEntry
  = BoxP Tok [PEntry]
  | LineP Tok LineNo Form Rule [PRef]

-- Get last line number
numlP :: [PEntry] -> LineNo
numlP [BoxP _ p] = numlP p
numlP [LineP _ l _ _ _] = l
numlP (_ : t) = numlP t

-- Does the proof contain this line?
containsP :: [PEntry] -> LineNo -> Bool
containsP [] _ =
  False
containsP ((LineP _ n' _ _ _) : tl) n =
  n == n' || containsP tl n
containsP ((BoxP _ p) : tl) n =
  containsP p n || containsP tl n

-- Does the first and last lines have
-- the specified line numbers?
matchB :: [PEntry] -> LineNo -> LineNo -> Bool
matchB p a b = case (head p, last p) of
  (LineP _ a' _ _ _, LineP _ b' _ _ _) ->
    a == a' && b == b'
  _ ->
    False

-- Get the first and last formulas of a (sub)proof
getB :: Loc -> [PEntry] -> Result (Form, Form)
getB l p = case (head p, last p) of
  (LineP _ _ f _ _, LineP _ _ g _ _) ->
    return (f, g)
  (BoxP {}, _) ->
    Error [(l, "First entry of this box is also a box")]
  (_, BoxP {}) ->
    Error [(l, "Last entry of this box is also a box")]

getL :: [PEntry] -> LineNo -> PEntry
getL (l@(LineP _ n' _ _ _) : t) n
  | n == n' = l
  | otherwise = getL t n
getL ((BoxP _ p) : t) n
  | containsP p n = getL p n
  | otherwise = getL t n

refB :: [PEntry] -> PRef -> LineNo -> Result (Form, Form)
refB _ (LineR l _) _ =
  Error [(l, "Expected a box reference here")]
refB ((LineP {}) : t) r i =
  refB t r i
refB ((BoxP _ p) : t) r@(BoxR l a b) i
  | not $ containsP p a = refB t r i
  | not $ containsP p b = refB t r i
  | matchB p a b = getB l p
  | containsP p i = refB p r i
  | otherwise = Error [(l, "No reachable box matches this reference")]
refB [] (BoxR l _ _) _ =
  Error [(l, "No reachable box matches this reference")]

refL :: [PEntry] -> PRef -> LineNo -> Result Form
refL _ (BoxR l _ _) _ =
  Error [(l, "Expected a line reference here")]
refL ((LineP _ l f _ _) : t) r@(LineR _ l') i
  | l == l' =
      return f
  | otherwise =
      refL t r i
refL ((BoxP _ p) : t) r@(LineR _ i') i
  | containsP p i' && containsP p i =
      refL p r i
  | otherwise =
      refL t r i
refL [] (LineR l _) _ =
  Error [(l, "No reachable line matches this reference")]
