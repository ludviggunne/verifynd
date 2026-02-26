module Proof where

import Command (Command)
import Formula (Formula)
import Rule (Input)
import qualified Rule as R (Input (..))

data Entry
  = Line Command Formula
  | Box Proof
  deriving (Show, Eq)

type Proof = [Entry]

invalidReference :: String -> Either String t
invalidReference descr = Left $ "Invalid reference: " ++ descr

proofLength :: Proof -> Int
proofLength [] = 0
proofLength ((Line _ _) : tail) = 1 + proofLength tail
proofLength ((Box subproof) : tail) = proofLength subproof + proofLength tail

proofInsert :: Proof -> Int -> Entry -> Proof
proofInsert [] 0 entry = [entry]
proofInsert [last] 0 entry = last : [entry]
proofInsert [Box subproof] n entry = [Box (proofInsert subproof (n - 1) entry)]
proofInsert (head : tail) n entry = head : proofInsert tail n entry

lastLine :: Proof -> (Command, Formula)
lastLine [Box subproof] = lastLine subproof
lastLine [Line command formula] = (command, formula)
lastLine (_ : tail) = lastLine tail

ref :: Proof -> Input -> Either String Entry
ref (head : tail) input = case input of
  R.Line n ->
    if n == 1
      then case head of
        Line _ _ -> Right head
        Box subproof -> ref subproof input
      else case head of
        Line _ _ -> ref tail $ R.Line (n - 1)
        Box subproof ->
          if n <= len
            then ref subproof input
            else ref tail (R.Line (n - len))
          where
            len = proofLength subproof
  R.Box start end ->
    if start == 1
      then case head of
        Line _ _ -> invalidReference "Not a box"
        Box subproof ->
          if proofLength subproof == end
            then Right head
            else invalidReference "?"
      else case head of
        Line _ _ -> ref tail $ R.Box (start - 1) (end - 1)
        Box subproof ->
          if end <= len
            then ref subproof input
            else ref tail $ R.Box (start - len) (end - len)
          where
            len = proofLength subproof
