module Proof where

import Expr
import Location
import Ref
import Token

data Entry
  = Box
      { loc :: Location,
        entries :: [Entry]
      }
  | Line
      { loc :: Location,
        num :: Int,
        exp :: Expr,
        rule :: Token,
        refs :: [Ref]
      }
  deriving (Show)

contains :: [Entry] -> Int -> Bool
contains [] _ = False
contains (entry@(Proof.Line {}) : rest) n =
  n == Proof.num entry || rest `contains` n
contains (entry@(Proof.Box {}) : rest) n =
  entries entry `contains` n || rest `contains` n
