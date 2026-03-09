module Ref where

import Location
import Text.Printf

data Ref
  = Box
      { loc :: Location,
        start :: Int,
        end :: Int
      }
  | Line
      { loc :: Location,
        num :: Int
      }
  deriving (Eq)

instance Show Ref where
  show :: Ref -> String
  show (Box _ start end) = printf "%d-%d" start end
  show (Line _ num) = printf "%d" num
