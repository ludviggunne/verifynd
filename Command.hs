module Command where

import Formula (Formula)
import qualified Formula as F (Formula (..), parse)
import Rule (Rule)
import qualified Rule as R (parse)
import Text.Printf
import Token (Token)
import qualified Token as T (Token (..))

data Command
  = Presume Formula
  | Assume Formula
  | Open
  | Close
  | Apply Rule
  deriving (Show, Eq)

parse :: [Token] -> Either String Command
parse [] = Left "Empty command"
parse [T.Name "open"] = Right Open
parse [T.Name "close"] = Right Close
parse ((T.Name "presume") : tail) = Presume <$> F.parse tail
parse ((T.Name "assume") : tail) = Assume <$> F.parse tail
parse (T.Name "apply" : tail) = Apply <$> R.parse tail
parse (invalid : _) = Left $ printf "Unknown command '%s'" (show invalid)
