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
  | Exit
  | Apply Rule
  deriving (Eq)

instance Show Command where
  show (Presume _) = "premise"
  show (Assume _) = "assumption"
  show Open = "open"
  show Close = "close"
  show Exit = "exit"
  show (Apply rule) = "apply " ++ show rule

assertEmpty :: [Token] -> Either String ()
assertEmpty [] = Right ()
assertEmpty _ = Left "Invalid command syntax"

parse :: [Token] -> Either String Command
parse [] =
  Left "Empty command"
parse (T.Name "open" : tail) = do
  assertEmpty tail
  return Open
parse (T.Name "close" : tail) = do
  assertEmpty tail
  return Close
parse (T.Name "exit" : tail) = do
  assertEmpty tail
  return Exit
parse (T.Name "presume" : tail) =
  Presume <$> F.parse tail
parse (T.Name "assume" : tail) =
  Assume <$> F.parse tail
parse (T.Name "apply" : tail) =
  Apply <$> R.parse tail
parse (invalid : _) =
  Left $ printf "Unknown command '%s'" (show invalid)
