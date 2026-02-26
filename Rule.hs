module Rule where

import Formula (Formula)
import Formula as F (parseFormula)
import Text.Printf
import Token (Token)
import qualified Token as T (Token (..))

data Input
  = Line Int
  | Box Int Int
  | Formula Formula
  deriving (Eq)

instance Show Input where
  show (Line n) = show n
  show (Box from to) = show from ++ "-" ++ show to
  show (Formula formula) = show formula

data Kind
  = AndIntr
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

instance Show Kind where
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

type Rule = (Kind, [Input])

inputStr :: [Input] -> String
inputStr [single] = show single
inputStr (head : tail) = show head ++ ", " ++ inputStr tail

ruleStr :: Rule -> String
ruleStr (kind, inputs) = (show kind) ++ " " ++ inputStr inputs

parse :: [Token] -> Either String Rule
parse (kindTok : inputToks) =
  (,) <$> parseKind kindTok <*> parseInputs inputToks

parseKind :: Token -> Either String Kind
parseKind T.AndIntr = Right AndIntr
parseKind T.AndElim1 = Right AndElim1
parseKind T.AndElim2 = Right AndElim2
parseKind T.OrIntr1 = Right OrIntr1
parseKind T.OrIntr2 = Right OrIntr2
parseKind T.OrElim1 = Right OrElim1
parseKind T.OrElim2 = Right OrElim2
parseKind T.ImplIntr = Right ImplIntr
parseKind T.ImplElim = Right ImplElim
parseKind T.NotIntr = Right NotIntr
parseKind T.NotElim = Right NotElim
parseKind invalid = Left $ printf "Invalid rule '%s'" (show invalid)

parseInputs :: [Token] -> Either String [Input]
parseInputs [] =
  Right []
parseInputs tokens =
  case tokens of
    (T.Number _ : _) ->
      do
        (input, restTokens) <- parseInput tokens
        case restTokens of
          (T.Comma : tail) -> (input :) <$> parseInputs tail
          (unexpected : _) -> Left $ printf "Unexpected '%s' in input" (show unexpected)
          [] -> Right [input]
    _ ->
      do
        (formula, restTokens) <- parseFormula tokens
        case restTokens of
          (T.Comma : tail) -> (Formula formula :) <$> parseInputs tail
          (unexpected : _) -> Left $ printf "Unexpected '%s' in input" (show unexpected)
          [] -> Right [Formula formula]

parseInput :: [Token] -> Either String (Input, [Token])
parseInput ((T.Number from) : T.Hyphen : (T.Number to) : tail) =
  Right (Box from to, tail)
parseInput ((T.Number line) : tail) =
  Right (Line line, tail)
parseInput (unexpected : _) =
  Left $ printf "Unpexpected '%s' in input" (show unexpected)
