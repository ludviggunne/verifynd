module Formula where

import Text.Printf
import Token (Token)
import qualified Token as T (Token (..))

data Formula
  = Binop Token Formula Formula
  | Name [Char]
  | Not Formula
  | Con
  deriving (Eq)

instance Show Formula where
  show (Binop tok lhs rhs) = "(" ++ show lhs ++ op ++ show rhs ++ ")"
    where
      op = case tok of
        T.And -> " & "
        T.Or -> " | "
        T.Impl -> " -> "
  show (Not sub) = "-(" ++ show sub ++ ")"
  show (Name name) = name
  show Con = "_|_"

parse :: [Token] -> Either String Formula
parse tokens = do
  (formula, trailing) <- parseFormula tokens
  case trailing of
    [] -> Right formula
    _ -> Left "Trailing tokens"

parseFormula :: [Token] -> Either String (Formula, [Token])
parseFormula [] = Left "Empty formula"
parseFormula tokens = do
  (lhs, tokens) <- parseAtom tokens
  case tokens of
    (T.And : tail) -> do
      (rhs, tokens) <- parseAtom tail
      return (Binop T.And lhs rhs, tokens)
    (T.Or : tail) -> do
      (rhs, tokens) <- parseAtom tail
      return (Binop T.Or lhs rhs, tokens)
    (T.Impl : tail) -> do
      (rhs, tokens) <- parseAtom tail
      return (Binop T.Impl lhs rhs, tokens)
    _ -> return (lhs, tokens)

parseAtom :: [Token] -> Either String (Formula, [Token])
parseAtom (T.Name name : tail) = Right (Name name, tail)
parseAtom (T.Con : tail) = Right (Con, tail)
parseAtom (T.Hyphen : tail) = do
  (sub, tokens) <- parseAtom tail
  return (Not sub, tokens)
parseAtom (T.Lpar : tail) = do
  (sub, tokens) <- parseFormula tail
  case tokens of
    (T.Rpar : tail) -> Right (sub, tail)
    _ -> Left $ printf "Missing closing parenthesis in formula"
parseAtom (unexpected : _) = Left $ printf "Unexpected '%s' in formula" $ show unexpected
