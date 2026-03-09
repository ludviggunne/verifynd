module Token (Token (..), Tag (..), tokenize) where

import Control.Applicative
import Control.Monad (unless, void, when)
import Data.Char (isAlpha, isDigit)
import Data.List (find, isPrefixOf)
import Error (Error (..))
import Location

-- import Parser

data Token = Token
  { tag :: Tag,
    loc :: Location,
    str :: String
  }
  deriving (Eq)

instance Show Token where
  show :: Token -> String
  show tok
    | tag tok == Eof = "EOF"
    | otherwise = str tok

data Tag
  = Impl
  | And
  | Or
  | Var
  | Neg
  | Con
  | Wildcard
  | LPar
  | RPar
  | LBrc
  | RBrc
  | Semi
  | Comma
  | Hyphen
  | ImplE
  | ImplI
  | AndE1
  | AndE2
  | AndI
  | OrE
  | OrI1
  | OrI2
  | NegI
  | NegE
  | NegNegE
  | ConE
  | Prem
  | Assum
  | Lem
  | Pbc
  | Mt
  | Copy
  | Num
  | Eof
  deriving (Eq, Show)

tags :: [(String, Tag)]
tags =
  [ ("->e", ImplE),
    ("->i", ImplI),
    ("&&e1", AndE1),
    ("&&e2", AndE2),
    ("&&i", AndI),
    ("||e", OrE),
    ("||i1", OrI1),
    ("||i2", OrI2),
    ("!i", NegI),
    ("!e", NegE),
    ("!!e", NegNegE),
    ("?e", ConE),
    ("->", Impl),
    ("&&", And),
    ("||", Or),
    ("!", Neg),
    ("?", Con),
    ("_", Wildcard),
    ("(", LPar),
    (")", RPar),
    ("premise", Prem),
    ("assumption", Assum),
    ("LEM", Lem),
    ("PBC", Pbc),
    ("MT", Mt),
    ("copy", Copy),
    ("{", LBrc),
    ("}", RBrc),
    (";", Semi),
    ("-", Hyphen),
    (",", Comma)
  ]

tokenize :: Int -> String -> Either [Error] [Token]
tokenize n str
  | null str = pure [Token Eof (Location (n - 1) n) ""]
  -- Comments
  | "//" `isPrefixOf` str =
      let (line, rest) = span (/= '\n') str
          n' = n + length line
       in tokenize n' rest
  -- Whitespace
  | head str `elem` " \n\t" = tokenize (n + 1) (tail str)
  -- Variables/keywords
  | isAlpha $ head str =
      let (name, rest) = span isAlpha str
          n' = n + length name
          tag = maybe Var snd (find (\(s, t) -> s == name) tags)
       in (Token tag (Location n n') name :) <$> tokenize n' rest
  -- Numbers
  | isDigit $ head str =
      let (digits, rest) = span isDigit str
          n' = n + length digits
       in (Token Num (Location n n') digits :) <$> tokenize n' rest
  -- Other tokens
  | otherwise = case find (\(s, t) -> s `isPrefixOf` str) tags of
      Just (s, tag) ->
        let n' = n + length s
            rest = drop (length s) str
         in (Token tag (Location n n') s :) <$> tokenize n' rest
      Nothing -> Left [Error (Location n (n + 1)) "Unexpected character"]
