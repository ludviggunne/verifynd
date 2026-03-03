module Token where

import Data.Char (isAlpha, isDigit, isSpace)
import Result
import Types

type Tok = (Loc, TagT)

data TagT
  = -- Single characters
    LBraceT
  | RBraceT
  | LParT
  | RParT
  | SemiT
  | CommaT
  | HyphenT
  | -- Rules
    PremT
  | AssumT
  | LemT
  | PbcT
  | CopyT
  | ImplET
  | ImplIT
  | AndIT
  | AndE1T
  | AndE2T
  | OrI1T
  | OrI2T
  | OrET
  | NotIT
  | NotET
  | NotNotET
  | ConET
  | -- Formulas
    ImplT
  | AndT
  | OrT
  | NotT
  | ConT
  | VarT String
  | NumT Int
  deriving (Eq)

scan :: String -> Result [Tok]
scan = scanI 0

scanI :: Loc -> String -> Result [Tok]
-- Skip whitespace
scanI n (' ' : tl) = scanI (n + 1) tl
scanI n ('\t' : tl) = scanI (n + 1) tl
scanI n ('\n' : tl) = scanI (n + 1) tl
scanI n s = scanT n s

scanT :: Loc -> String -> Result [Tok]
scanT n ('/' : '/' : tl) = scanC (n + 2) tl
scanT n ('{' : tl) = ((n, LBraceT) :) <$> scanI (n + 1) tl
scanT n ('}' : tl) = ((n, RBraceT) :) <$> scanI (n + 1) tl
scanT n ('(' : tl) = ((n, LParT) :) <$> scanI (n + 1) tl
scanT n (')' : tl) = ((n, RParT) :) <$> scanI (n + 1) tl
scanT n (';' : tl) = ((n, SemiT) :) <$> scanI (n + 1) tl
scanT n (',' : tl) = ((n, CommaT) :) <$> scanI (n + 1) tl
scanT n ('-' : '>' : 'i' : tl) = ((n, ImplIT) :) <$> scanI (n + 3) tl
scanT n ('-' : '>' : 'e' : tl) = ((n, ImplET) :) <$> scanI (n + 3) tl
scanT n ('&' : '&' : 'i' : tl) = ((n, AndIT) :) <$> scanI (n + 3) tl
scanT n ('&' : '&' : 'e' : '1' : tl) = ((n, AndE1T) :) <$> scanI (n + 4) tl
scanT n ('&' : '&' : 'e' : '2' : tl) = ((n, AndE2T) :) <$> scanI (n + 4) tl
scanT n ('|' : '|' : 'i' : '1' : tl) = ((n, OrI1T) :) <$> scanI (n + 4) tl
scanT n ('|' : '|' : 'i' : '2' : tl) = ((n, OrI2T) :) <$> scanI (n + 4) tl
scanT n ('|' : '|' : 'e' : tl) = ((n, OrET) :) <$> scanI (n + 3) tl
scanT n ('!' : 'i' : tl) = ((n, NotIT) :) <$> scanI (n + 2) tl
scanT n ('!' : 'e' : tl) = ((n, NotET) :) <$> scanI (n + 2) tl
scanT n ('!' : '!' : 'e' : tl) = ((n, NotNotET) :) <$> scanI (n + 2) tl
scanT n ('?' : 'e' : tl) = ((n, ConET) :) <$> scanI (n + 2) tl
scanT n ('-' : '>' : tl) = ((n, ImplT) :) <$> scanI (n + 2) tl
scanT n ('&' : '&' : tl) = ((n, AndT) :) <$> scanI (n + 2) tl
scanT n ('|' : '|' : tl) = ((n, OrT) :) <$> scanI (n + 2) tl
scanT n ('!' : tl) = ((n, NotT) :) <$> scanI (n + 1) tl
scanT n ('?' : tl) = ((n, ConT) :) <$> scanI (n + 1) tl
scanT n ('-' : tl) = ((n, HyphenT) :) <$> scanI (n + 1) tl
scanT n s@(h : _)
  | isAlpha h = scanS n s
  | isDigit h = scanN n s
  | otherwise = Error [(n, "Unexpected character: " ++ [h])]
scanT n "" = return []

-- Skip a comment
scanC :: Loc -> String -> Result [Tok]
scanC _ "" = return []
scanC n ('\n' : tl) = scanI (n + 1) tl
scanC n (_ : tl) = scanC (n + 1) tl

scanS :: Loc -> String -> Result [Tok]
scanS n s = case span isAlpha s of
  ("premise", s') -> ((n, PremT) :) <$> scanI (n + length "premise") s'
  ("assumption", s') -> ((n, AssumT) :) <$> scanI (n + length "assumption") s'
  ("PBC", s') -> ((n, PbcT) :) <$> scanI (n + length "PBC") s'
  ("LEM", s') -> ((n, LemT) :) <$> scanI (n + length "LEM") s'
  ("copy", s') -> ((n, CopyT) :) <$> scanI (n + length "copy") s'
  (v, s') -> ((n, VarT v) :) <$> scanI (n + length v) s'

scanN :: Loc -> String -> Result [Tok]
scanN n s = case reads s of
  [(i, s')] -> ((n, NumT i) :) <$> scanI (n + length (show i)) s'
  _ -> Error [(n, "Invalid number")]

instance Show TagT where
  show LBraceT = "{"
  show RBraceT = "'}'"
  show LParT = "'('"
  show RParT = "')'"
  show SemiT = "';'"
  show CommaT = "','"
  show HyphenT = "'-'"
  show PremT = "'premise'"
  show AssumT = "'assumption'"
  show LemT = "'LEM'"
  show PbcT = "'PBC'"
  show CopyT = "'copy'"
  show ImplET = "'->e'"
  show ImplIT = "'->i'"
  show AndIT = "'&&i'"
  show AndE1T = "'&&e1'"
  show AndE2T = "'&&e2'"
  show OrI1T = "'||i1'"
  show OrI2T = "'||i2'"
  show OrET = "'||e'"
  show NotIT = "'!i'"
  show NotET = "'!e'"
  show NotNotET = "'!!e'"
  show ConET = "'?e'"
  show ImplT = "'->'"
  show AndT = "'&&'"
  show OrT = "'||'"
  show NotT = "'!'"
  show ConT = "'?'"
  show (VarT s) = "'" ++ s ++ "'"
  show (NumT n) = "'" ++ show n ++ "'"
