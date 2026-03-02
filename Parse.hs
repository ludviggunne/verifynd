module Parse where

import Form (Form, TagF (..))
import Proof (PEntry (..), PRef (..))
import Result
import Token (TagT (..), Tok)

expectT :: [Tok] -> TagT -> Result (Tok, [Tok])
expectT (tok@(n, t) : tl) t'
  | t == t' = return (tok, tl)
  | otherwise =
      Error [(n, "Unexpected " ++ show t ++ ", expected " ++ show t')]
expectT [] t =
  Error [(-1, "Unexpected end of input, expected " ++ show t)]

expectN :: [Tok] -> Result (Int, Tok, [Tok])
expectN (t@(n, NumT v) : tl) =
  return (v, t, tl)
expectN ((n, t) : tl) =
  Error [(n, "Unexpected " ++ show t ++ ", expected number")]
expectN [] =
  Error [(-1, "Unexpected end of input, expected number")]

expectR :: [Tok] -> Result (Tok, [Tok])
expectR (t@(_, PremT) : tl) = return (t, tl)
expectR (t@(_, AssumT) : tl) = return (t, tl)
expectR (t@(_, LemT) : tl) = return (t, tl)
expectR (t@(_, PbcT) : tl) = return (t, tl)
expectR (t@(_, CopyT) : tl) = return (t, tl)
expectR (t@(_, ImplET) : tl) = return (t, tl)
expectR (t@(_, ImplIT) : tl) = return (t, tl)
expectR (t@(_, AndIT) : tl) = return (t, tl)
expectR (t@(_, AndE1T) : tl) = return (t, tl)
expectR (t@(_, AndE2T) : tl) = return (t, tl)
expectR (t@(_, OrI1T) : tl) = return (t, tl)
expectR (t@(_, OrI2T) : tl) = return (t, tl)
expectR (t@(_, OrET) : tl) = return (t, tl)
expectR (t@(_, NotIT) : tl) = return (t, tl)
expectR (t@(_, NotET) : tl) = return (t, tl)
expectR (t@(_, NotNotET) : tl) = return (t, tl)
expectR (t@(_, ConET) : tl) = return (t, tl)
expectR ((n, t) : _) =
  Error [(n, "Unexpected " ++ show t ++ ", expected rule")]

parse :: [Tok] -> Result [PEntry]
parse ts = do
  (p, ts) <- parseP ts
  case ts of
    [] -> return p
    ((n, _) : _) -> Error [(n, "Unconsumed token at end of input")]

parseP :: [Tok] -> Result ([PEntry], [Tok])
parseP [] =
  return ([], [])
parseP ts = do
  (e, ts) <- parseE ts
  case ts of
    ((_, RBraceT) : _) -> return ([e], ts)
    _ -> do
      (r, ts) <- parseP ts
      return (e : r, ts)

parseE :: [Tok] -> Result (PEntry, [Tok])
parseE ((n, LBraceT) : tl) = do
  (p, ts) <- parseP tl
  (t, ts) <- expectT ts RBraceT </ (n, "This box is not closed")
  return (BoxP t p, ts)
parseE ts = do
  (v, n, ts) <- expectN ts
  (_, ts) <- expectT ts PeriodT
  (f, ts) <- parseF ts
  (_, ts) <- expectT ts PeriodT
  (r, ts) <- expectR ts
  (rs, ts) <- parseR ts
  (_, ts) <- expectT ts SemiT
  return (LineP n v f r rs, ts)

parseF :: [Tok] -> Result (Form, [Tok])
parseF ts = do
  (l@(n, _), ts) <- parseF' ts
  case ts of
    ((_, ImplT) : ts) -> do
      (r, ts) <- parseF' ts
      return ((n, ImplF l r), ts)
    ((_, AndT) : ts) -> do
      (r, ts) <- parseF' ts
      return ((n, AndF l r), ts)
    ((_, OrT) : ts) -> do
      (r, ts) <- parseF' ts
      return ((n, OrF l r), ts)
    _ -> return (l, ts)

parseF' :: [Tok] -> Result (Form, [Tok])
parseF' ((n, LParT) : tl) = do
  (f, ts) <- parseF tl
  (_, ts) <- expectT ts RParT </ (n, "This parenthesis is not closed")
  return (f, ts)
parseF' ((n, VarT s) : tl) =
  return ((n, VarF s), tl)
parseF' ((n, ConT) : tl) =
  return ((n, ConF), tl)
parseF' ((_, NotT) : tl) = do
  (f@(n, _), ts) <- parseF' tl
  return ((n, NotF f), ts)
parseF' ((n, t) : _) =
  Error [(n, "Unexpected token " ++ show t ++ " in formula")]

parseR :: [Tok] -> Result ([PRef], [Tok])
parseR ts@((n, NumT _) : _) = do
  (xv, x, ts) <- expectN ts
  (r, ts) <- case ts of
    ((_, HyphenT) : ts) -> do
      (yv, y, ts) <- expectN ts
      return (BoxR n xv yv, ts)
    _ -> return (LineR n xv, ts)
  case ts of
    ((_, CommaT) : ts) -> do
      (rs, ts) <- parseR ts
      return (r : rs, ts)
    _ -> return ([r], ts)
parseR ts = return ([], ts)
