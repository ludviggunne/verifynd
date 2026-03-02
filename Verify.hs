module Verify where

import Form (Form, TagF (..), (<<~), (<~))
import Proof (PEntry (..), PRef (..), getL, numlP, refB, refL)
import Result
import Token (TagT (..))

verify :: [PEntry] -> Result ()
verify p = do
  _ <- verifyN 0 p
  verifyP p 1 (numlP p)

-- Verify that line numbers are consecutive
-- and that only previous lines are referenced
verifyN :: Int -> [PEntry] -> Result Int
verifyN _ [] = return 0
verifyN n ((BoxP _ p) : tl) = do
  d <- verifyN n p
  (d +) <$> verifyN (n + d) tl
verifyN n ((LineP (l, _) n' _ _ rs) : tl) = do
  if n' /= n + 1
    then Error [(l, "Inconsistent numbering (expected " ++ show (n + 1) ++ ")")]
    else do
      verifyR n' rs
      (1 +) <$> verifyN (n + 1) tl
  where
    verifyR :: Int -> [PRef] -> Result ()
    verifyR _ [] = return ()
    verifyR n ((LineR l n') : tl)
      | n == n' = Error [(l, "Reference to the same line")]
      | n < n' = Error [(l, "Reference to a later line")]
      | otherwise = verifyR n tl
    verifyR n ((BoxR l n' n'') : tl)
      | n' >= n'' = Error [(l, "Invalid line range")]
      | n < n'' = Error [(l, "Reference to a later line")]
      | n == n'' = Error [(l, "Reference to the same line")]
      | otherwise = verifyR n tl

-- Verify all applications in proof
verifyP :: [PEntry] -> Int -> Int -> Result ()
verifyP p n m
  | n > m =
      return ()
  | otherwise = do
      verifyL p n (getL p n)
      verifyP p (n + 1) m

implF :: Form -> Form -> Form
implF f g = (0, ImplF f g)

andF :: Form -> Form -> Form
andF f g = (0, AndF f g)

orF :: Form -> Form -> Form
orF f g = (0, OrF f g)

notF :: Form -> Form
notF f = (0, NotF f)

conF :: Form
conF = (0, ConF)

holeF :: Form
holeF = (0, HoleF)

getR :: [PRef] -> Int -> Int -> Int -> Result PRef
getR rs i m l
  | m > length rs =
      Error [(l, "Not enough references")]
  | m < length rs =
      Error [(l, "Too many references")]
  | otherwise = return (rs !! i)

-- Verify line
verifyL :: [PEntry] -> Int -> PEntry -> Result ()
verifyL p n (LineP _ _ f (_, PremT) _) =
  return ()
verifyL p n (LineP _ _ f (_, AssumT) _) =
  return ()
verifyL p n (LineP _ _ f (_, LemT) _) = do
  fs <- f <<~ orF holeF (notF holeF)
  head fs <~ (fs !! 1)
  return ()
verifyL p n (LineP _ _ f (l, PbcT) rs) = do
  r <- getR rs 0 1 l
  (g, h) <- refB p r n
  h <~ conF
  g <~ notF f
verifyL p n (LineP _ _ f (l, CopyT) rs) = do
  r <- getR rs 0 1 l
  g <- refL p r n
  f <~ g
verifyL p n (LineP _ _ f (l, ImplET) rs) = do
  r <- getR rs 0 2 l
  s <- getR rs 1 2 l
  g <- refL p r n
  h <- refL p s n
  g <~ implF h f </ (l, "Referenced by this rule")
verifyL p n (LineP _ _ f (l, ImplIT) rs) = do
  r <- getR rs 0 1 l
  (g, h) <- refB p r n
  f <~ implF g h
verifyL p n (LineP _ _ f (l, AndIT) rs) = do
  r <- getR rs 0 2 l
  s <- getR rs 1 2 l
  g <- refL p r n
  h <- refL p s n
  f <~ andF g h
verifyL p n (LineP _ _ f (l, AndE1T) rs) = do
  r <- getR rs 0 1 l
  g <- refL p r n
  g <~ andF f holeF </ (l, "Referenced by this rule")
verifyL p n (LineP _ _ f (l, AndE2T) rs) = do
  r <- getR rs 0 1 l
  g <- refL p r n
  g <~ andF holeF f </ (l, "Referenced by this rule")
verifyL p n (LineP _ _ f (l, OrI1T) rs) = do
  r <- getR rs 0 1 l
  g <- refL p r n
  f <~ orF g holeF
verifyL p n (LineP _ _ f (l, OrI2T) rs) = do
  r <- getR rs 0 1 l
  g <- refL p r n
  f <~ orF holeF g
verifyL p n (LineP _ _ f (l, OrET) rs) = do
  r <- getR rs 0 3 l
  s <- getR rs 1 3 l
  t <- getR rs 2 3 l
  (g, h) <- refB p r n
  (g', h') <- refB p s n
  i <- refL p t n
  i <~ orF g g' </ (l, "Referenced by this rule")
  f <~ h
  f <~ h'
verifyL p n (LineP _ _ f (l, NotIT) rs) = do
  r <- getR rs 0 1 l
  (g, h) <- refB p r n
  h <~ conF
  f <~ notF g
verifyL p n (LineP _ _ f (l, NotET) rs) = do
  r <- getR rs 0 2 l
  s <- getR rs 1 2 l
  g <- refL p r n
  h <- refL p s n
  f <~ conF
  h <~ notF g
verifyL p n (LineP _ _ f (l, ConET) rs) = do
  r <- getR rs 0 1 l
  g <- refL p r n
  g <~ conF
verifyL p n (LineP _ _ f (l, NotNotET) rs) = do
  r <- getR rs 0 1 l
  g <- refL p r n
  g <~ notF (notF f) </ (l, "Referenced by this rule")
verifyL _ _ (LineP _ _ _ (l, t) _) =
  Error [(l, "Rule not implemented: " ++ show t)]
