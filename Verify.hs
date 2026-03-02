module Verify where

import Annotation (Annotation (..), Reference (..), Rule (..))
import Formula (Formula (..), (~=))
import Proof (Proof (..), len)
import qualified Proof as P (ref, refBox)
import Result (Result (..), errMsg, wrapErr)

ref :: Proof -> Int -> Int -> Result Formula
ref p r n = wrapErr s $ P.ref p r n
  where
    s = "reference " ++ show r

refBox :: Proof -> (Int, Int) -> Int -> Result (Formula, Formula)
refBox p r n = wrapErr s $ P.refBox p r n
  where
    s = "reference " ++ show (fst r) ++ "-" ++ show (snd r)

fromRef :: Reference -> Result t -> Result t
fromRef r = wrapErr ("from reference " ++ show r)

verify :: Proof -> Proof -> Int -> Result ()
verify p (Line f a) n =
  case a of
    Premise -> Ok ()
    Assumption -> Ok ()
    Rule r rs -> verifyRn p f n r rs
verify p (Box (hd : tl)) n = do
  verify p hd n
  verify p (Box tl) (n + len hd)
verify _ (Box []) _ = Ok ()

verifyRn :: Proof -> Formula -> Int -> Rule -> [Reference] -> Result ()
verifyRn p f n r rs =
  wrapErr ("line " ++ show n ++ ", in application of rule " ++ show r) $
    verifyR p f n r rs

verifyR :: Proof -> Formula -> Int -> Rule -> [Reference] -> Result ()
verifyR p f n ImplIntr [BoxRef r] = do
  (a, b) <- refBox p r n
  f ~= Impl a b
verifyR p f n ImplElim [LineRef r1, LineRef r2] = do
  f1 <- ref p r1 n
  f2 <- ref p r2 n
  f1 ~= Impl f2 f
verifyR p (Conj a b) n ConjIntr [LineRef r1, LineRef r2] = do
  a' <- ref p r1 n
  b' <- ref p r2 n
  a ~= a'
  b ~= b'
verifyR p f n ConjElim1 [LineRef r] = do
  f' <- ref p r n
  f' ~= Conj f Wildcard
verifyR p f n ConjElim2 [LineRef r] = do
  f' <- ref p r n
  f' ~= Conj Wildcard f
verifyR p (Disj a b) n DisjIntr1 [LineRef r] = do
  a' <- ref p r n
  a ~= a'
verifyR p (Disj a b) n DisjIntr2 [LineRef r] = do
  b' <- ref p r n
  b ~= b'
verifyR p f n DisjElim [BoxRef r1, BoxRef r2, LineRef r3] = do
  (l, f') <- refBox p r1 n
  (r, f'') <- refBox p r2 n
  o <- ref p r3 n
  o ~= Disj l r
  f' ~= f
  f'' ~= f
verifyR p (Neg f) n NegIntr [BoxRef r] = do
  (f', c) <- refBox p r n
  f ~= f'
  c ~= Con
verifyR p Con n NegElim [LineRef r1, LineRef r2] = do
  l1 <- ref p r1 n
  l2 <- ref p r2 n
  l1 ~= Neg l2
verifyR p (Disj f f') _ Lem [] = do
  f' ~= Neg f
verifyR p f n Pbc [BoxRef r] = do
  (f', c) <- refBox p r n
  f ~= Neg f'
  c ~= Con
verifyR _ _ _ r _ =
  errMsg $ "invalid application of " ++ show r
