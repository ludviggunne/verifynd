module Apply where

import Formula (Formula)
import qualified Formula as F (Formula (..))
import Proof (Proof, lastLine)
import qualified Proof as P (Entry (..), ref)
import Rule (Inputs (..), Rule (..))
import qualified Rule as R (Input (..), Inputs (..), Kind (..), Rule (..))
import Text.Printf
import Token (Token)
import qualified Token as T (Token (..))

invalidInputs :: R.Inputs -> Either String t
invalidInputs inputs = Left $ printf "Invalid inputs: %s" (show inputs)

apply :: Rule -> Proof -> Either String Formula
apply (Rule R.AndIntr (Inputs [lref, rref])) proof =
  do
    lhs <- P.ref proof lref
    rhs <- P.ref proof rref
    case (lhs, rhs) of
      (P.Line _ lhs, P.Line _ rhs) -> Right $ F.Binop T.And lhs rhs
      _ -> invalidInputs $ Inputs [lref, rref]
apply (Rule R.AndElim1 (Inputs [ref])) proof =
  do
    line <- P.ref proof ref
    case line of
      (P.Line _ (F.Binop T.And lhs _)) -> Right lhs
      _ -> invalidInputs $ Inputs [ref]
apply (Rule R.AndElim2 (Inputs [ref])) proof =
  do
    line <- P.ref proof ref
    case line of
      (P.Line _ (F.Binop T.And _ rhs)) -> Right rhs
      _ -> invalidInputs $ Inputs [ref]
apply (Rule R.OrIntr1 (Inputs [ref, R.Formula rhs])) proof =
  do
    line <- P.ref proof ref
    case line of
      (P.Line _ lhs) -> Right $ F.Binop T.Or lhs rhs
      _ -> invalidInputs $ Inputs [ref, R.Formula rhs]
apply (Rule R.OrIntr2 (Inputs [R.Formula lhs, ref])) proof =
  do
    line <- P.ref proof ref
    case line of
      (P.Line _ rhs) -> Right $ F.Binop T.Or lhs rhs
      _ -> invalidInputs $ Inputs [R.Formula lhs, ref]
apply (Rule R.OrElim1 (Inputs [ref])) proof =
  do
    line <- P.ref proof ref
    case line of
      (P.Line _ (F.Binop T.Or lhs _)) -> Right lhs
      _ -> invalidInputs $ Inputs [ref]
apply (Rule R.OrElim2 (Inputs [ref])) proof =
  do
    line <- P.ref proof ref
    case line of
      (P.Line _ (F.Binop T.Or _ rhs)) -> Right rhs
      _ -> invalidInputs $ Inputs [ref]
apply (Rule R.NotElim (Inputs [yesRef, noRef])) proof =
  do
    yesLine <- P.ref proof yesRef
    noLine <- P.ref proof noRef
    case (yesLine, noLine) of
      (P.Line _ yes, P.Line _ (F.Not no)) ->
        if yes == no
          then Right F.Con
          else invalidInputs $ Inputs [yesRef, noRef]
      _ -> invalidInputs $ Inputs [yesRef, noRef]
apply (Rule R.ImplIntr (Inputs [ref])) proof =
  do
    box <- P.ref proof ref
    case box of
      (P.Box (head : tail)) -> Right $ F.Binop T.Impl lhs rhs
        where
          (P.Line _ lhs) = head
          (_, rhs) = lastLine tail
apply (Rule R.ImplElim (Inputs [lhsRef, implRef])) proof =
  do
    lhs <- P.ref proof lhsRef
    impl <- P.ref proof implRef
    case (lhs, impl) of
      (P.Line _ lhs, P.Line _ (F.Binop T.Impl implLhs implRhs)) ->
        if lhs == implLhs
          then Right implRhs
          else invalidInputs $ Inputs [lhsRef, implRef]
      _ -> invalidInputs $ Inputs [lhsRef, implRef]
apply (Rule _ inputs) proof =
  invalidInputs inputs
