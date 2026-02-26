module Apply where

import Formula (Formula)
import qualified Formula as F (Formula (..))
import Proof (Proof)
import qualified Proof as P (Entry (..), ref)
import Rule (Rule)
import qualified Rule as R (Input (..), Kind (..), Rule (..))
import Text.Printf
import Token (Token)
import qualified Token as T (Token (..))

invalidInputs :: [R.Input] -> Either String t
invalidInputs inputs = Left $ printf "Invalid inputs: %s" (show inputs)

apply :: Rule -> Proof -> Either String Formula
apply (R.AndIntr, [lref, rref]) proof =
  do
    lhs <- P.ref proof lref
    rhs <- P.ref proof rref
    case (lhs, rhs) of
      (P.Line _ lhs, P.Line _ rhs) -> Right $ F.Binop T.And lhs rhs
      _ -> invalidInputs [lref, rref]
apply (R.AndElim1, [ref]) proof =
  do
    line <- P.ref proof ref
    case line of
      (P.Line _ (F.Binop T.And lhs _)) -> Right lhs
      _ -> invalidInputs [ref]
apply (R.AndElim2, [ref]) proof =
  do
    line <- P.ref proof ref
    case line of
      (P.Line _ (F.Binop T.And _ rhs)) -> Right rhs
      _ -> invalidInputs [ref]
apply (R.OrIntr1, [ref, R.Formula rhs]) proof =
  do
    line <- P.ref proof ref
    case line of
      (P.Line _ lhs) -> Right $ F.Binop T.Or lhs rhs
      _ -> invalidInputs [ref, R.Formula rhs]
apply (R.OrIntr2, [R.Formula lhs, ref]) proof =
  do
    line <- P.ref proof ref
    case line of
      (P.Line _ rhs) -> Right $ F.Binop T.Or lhs rhs
      _ -> invalidInputs [R.Formula lhs, ref]
apply (R.OrElim1, [ref]) proof =
  do
    line <- P.ref proof ref
    case line of
      (P.Line _ (F.Binop T.Or lhs _)) -> Right lhs
      _ -> invalidInputs [ref]
apply (R.OrElim2, [ref]) proof =
  do
    line <- P.ref proof ref
    case line of
      (P.Line _ (F.Binop T.Or _ rhs)) -> Right rhs
      _ -> invalidInputs [ref]
apply (R.NotElim, [yesRef, noRef]) proof =
  do
    yesLine <- P.ref proof yesRef
    noLine <- P.ref proof noRef
    case (yesLine, noLine) of
      (P.Line _ yes, P.Line _ (F.Not no)) ->
        if yes == no
          then Right F.Con
          else invalidInputs [yesRef, noRef]
      _ -> invalidInputs [yesRef, noRef]
