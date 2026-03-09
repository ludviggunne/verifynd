module Verify where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Monad (guard, unless, void)
import Data.Functor (($>))
import Data.List (find, last)
import Error
import Expr
import Location
import Parser
import Proof
import Ref
import Text.Printf
import Token

data VerificationData = VerificationData
  { entry :: Entry,
    root :: [Entry]
  }

newtype Verification t = Verification
  { runVerification :: VerificationData -> Either [Error] t
  }

instance Functor Verification where
  fmap f v = Verification $ \vdata -> do
    result <- runVerification v vdata
    pure $ f result

instance Applicative Verification where
  pure result = Verification $ \_ -> pure result
  fv <*> av = Verification $ \vdata -> do
    f <- runVerification fv vdata
    a <- runVerification av vdata
    pure $ f a

instance Monad Verification where
  v >>= f = Verification $ \vdata -> do
    a <- runVerification v vdata
    runVerification (f a) vdata

instance Alternative Verification where
  empty = Verification $ const $ Left []
  lv <|> rv = Verification $ \vdata ->
    case runVerification lv vdata of
      Left le -> case runVerification rv vdata of
        Left re -> Left $ le <> re
        ok -> ok
      ok -> ok

abort :: Location -> String -> Verification t
abort loc msg = Verification $ const $ Left [Error loc msg]

exprContext :: Expr -> String -> Verification t
exprContext expr = abort (Expr.loc expr)

ruleContext :: String -> Verification t
ruleContext msg = getRule >>= flip abort msg . Token.loc

getData :: Verification VerificationData
getData = Verification pure

getEntry :: Verification Entry
getEntry = entry <$> getData

getRoot :: Verification [Entry]
getRoot = root <$> getData

getNum :: Verification Int
getNum = Proof.num <$> getEntry

getCurrent :: Verification Expr
getCurrent = Proof.exp <$> getEntry

getRule :: Verification Token
getRule = Proof.rule <$> getEntry

getRefs :: Verification [Ref]
getRefs = Proof.refs <$> getEntry

getRef :: Int -> Verification Ref
getRef i = flip (!!) i <$> getRefs

refCount :: Int -> Verification ()
refCount n = do
  refs <- getRefs
  rule <- getRule
  if n == length refs
    then pure ()
    else
      abort (Token.loc rule) $
        printf "Expected %d references, but %d were provided" n (length refs)

refLine :: Ref -> Verification Expr
refLine ref@(Ref.Box {}) = abort (Ref.loc ref) "Expected a line reference here"
refLine ref = do
  root <- getRoot
  impl (Ref.num ref) root <|> abort (Ref.loc ref) "No reachable line matches this reference"
  where
    impl :: Int -> [Entry] -> Verification Expr
    impl _ [] = empty
    impl n (first : rest) = do
      current <- getNum
      case first of
        Proof.Line {} ->
          if n == Proof.num first
            then pure $ Proof.exp first
            else impl n rest
        Proof.Box {} -> do
          let entries = Proof.entries first
          if entries `contains` current
            then impl n entries <|> impl n rest
            else impl n rest

refBox :: Ref -> Verification (Expr, Expr)
refBox ref@(Ref.Line {}) = abort (Ref.loc ref) "Expected a box reference here"
refBox ref = do
  root <- getRoot
  impl (Ref.start ref) (Ref.end ref) root <|> abort (Ref.loc ref) "No reachable box matches this reference"
  where
    impl :: Int -> Int -> [Entry] -> Verification (Expr, Expr)
    impl _ _ [] = empty
    impl start end (entry : rest) = do
      current <- getNum
      case entry of
        Proof.Line {} -> impl start end rest
        Proof.Box {} -> do
          let box = Proof.entries entry
              first = head box
              last = Data.List.last box
          if Proof.num first == start && Proof.num last == end
            then pure (Proof.exp first, Proof.exp last)
            else
              if box `contains` start && box `contains` end
                then impl start end box
                else impl start end rest

match :: Expr -> Expr -> Verification [Expr]
match lhs rhs =
  impl lhs rhs
    <|> abort (Expr.loc lhs) (printf "\x1b[1m%s\x1b[0m doesn't match \x1b[1m%s\x1b[0m" (show lhs) (show rhs))
  where
    impl :: Expr -> Expr -> Verification [Expr]
    impl lhs@(Expr.Impl {}) rhs@(Expr.Impl {}) =
      (<>)
        <$> match (Expr.lhs lhs) (Expr.lhs rhs)
        <*> match (Expr.rhs lhs) (Expr.rhs rhs)
    impl lhs@(Expr.And {}) rhs@(Expr.And {}) =
      (<>)
        <$> match (Expr.lhs lhs) (Expr.lhs rhs)
        <*> match (Expr.rhs lhs) (Expr.rhs rhs)
    impl lhs@(Expr.Or {}) rhs@(Expr.Or {}) =
      (<>)
        <$> match (Expr.lhs lhs) (Expr.lhs rhs)
        <*> match (Expr.rhs lhs) (Expr.rhs rhs)
    impl lhs@(Expr.Neg {}) rhs@(Expr.Neg {}) =
      match (Expr.exp lhs) (Expr.exp rhs)
    impl lhs@(Expr.Var {}) rhs@(Expr.Var {}) =
      if Expr.str lhs == Expr.str rhs
        then pure []
        else empty
    impl (Expr.Con {}) (Expr.Con {}) = pure []
    impl (Expr.Wildcard {}) rhs = pure [rhs]
    impl lhs (Expr.Wildcard {}) = pure [lhs]
    impl _ _ = empty

(=~) :: Expr -> Expr -> Verification [Expr]
(=~) = match

(~~) :: Expr -> Expr -> Verification ()
lhs ~~ rhs = lhs =~ rhs >> pure ()

template :: String -> Expr
template str =
  case Parser.runParser Parser.expr <$> tokenize 0 str of
    Right (Right (expr, _)) -> expr
    _ -> error "Syntax error in template"

verifyEntries :: [Entry] -> Verification ()
verifyEntries [] = pure ()
verifyEntries (entry : rest) = do
  verifyEntry entry
  verifyEntries rest

verifyEntry :: Entry -> Verification ()
verifyEntry (Proof.Box _ entries) = verifyEntries entries
verifyEntry entry = Verification $ \vdata ->
  let vdata' = vdata {entry = entry}
   in runVerification
        ( case tag $ rule entry of
            Token.Prem -> verifyPremise
            Token.Assum -> verifyAssumption
            Token.Copy -> verifyCopy
            Token.Lem -> verifyLem
            Token.Pbc -> verifyPbc
            Token.ImplI -> verifyImplI
            Token.ImplE -> verifyImplE
            Token.AndI -> verifyAndI
            Token.AndE1 -> verifyAndE1
            Token.AndE2 -> verifyAndE2
            Token.OrI1 -> verifyOrI1
            Token.OrI2 -> verifyOrI2
            Token.OrE -> verifyOrE
            Token.NegI -> verifyNegI
            Token.NegE -> verifyNegE
            Token.ConE -> verifyConE
            Token.NegNegE -> verifyNegNegE
            Token.Mt -> verifyMt
            _ -> abort (Token.loc $ rule entry) "Unhandled rule"
        )
        vdata'

-- TODO: Only in top level proof
verifyPremise :: Verification ()
verifyPremise = refCount 0

-- TODO: Only at beginning of subproofs
verifyAssumption :: Verification ()
verifyAssumption = refCount 0

verifyCopy :: Verification ()
verifyCopy = do
  refCount 1
  copied <- getRef 0 >>= refLine
  current <- getCurrent
  current ~~ copied <|> exprContext copied "Referenced expression"

verifyLem :: Verification ()
verifyLem = do
  refCount 0
  current <- getCurrent
  captures <- current =~ template "_ || !_"
  head captures ~~ last captures

verifyPbc :: Verification ()
verifyPbc = do
  refCount 1
  (start, end) <- getRef 0 >>= refBox
  current <- getCurrent
  start ~~ Expr.Neg mempty current <|> exprContext current "Constraining expression"
  end ~~ template "?" <|> ruleContext "Required by this rule"

verifyImplI :: Verification ()
verifyImplI = do
  refCount 1
  (start, end) <- getRef 0 >>= refBox
  current <- getCurrent
  captures <- (current =~ template "_ -> _") <|> ruleContext "Required by this rule"
  head captures ~~ start <|> exprContext start "Referenced expression"
  last captures ~~ end <|> exprContext end "Referenced expression"

verifyImplE :: Verification ()
verifyImplE = do
  refCount 2
  impl <- getRef 0 >>= refLine
  lhs <- getRef 1 >>= refLine
  current <- getCurrent
  captures <- (impl =~ template "_ -> _") <|> ruleContext "Required by this rule"
  let lhs' = head captures
      rhs' = last captures
  lhs ~~ lhs'
    <|> exprContext impl "Referenced expression"
    <|> ruleContext "From this rule"
  current ~~ rhs' <|> exprContext impl "Referenced expression"
  pure ()

verifyAndI :: Verification ()
verifyAndI = do
  refCount 2
  lhs <- getRef 0 >>= refLine
  rhs <- getRef 1 >>= refLine
  current <- getCurrent
  captures <- (current =~ template "_ && _") <|> ruleContext "Required by this rule"
  let currLhs = head captures
      currRhs = last captures
  currLhs ~~ lhs <|> exprContext lhs "Referenced expression"
  currRhs ~~ rhs <|> exprContext rhs "Referenced expression"

verifyAndE1 :: Verification ()
verifyAndE1 = do
  refCount 1
  expr <- getRef 0 >>= refLine
  captures <- expr =~ template "_ && _" <|> ruleContext "Required by this rule"
  let lhs = head captures
  current <- getCurrent
  current ~~ lhs <|> exprContext expr "Referenced expression"

verifyAndE2 :: Verification ()
verifyAndE2 = do
  refCount 1
  expr <- getRef 0 >>= refLine
  captures <- expr =~ template "_ && _" <|> ruleContext "Required by this rule"
  let rhs = last captures
  current <- getCurrent
  current ~~ rhs <|> exprContext expr "Referenced expression"

verifyOrI1 :: Verification ()
verifyOrI1 = do
  refCount 1
  expr <- getRef 0 >>= refLine
  current <- getCurrent
  captures <- current =~ template "_ || _" <|> ruleContext "Required by this rule"
  let lhs = head captures
  lhs ~~ expr <|> exprContext expr "Referenced expression"

verifyOrI2 :: Verification ()
verifyOrI2 = do
  refCount 1
  expr <- getRef 0 >>= refLine
  current <- getCurrent
  captures <- current =~ template "_ || _" <|> ruleContext "Required by this rule"
  let rhs = last captures
  rhs ~~ expr <|> exprContext expr "Referenced expression"

verifyOrE :: Verification ()
verifyOrE = do
  refCount 3
  (start1, end1) <- getRef 0 >>= refBox
  (start2, end2) <- getRef 1 >>= refBox
  or <- getRef 2 >>= refLine
  current <- getCurrent
  captures <- or =~ template "_ || _" <|> ruleContext "Required by this rule"
  let lhs = head captures
      rhs = last captures
  lhs ~~ start1 <|> exprContext start1 "Referenced expression" <|> ruleContext "From this rule"
  rhs ~~ start2 <|> exprContext start1 "Referenced expression" <|> ruleContext "From this rule"
  current ~~ end1 <|> exprContext end1 "Referenced expression"
  current ~~ end2 <|> exprContext end2 "Referenced expression"

verifyNegI :: Verification ()
verifyNegI = do
  refCount 1
  current <- getCurrent
  captures <- current =~ template "!_" <|> ruleContext "Required by this rule"
  (start, end) <- getRef 0 >>= refBox
  start ~~ head captures <|> exprContext current "From this expression"
  end ~~ template "?" <|> ruleContext "Required by this rule"

verifyNegE :: Verification ()
verifyNegE = do
  refCount 2
  current <- getCurrent
  current ~~ template "?" <|> ruleContext "Required by this rule"
  first <- getRef 0 >>= refLine
  second <- getRef 1 >>= refLine
  second ~~ Expr.Neg mempty first <|> ruleContext "Required by this rule"

verifyConE :: Verification ()
verifyConE = do
  refCount 1
  expr <- getRef 0 >>= refLine
  expr ~~ template "?" <|> ruleContext "Required by this rule"

verifyNegNegE :: Verification ()
verifyNegNegE = do
  refCount 1
  expr <- getRef 0 >>= refLine
  captures <- expr =~ template "!!_" <|> ruleContext "Required by this rule"
  current <- getCurrent
  let inner = head captures
  current ~~ inner <|> exprContext inner "Referenced expression"

verifyMt :: Verification ()
verifyMt = do
  refCount 2
  impl <- getRef 0 >>= refLine
  lhs <- getRef 1 >>= refLine
  current <- getCurrent
  implCaps <- impl =~ template "_ -> _" <|> ruleContext "Required by this rule"
  lhsCaps <- lhs =~ template "!_" <|> ruleContext "Required by this rule"
  currentCaps <- current =~ template "!_" <|> ruleContext "Required by this rule"
  head lhsCaps ~~ last implCaps <|> exprContext impl "From this expression" <|> ruleContext "Referenced by this rule"
  head currentCaps ~~ head implCaps <|> exprContext impl "From this expression" <|> ruleContext "Referenced by this rule"

verify :: [Entry] -> Either [Error] ()
verify proof = runVerification (verifyEntries proof) dummy
  where
    dummy =
      VerificationData
        { -- TODO: Make Verification generic in terms of underlying data
          --       Only use this for verifying entries
          entry = Proof.Box mempty [],
          root = proof
        }
