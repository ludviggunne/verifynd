{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module Parser (parse, Parser (..), expr) where

import Control.Applicative
import Error
import Expr
import Location
import Proof
import Ref
import Token

newtype Parser r = Parser
  { runParser :: [Token] -> Either [Error] (r, [Token])
  }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $ \s -> do
    (r, s') <- runParser p s
    pure (f r, s')

instance Applicative Parser where
  pure :: a -> Parser a
  pure v = Parser $ \s -> pure (v, s)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p <*> p' = Parser $ \s -> do
    (f, s') <- runParser p s
    (v, s'') <- runParser p' s'
    pure (f v, s')

instance Monad Parser where
  return = pure
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = Parser $ \s -> do
    (v, s') <- runParser p s
    runParser (f v) s'

addctx :: Parser r -> Error -> Parser r
p `addctx` e = Parser $ \s ->
  case runParser p s of
    Left es -> Left $ es ++ [e]
    ok -> ok

peek :: Parser Token
peek = Parser $ \toks@(tok : _) -> pure (tok, toks)

eat :: Parser ()
eat = Parser $ \(_ : rest) -> pure ((), rest)

err :: Location -> String -> Parser t
err loc msg = Parser $ \_ ->
  Left [ctx loc msg]

ctx :: Location -> String -> Error
ctx = Error

expect :: Tag -> Parser Token
expect tag = do
  tok <- peek
  if Token.tag tok == tag
    then do
      eat
      pure tok
    else case Token.tag tok of
      Eof -> err (Token.loc tok) $ "Unexpected end of input, expected " ++ show tag
      tag' -> err (Token.loc tok) $ "Unexpected " ++ show tag' ++ ", expected " ++ show tag

expr :: Parser Expr
expr = do
  lhs <- atom
  op <- peek
  if tag op `elem` [Token.Impl, Token.And, Token.Or]
    then do
      eat
      rhs <- atom `addctx` ctx (Token.loc op) "While parsing left hand side of this operator"
      let loc = Expr.loc lhs <> Expr.loc rhs
          ctor = case tag op of
            Token.Impl -> Expr.Impl
            Token.And -> Expr.And
            Token.Or -> Expr.Or
      pure $ ctor loc lhs rhs
    else pure lhs

atom :: Parser Expr
atom = do
  start <- peek
  case tag start of
    Token.Var -> var
    Token.LPar -> par
    Token.Neg -> neg
    Token.Con -> con
    Token.Wildcard -> wildcard
    _ -> err (Token.loc start) "Expected atomic expression"

var :: Parser Expr
var = do
  tok <- peek
  eat
  pure $ Expr.Var (Token.loc tok) (Token.str tok)

neg :: Parser Expr
neg = do
  tok <- peek
  eat
  exp <- atom
  pure $ Expr.Neg (Token.loc tok <> Expr.loc exp) exp

con :: Parser Expr
con = do
  tok <- peek
  eat
  pure $ Expr.Con (Token.loc tok)

wildcard :: Parser Expr
wildcard = do
  tok <- peek
  eat
  pure $ Expr.Wildcard (Token.loc tok)

par :: Parser Expr
par = do
  lpar <- peek
  eat
  exp <- expr
  rpar <- peek
  if Token.tag rpar /= Token.RPar
    then
      err (Token.loc lpar) "Missing closing parenthesis"
        `addctx` ctx (Expr.loc exp) "After this expression"
    else do
      eat
      pure exp

proof :: Parser [Entry]
proof = do
  first <- entry
  next <- peek
  case tag next of
    Eof -> pure [first]
    RBrc -> pure [first]
    _ -> (first :) <$> proof

entry :: Parser Entry
entry = do
  start <- peek
  case tag start of
    Num -> line
    LBrc -> Parser.box
    _ -> err (Token.loc start) "Invalid start of line/box"

line :: Parser Entry
line = do
  num <- expect Num
  exp <- expr
  rule <- Parser.rule
  refs <- Parser.refs
  semi <- expect Semi
  pure $
    Proof.Line
      (Token.loc num <> Token.loc semi)
      (read $ Token.str num)
      exp
      rule
      refs

box :: Parser Entry
box = do
  lbrc <- peek
  eat
  sub <- proof
  rbrc <- peek
  if tag rbrc /= RBrc
    then
      err (Token.loc lbrc) "Missing closing brace"
        `addctx` ctx (mconcat (map Proof.loc sub)) "After this subproof"
    else do
      eat
      pure $
        Proof.Box
          (Token.loc lbrc <> Token.loc rbrc)
          sub

rule :: Parser Token
rule = do
  tok <- peek
  if tag tok `elem` tags
    then do
      eat
      pure tok
    else err (Token.loc tok) "Expected rule"
  where
    tags :: [Tag]
    tags =
      [ Token.ImplE,
        Token.ImplI,
        Token.AndE1,
        Token.AndE2,
        Token.AndI,
        Token.OrE,
        Token.OrI1,
        Token.OrI2,
        Token.NegI,
        Token.NegE,
        Token.NegNegE,
        Token.ConE,
        Token.Prem,
        Token.Assum,
        Token.Lem,
        Token.Pbc,
        Token.Mt,
        Token.Copy
      ]

refs :: Parser [Ref]
refs = do
  tok <- peek
  if tag tok /= Num
    then pure []
    else do
      ref <- ref
      tok <- peek
      if tag tok == Token.Comma
        then do
          eat
          (ref :) <$> Parser.refs
        else pure [ref]

ref :: Parser Ref
ref = do
  start <- expect Num
  tok <- peek
  if tag tok == Token.Hyphen
    then do
      eat
      end <- expect Num
      pure $
        Ref.Box
          (Token.loc start <> Token.loc end)
          (read $ Token.str start)
          (read $ Token.str end)
    else
      pure $
        Ref.Line
          (Token.loc start)
          (read $ Token.str start)

parse :: [Token] -> Either [Error] [Entry]
parse tokens = fst <$> runParser proof tokens
