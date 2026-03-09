module Main where

import Error
import Expr
import Location
import Parser
import Proof
import System.Environment (getArgs)
import Text.Printf
import Token
import Verify

process :: String -> Either [Error] ()
process s = do
  tokens <- tokenize 0 s
  proof <- parse tokens
  verify proof

run :: String -> IO ()
run path = do
  str <- readFile path
  case process str of
    Right exp -> pure ()
    Left errs -> mapM_ p errs
      where
        p :: Error -> IO ()
        p err = do
          printf "\n"
          printf "%s:\n" $ msg err
          printf "%s" $ concatMap ("\t" ++) $ lines $ highlight str (Error.loc err)
          printf "\n"

main :: IO ()
main = do
  args <- getArgs
  run $ head args
