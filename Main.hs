module Main where

import Proof
import Result
import Text.Printf
import Verify (verify)

parseAndVerify :: String -> Result ()
parseAndVerify input = do
  proof <- parse input
  verify proof proof 1

main :: IO ()
main = do
  input <- getContents
  case parseAndVerify input of
    Ok _ -> return ()
    e -> printf "%s\n" $ show e
