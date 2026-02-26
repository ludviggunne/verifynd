module Main where

import Apply (apply)
import Command (Command)
import qualified Command as C (Command (..), parse)
import Formula (Formula)
import qualified Formula as F (Formula (..), parse)
import Proof (Proof, lastLine, proofLength)
import qualified Proof as P (Entry (..), Proof (..))
import System.IO (hFlush, stdout)
import Text.Printf
import Token (Token, tokenize)
import qualified Token as T (Token (..), tokenize)

handleLine :: Int -> Proof -> String -> Either String (Int, Proof)
handleLine depth proof line = do
  tokens <- tokenize line
  command <- C.parse tokens
  case command of
    C.Open ->
      Right (depth + 1, proof)
    C.Close ->
      Right (depth - 1, proof)
    C.Exit ->
      Left "exit"
    C.Presume formula ->
      Right (depth, proof ++ [P.Line command formula])
    C.Assume formula ->
      Left "Assume is not implemented"
    C.Apply rule ->
      do
        result <- apply rule proof
        Right (depth, proof ++ [P.Line command result])

clear :: String
clear = "\x1b[1F\x1b[2K"

loop :: Int -> Proof -> IO ()
loop depth proof = do
  hPrintf stdout "%3d. %s" (1 + proofLength proof) (concat $ replicate depth "| ")
  hFlush stdout
  line <- getLine
  case handleLine depth proof line of
    Left "exit" ->
      return ()
    Left error ->
      do
        hPrintf stdout (clear ++ "\x1b[31mError: %s (press ENTER to continue)\x1b[0m") error
        hFlush stdout
        _ <- getLine
        hPrintf stdout (clear ++ "\r")
        hFlush stdout
        loop depth proof
    Right (newDepth, newProof) ->
      do
        hPrintf stdout (clear ++ "%3d. %-60s%s\n") len (show $ formula) (show $ command)
        loop newDepth newProof
      where
        len = proofLength newProof
        (command, formula) = lastLine newProof

main :: IO ()
main = loop 0 []
