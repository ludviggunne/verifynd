module Main where

import Apply (apply)
import Command (Command)
import qualified Command as C (Command (..), parse)
import Formula (Formula)
import qualified Formula as F (Formula (..), parse)
import Proof (Proof, proofLength)
import qualified Proof as P (Entry (..), Proof (..))
import System.IO (hFlush, stdout)
import Text.Printf
import Token (Token, tokenize)
import qualified Token as T (Token (..), tokenize)

handleLine :: Proof -> String -> Either String Proof
handleLine proof line = do
  tokens <- tokenize line
  command <- C.parse tokens
  case command of
    C.Open -> Left "Open is not implemented"
    C.Close -> Left "Close is not implemented"
    C.Presume formula -> Right $ proof ++ [P.Line command formula]
    C.Assume formula -> Left "Assume is not implemented"
    C.Apply rule -> do
      result <- apply rule proof
      Right $ proof ++ [P.Line command result]

loop :: Proof -> IO ()
loop proof = do
  hPrintf stdout "%d. " $ 1 + proofLength proof
  hFlush stdout
  line <- getLine
  case handleLine proof line of
    Left error -> do
      hPrintf stdout "\x1b[1F\x1b[2KError: %s (press ENTER to continue)" error
      hFlush stdout
      _ <- getLine
      hPrintf stdout "\x1b[1F\x1b[2K\r"
      hFlush stdout
      loop proof
    Right newProof -> do
      hPrintf stdout "Result: %s\n" (show $ last newProof)
      loop newProof

main :: IO ()
main = loop []
