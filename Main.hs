module Main where

import Apply (apply)
import Command (Command)
import qualified Command as C (Command (..), parse)
import Formula (Formula)
import qualified Formula as F (Formula (..), parse)
import Proof (Proof, lastLine, proofInsert, proofLength)
import qualified Proof as P (Entry (..), Proof (..))
import System.IO (hFlush, stdout)
import Text.Printf
import Token (Token, tokenize)
import qualified Token as T (Token (..), tokenize)

clear :: String
clear = "\x1b[1F\x1b[2K"

printError :: String -> IO ()
printError error = do
  hPrintf stdout (clear ++ "   Error: " ++ error ++ " (Press ENTER to continue)")
  hFlush stdout
  _ <- getLine
  hPrintf stdout clear
  hFlush stdout

printLast :: Int -> Proof -> IO ()
printLast depth proof =
  let (command, formula) = lastLine proof
   in hPrintf
        stdout
        (clear ++ "%3d. %s%-60s%s\n")
        (proofLength proof)
        (concat $ replicate depth "┃ ")
        (show formula)
        (show command)

loop :: Int -> Proof -> IO ()
loop depth proof = do
  hPrintf
    stdout
    "%3d. %s "
    (1 + proofLength proof)
    (concat $ replicate depth "┃")
  hFlush stdout
  line <- getLine
  case tokenize line >>= C.parse of
    Left error ->
      do
        printError error
        loop depth proof
    Right command -> case command of
      C.Exit ->
        return ()
      C.Open ->
        do
          hPrintf stdout clear
          loop (depth + 1) (proofInsert proof depth (P.Box []))
      C.Close ->
        if depth > 0
          then do
            hPrintf stdout clear
            loop (depth - 1) proof
          else do
            printError "No box to close"
            loop depth proof
      C.Presume formula ->
        if depth == 0
          then
            let newProof = proofInsert proof depth (P.Line command formula)
             in do
                  printLast depth newProof
                  loop depth newProof
          else do
            printError "Premises may only be introduced in bottom level proof"
            loop depth proof
      C.Assume formula ->
        if depth > 0
          then
            let newProof = proofInsert proof depth (P.Line command formula)
             in do
                  printLast depth newProof
                  loop depth newProof
          else do
            printError "Assumptions may only be introduced in boxes"
            loop depth proof
      C.Apply rule ->
        case apply rule proof of
          Left error ->
            do
              printError error
              loop depth proof
          Right formula ->
            let newProof = proofInsert proof depth (P.Line command formula)
             in do
                  printLast depth newProof
                  loop depth newProof

main :: IO ()
main = loop 0 []
