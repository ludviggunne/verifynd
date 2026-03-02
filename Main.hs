module Main where

import Parse (parse)
import Proof (PEntry)
import Result
import Text.Printf
import Token
import Verify (verify)

loop :: [Tok] -> IO ()
loop [] = return ()
loop (hd : tl) = do
  print hd
  loop tl

enum :: [String] -> [(Int, Int, String)]
enum = impl 1 0
  where
    impl :: Int -> Int -> [String] -> [(Int, Int, String)]
    impl _ _ [] = []
    impl n c (hd : tl) =
      (n, c, hd) : impl (n + 1) (1 + c + length hd) tl

find :: String -> Int -> (Int, Int, String)
find s = impl $ enum (lines s)
  where
    impl :: [(Int, Int, String)] -> Int -> (Int, Int, String)
    impl [] _ = error "invalid offset"
    impl ((n, c, s) : tl@((n', c', s') : tl')) i
      | c' <= i = impl tl i
      | otherwise = (n, i - c, s)
    impl [(n, c, s)] i = (n, i - c, s)

pointer :: Int -> String -> String
pointer 1 _ = "^"
pointer c ('\t' : tl) =
  "\t" ++ pointer (c - 1) tl
pointer c (_ : tl) =
  " " ++ pointer (c - 1) tl
pointer c l = error $ show c ++ " " ++ show l

printError :: String -> (Int, String) -> IO ()
printError s (i, m) = do
  printf "line %d:\n" n
  printf "%s\n" l
  printf "\x1b[31m%s %s\x1b[0m\n" (pointer c l) m
  where
    (n, c, l) = find s i'
    i' =
      if i < 0
        then length s - 1
        else i

parseAndVerify :: String -> Result ()
parseAndVerify s = do
  ts <- scan s
  p <- parse ts
  verify p

main :: IO ()
main = do
  src <- getContents
  case parseAndVerify src of
    Error e -> printError src e
    _ -> return ()
