module Main where

import Parse (parse)
import Result (Result (..))
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)
import Token (scan)
import Verify (verify)

-- Associates each line with a line number and
-- a bytes offset in the input string
enum :: [String] -> [(Int, Int, String)]
enum = impl 1 0
  where
    impl :: Int -> Int -> [String] -> [(Int, Int, String)]
    impl _ _ [] = []
    impl n c (hd : tl) =
      (n, c, hd) : impl (n + 1) (1 + c + length hd) tl

-- Find which line a byte offset is contained in
find :: String -> Int -> (Int, Int, String)
find s = impl $ enum (lines s)
  where
    impl :: [(Int, Int, String)] -> Int -> (Int, Int, String)
    impl [] _ = error "invalid offset"
    impl ((n, c, s) : tl@((n', c', s') : tl')) i
      | c' <= i = impl tl i
      | otherwise = (n, i - c, s)
    impl [(n, c, s)] i = (n, i - c, s)

-- Point to a specific character on a line
pointer :: Int -> String -> String
pointer 0 _ = "^"
pointer c ('\t' : tl) =
  "\t" ++ pointer (c - 1) tl
pointer c (_ : tl) =
  " " ++ pointer (c - 1) tl
pointer c l = error $ show c ++ " " ++ show l

-- Print an error with context
printE :: String -> (Int, String) -> IO ()
printE s (i, m) = do
  printf "line %d:\n" n
  printf "| %s\n" l
  printf "| %s %s\n" (pointer c l) m
  where
    (n, c, l) = find s i'
    i' =
      if i < 0
        then length s - 1
        else i

-- Print error list
printEs :: String -> [(Int, String)] -> IO ()
printEs _ [] = return ()
printEs s [h] = do
  printE s h
printEs s (h : t) = do
  printE s h
  -- printf "\n"
  printEs s t

parseAndVerify :: String -> Result ()
parseAndVerify s = do
  ts <- scan s
  p <- parse ts
  verify p

main :: IO ()
main = do
  src <- getContents
  case parseAndVerify src of
    Error e -> do
      printEs src e
      exitFailure
    _ -> exitSuccess
