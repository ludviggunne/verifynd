module Main where

import Color
import Control.Monad (when)
import Parse (parse)
import Result (Result (..))
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO
import Text.Printf (hPrintf, printf)
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
printE :: (String -> String) -> String -> (Int, String) -> IO ()
printE col s (i, m) = do
  hPrintf stderr (dim "line %d:\n") n
  hPrintf stderr "%s %s\n" (dim "|") l
  hPrintf stderr "%s %s %s\n" (dim "|") (col $ pointer c l) $ col m
  where
    (n, c, l) = find s i'
    i' =
      if i < 0
        then length s - 1
        else i

-- Print error list
printEs :: (String -> String) -> String -> [(Int, String)] -> IO ()
printEs _ _ [] = return ()
printEs c s [h] = do
  printE c s h
printEs c s (h : t) = do
  printE c s h
  printEs yellow s t

parseAndVerify :: String -> Result ()
parseAndVerify s = do
  ts <- scan s
  when (null ts) $ Error [(-1, "Empty proof")]
  p <- parse ts
  verify p

getInputHandle :: IO Handle
getInputHandle = do
  args <- getArgs
  case args of
    (path : _) -> openFile path ReadMode
    _ -> return stdin

main :: IO ()
main = do
  handle <- getInputHandle
  src <- hGetContents handle
  when
    (null src)
    ( do
        printf "Empty proof\n"
        exitFailure
    )
  case parseAndVerify src of
    Error e -> do
      printEs red src e
      exitFailure
    _ -> exitSuccess
