module Color where

color :: Int -> String -> String
color x s = "\x1b[" ++ show x ++ "m" ++ s ++ "\x1b[0m"

red :: String -> String
red = color 31

green :: String -> String
green = color 32

yellow :: String -> String
yellow = color 33

blue :: String -> String
blue = color 34

dim :: String -> String
dim = color 2
