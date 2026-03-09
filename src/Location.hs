module Location (highlight, Location (..)) where

data Location = Location
  { start :: Int,
    end :: Int
  }
  deriving (Eq, Show)

instance Semigroup Location where
  a <> b = Location (min (start a) (start b)) (max (end a) (end b))

instance Monoid Location where
  mempty = Location maxBound minBound

dim, uline, red, rst, inv, bold, italic :: String
dim = "\x1b[2m"
uline = "\x1b[4:3;58:2::200:0:0m"
rst = "\x1b[0m"
red = "\x1b[41m"
inv = "\x1b[7m"
bold = "\x1b[1m"
italic = "\x1b[3m"

shift :: Location -> Int -> Location
shift loc l =
  loc
    { start = max 0 $ start loc - l,
      end = max 0 $ end loc - l
    }

hlineno :: Int -> String
hlineno i = dim ++ show i ++ ": " ++ rst

untab :: String -> String
untab = concatMap f
  where
    f '\t' = " "
    f c = [c]

hlines :: [(Int, String)] -> Location -> String
hlines [] _ = ""
hlines ((nr, str) : rest) loc
  | end loc == 0 = ""
  | start loc > length str = hlines rest (shift loc $ length str + 1)
  | otherwise =
      hlineno nr
        ++ untab (take (start loc) str)
        ++ uline
        ++ untab (take (end loc - start loc) (drop (start loc) str))
        ++ rst
        ++ untab (take (length str - end loc) (drop (end loc) str))
        ++ "\n"
        ++ hlines rest (shift loc $ length str + 1)

highlight :: String -> Location -> String
highlight str = hlines (zip [1 ..] $ lines str)
