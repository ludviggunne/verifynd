module Error where

import Location

data Error = Error
  { loc :: Location,
    msg :: String
  }
  deriving (Show)
