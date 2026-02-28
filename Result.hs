module Result where

data Error
  = Message String
  | Empty
  deriving (Eq, Show)

(+/) :: String -> Error -> Error
(+/) str Empty = Message str
(+/) str (Message str') = Message $ str ++ ": " ++ str'

data Result t
  = Ok t
  | Error Error
  deriving (Eq)

instance Show (Result t) where
  show :: Result t -> String
  show (Error (Message msg)) = "error: " ++ msg

errMsg :: String -> Result t
errMsg msg = Error $ Message msg

err :: Result t
err = Error Empty

wrapErr :: String -> Result t -> Result t
wrapErr _ r@(Ok _) = r
wrapErr str (Error e) = Error $ str +/ e

notImplemented = errMsg "Not implemented"

instance Functor Result where
  fmap :: (a -> b) -> Result a -> Result b
  fmap f (Error e) = Error e
  fmap f (Ok v) = Ok $ f v

instance Applicative Result where
  (<*>) :: Result (a -> b) -> Result a -> Result b
  (<*>) (Ok f) (Ok v) = Ok $ f v
  (<*>) (Error e) _ = Error e
  (<*>) _ (Error e) = Error e
  pure = Ok

instance Monad Result where
  (>>=) :: Result a -> (a -> Result b) -> Result b
  (>>=) (Ok v) f = f v
  (>>=) (Error e) _ = Error e
