module Result where

data Result t
  = Ok t
  | Error (Int, String)
  deriving (Eq)

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
