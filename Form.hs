module Form where

import Color
import Control.Monad (void)
import Result
import Token (Tok)
import Types

-- Operator source offset, tag
type Form = (Loc, TagF)

data TagF
  = ImplF Form Form
  | AndF Form Form
  | OrF Form Form
  | NotF Form
  | ConF
  | VarF String
  | HoleF

par :: TagF -> String
par f = "(" ++ show f ++ ")"

atomize :: Form -> String
atomize (_, f@(ImplF _ _)) = par f
atomize (_, f@(AndF _ _)) = par f
atomize (_, f@(OrF _ _)) = par f
atomize (_, f) = show f

instance Show TagF where
  show :: TagF -> String
  show (VarF v) = v
  show (ImplF l r) = atomize l ++ " -> " ++ atomize r
  show (AndF l r) = atomize l ++ " && " ++ atomize r
  show (OrF l r) = atomize l ++ " || " ++ atomize r
  show (NotF f) = "!" ++ atomize f
  show ConF = "?"
  show HoleF = "_"

matchF :: Form -> Form -> Result [Form]
matchF f@(l, f') g@(_, g') = case impl f g of
  Error _ -> Error [(l, "'" ++ show f' ++ "' does not match expected '" ++ show g' ++ "'")]
  ok -> ok
  where
    impl :: Form -> Form -> Result [Form]
    impl (_, ImplF f g) (_, ImplF f' g') =
      (++) <$> impl f f' <*> impl g g'
    impl (_, AndF f g) (_, AndF f' g') =
      (++) <$> impl f f' <*> impl g g'
    impl (_, OrF f g) (_, OrF f' g') =
      (++) <$> impl f f' <*> impl g g'
    impl (_, VarF v) (_, VarF v')
      | v == v' = Ok []
      | otherwise = Error [(0, "")]
    impl (_, ConF) (_, ConF) =
      Ok []
    impl (_, NotF f) (_, NotF f') =
      impl f f'
    impl f (_, HoleF) =
      Ok [f]
    impl _ _ =
      Error [(0, "")]

(<<~) :: Form -> Form -> Result [Form]
(<<~) = matchF

(<~) :: Form -> Form -> Result ()
(<~) f g = void $ matchF f g
