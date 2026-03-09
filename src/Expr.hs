module Expr where

import Location
import Text.Printf

data Expr
  = Impl
      { loc :: Location,
        lhs :: Expr,
        rhs :: Expr
      }
  | And
      { loc :: Location,
        lhs :: Expr,
        rhs :: Expr
      }
  | Or
      { loc :: Location,
        lhs :: Expr,
        rhs :: Expr
      }
  | Neg
      { loc :: Location,
        exp :: Expr
      }
  | Con
      { loc :: Location
      }
  | Var
      { loc :: Location,
        str :: String
      }
  | Wildcard
      { loc :: Location
      }

atomize :: Expr -> String
atomize exp@(Impl {}) = printf "(%s)" (show exp)
atomize exp@(And {}) = printf "(%s)" (show exp)
atomize exp@(Or {}) = printf "(%s)" (show exp)
atomize exp = show exp

instance Show Expr where
  show :: Expr -> String
  show exp@(Impl {}) = printf "%s -> %s" (atomize (lhs exp)) (atomize (rhs exp))
  show exp@(And {}) = printf "%s && %s" (atomize (lhs exp)) (atomize (rhs exp))
  show exp@(Or {}) = printf "%s || %s" (atomize (lhs exp)) (atomize (rhs exp))
  show exp@(Neg {}) = printf "!%s" (atomize (Expr.exp exp))
  show exp@(Con {}) = "?"
  show exp@(Wildcard {}) = "_"
  show exp@(Var {}) = str exp
