module Lapse.Types where

import Control.Monad.State (State)
import Data.Map.Strict (Map)

type Scope = Map String Value
type Scopes = [Scope]

type Func = (Value -> ScopeM Value)

data Value
  = Nil
  | Number Int
  | Name String
  | Pair Value Value
  | Function Func
  deriving (Eq)

instance Eq Func where
  _ == _ = error "Can't compare functions"

show' :: Value -> String
show' (Pair a Nil) = show' a
show' (Pair a b) =
  show a ++ " " ++ f b
 where
  f x@(Pair _ _) = show' x
  f x = ". " ++ show x
show' x = show x

instance Show Value where
  show Nil = "()"
  show (Number n) = show n
  show (Name s) = s
  show pr@(Pair _ _) = surround $ show' pr
  show (Function _) = "<function>"

surround :: String -> String
surround s = "(" ++ s ++ ")"

type ScopeM a = State Scopes a
