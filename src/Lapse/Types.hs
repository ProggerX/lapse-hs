module Lapse.Types where

import Control.Monad.State (State)
import Data.Map.Strict (Map)

type Scope = Map String Value
type Scopes = [Scope]

data Value
  = Nil
  | Number Int
  | Name String
  | Pair Value Value
  | Function (Value -> ScopeM Value)

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

instance Eq Value where
  (==) (Function _) = undefined
  (==) x = (==) x

surround :: String -> String
surround s = "(" ++ s ++ ")"

type ScopeM a = State Scopes a
