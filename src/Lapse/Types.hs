module Lapse.Types where

import Control.Monad.State (State, StateT)
import Data.Char (isControl, isSpace)
import Data.Map.Strict (Map)

type Scope = Map String Value
type Scopes = [Scope]

type Func = (Value -> ScopeM Value)

data Value
  = Nil
  | Number Int
  | Name String
  | Pair Value Value
  | String String
  | Function Func
  | Macros Func
  deriving (Eq)

instance Eq Func where
  _ == _ = error "Can't compare functions"

show' :: Value -> String
show' (Pair a@(Pair _ _) Nil) = surround $ show' a
show' (Pair a Nil) = show' a
show' (Pair a b) =
  show a ++ " " ++ f b
 where
  f x@(Pair _ _) = show' x
  f x = ". " ++ show x
show' x = show x

isIdent :: Char -> Bool
isIdent x
  | isSpace x = False
  | isControl x = False
  | otherwise = case x of
      '(' -> False
      ')' -> False
      '[' -> False
      ']' -> False
      '{' -> False
      '}' -> False
      _ -> True

instance Show Value where
  show Nil = "()"
  show (Number n) = show n
  show (Name s) = if all isIdent s then s else "#{" ++ s ++ "}#"
  show (String s) = show s
  show pr@(Pair _ _) = surround $ show' pr
  show (Function _) = "<function>"
  show (Macros _) = "<macros>"

surround :: String -> String
surround s = "(" ++ s ++ ")"

type Counter = State Int

type ScopeM a = StateT Scopes Counter a
