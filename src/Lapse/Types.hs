module Lapse.Types where

import Control.Monad.State (StateT)
import Data.Char (isControl, isSpace)
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map)

type Scope m = Map String (Value m)
type Scopes m = [Scope m]

type Func m = (Value m -> LapseM Identity (Value m))

data Value m
  = Nil
  | Number Int
  | Name String
  | Pair (Value m) (Value m)
  | String String
  | Function (Func m)
  | Macros (Func m)
  deriving (Eq)

instance Eq (Func m) where
  _ == _ = error "Can't compare functions"

show' :: Value m -> String
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

instance Show (Value m) where
  show Nil = "()"
  show (Number n) = show n
  show (Name s) = if all isIdent s then s else "#{" ++ s ++ "}#"
  show (String s) = show s
  show pr@(Pair _ _) = surround $ show' pr
  show (Function _) = "<function>"
  show (Macros _) = "<macros>"

surround :: String -> String
surround s = "(" ++ s ++ ")"

type LapseM m = StateT (Scopes m) (StateT Int m)
