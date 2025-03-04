{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Lapse.Types (
  Env (..),
  Func,
  LapseM,
  Scope,
  Scopes,
  Value (List, ..),
  unwrapListUnsafe,
) where

import Control.Monad.State (StateT)
import Data.Char (isControl, isSpace)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)

type Scope m = Map String (Value m)
type Scopes m = [Scope m]

type Func m = (Value m -> LapseM m (Value m))

data Value m
  = Nil
  | Number Int
  | Name String
  | Pair (Value m) (Value m)
  | String String
  | Function (Func m)
  | Macros (Func m)
  deriving (Eq)

infixr 9 `Pair`

wrapList :: [Value m] -> Value m
wrapList = foldr Pair Nil

unwrapList :: Value m -> Maybe [Value m]
unwrapList = \case
  Nil -> Just []
  Pair a b -> (a :) <$> unwrapList b
  _ -> Nothing

unwrapListUnsafe :: (HasCallStack) => Value m -> [Value m]
unwrapListUnsafe = fromMaybe (error "value is not a list") . unwrapList

pattern List :: [Value m] -> Value m
pattern List xs <- (unwrapList -> Just xs) where List = wrapList

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

data Env m = Env {scopes :: Scopes m, counter :: Int}
  deriving (Generic)

type LapseM m = StateT (Env m) m
