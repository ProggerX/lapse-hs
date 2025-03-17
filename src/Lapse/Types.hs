{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Lapse.Types where

import Control.Monad.State (StateT)
import Data.Aeson qualified as A
import Data.Char (isControl, isSpace)
import Data.Map.Strict (Map, assocs)
import Data.Text qualified as T
import Data.Typeable (Typeable, cast)
import Data.Vector (toList)
import GHC.Generics (Generic)

type Scope m = Map String (Value m)
type Scopes m = [Scope m]

type Func m = (Value m -> LapseM m (Value m))

data TBox = forall a. (Typeable a, Show a, Eq a) => TBox a

data Value m
  = Nil
  | Number Int
  | Name String
  | Pair (Value m) (Value m)
  | String String
  | Dict (Map String (Value m))
  | Function (Func m)
  | Macros (Func m)
  | External TBox
  deriving (Eq, Generic)

instance Eq (Func m) where
  _ == _ = error "Can't compare functions"

instance A.ToJSON (Func m) where
  toJSON _ = error "Can't pack a function to JSON"

instance Eq TBox where
  (TBox a) == (TBox b) =
    case cast a of
      Just a' -> a' == b
      Nothing -> False

instance A.ToJSON TBox where
  toJSON (TBox a) = A.toJSON $ show a -- NOTE: Can cause problems in future

ext :: (Typeable a, Show a, Eq a) => a -> Value m
ext = External . TBox

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
  show (External (TBox a)) = show a
  show (Dict m) =
    let f (k, v) = (++) " " $ show $ foldr Pair Nil [String k, v]
     in "(dict" ++ concatMap f (assocs m) ++ ")"

surround :: String -> String
surround s = "(" ++ s ++ ")"

type LapseM m = StateT (Scopes m) (StateT Int m)

data UnList m = Proper [Value m] | Improper ([Value m], Value m) | Single (Value m) deriving (Show)

unList :: Value m -> UnList m
unList Nil = Proper []
unList (Pair h t) = case unList t of
  Proper x -> Proper $ h : x
  Improper (x, y) -> Improper (h : x, y)
  Single x -> Improper ([h], x)
unList v = Single v

instance A.ToJSON (Value m) where
  toJSON v = case unList v of
    Proper l -> A.toJSON l
    Improper l -> A.toJSON l
    Single x -> case x of
      Number n -> A.toJSON n
      String s -> A.toJSON s
      Name s -> A.toJSON s
      Function f -> A.toJSON f
      Macros m -> A.toJSON m
      Nil -> A.Null
      Dict m -> A.toJSON m
      External t -> A.toJSON t
      _ -> undefined

unsafeFromJSON :: (A.FromJSON a) => A.Value -> a
unsafeFromJSON v = case A.fromJSON v of
  A.Success a -> a
  A.Error err -> error $ "JSON error: " ++ err

instance A.FromJSON (Value m) where
  parseJSON = \case
    A.Null -> pure Nil
    A.Number n -> pure $ Number (floor n)
    A.String s -> pure $ String (T.unpack s)
    A.Bool b -> pure $ if b then Number 1 else Number 0
    A.Array arr -> pure $ foldr (Pair . unsafeFromJSON) Nil $ toList arr
    A.Object obj -> Dict <$> A.parseJSON (A.Object obj)
