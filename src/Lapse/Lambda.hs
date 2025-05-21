{-# LANGUAGE LambdaCase #-}

module Lapse.Lambda where

import Control.Monad ((<=<))
import Control.Monad.State (get, gets, put)
import Data.List (sort)
import Data.Map.Strict (empty, fromList)
import Lapse.Eval (eval)
import Lapse.Operators (lset)
import Lapse.Scopes (getValueM)
import Lapse.Types (Func, LapseM, Scopes, Value (..))

data UnList m = Proper [Value] | Improper ([Value], Value) | Single Value deriving (Show)

unList :: Value -> UnList m
unList Nil = Proper []
unList (Pair h t) = case unList t of
  Proper x -> Proper $ h : x
  Improper (x, y) -> Improper (h : x, y)
  Single x -> Improper ([h], x)
unList v = Single v

unList' :: Value -> [Value]
unList' x = case unList x of
  Proper a -> a
  _ -> error "Impossible"

inScopes :: Scopes -> Value -> LapseM Value
inScopes ss v = do
  oldScopes <- get
  put $ empty : ss
  res <- eval v
  s <- gets head
  put $ s : oldScopes
  pure res

unName :: Value -> String
unName (Name s) = s
unName v = error $ "Expected name, but got: " ++ show v

mkFunction :: Value -> Value -> LapseM Func
mkFunction argsN' expr = gets f
 where
  list = foldr Pair Nil
  f :: Scopes -> Func
  f ss args = do
    ns <- get
    let
      argsV = unList' args
      argsN'' = unList argsN'
     in
      case argsN'' of
        Proper argsN ->
          let a = map unName argsN
              ss' = fromList (zip a argsV) : ss ++ ns
           in inScopes ss' expr
        Improper (argsN, rest) ->
          let a = map unName argsN
              zipSS = fromList (zip a argsV) : ss
              ln = length argsV - length argsN
              ss' = zipSS ++ (fromList [(unName rest, list $ drop ln argsV)] : ns)
           in inScopes ss' expr
        Single v -> inScopes (fromList [(unName v, list argsV)] : ns) expr

lambda :: Func
lambda (Pair argN (Pair expr Nil)) =
  Function <$> mkFunction argN expr
lambda _ = error "Wrong lambda expression"

define :: Func
define (Pair (Name fname) ls@(Pair _ (Pair _ Nil))) = lambda ls >>= \x -> lset (Pair (Name fname) (Pair x Nil))
define _ = error "Wrong define expression"

macro :: Func
macro (Pair argN (Pair expr Nil)) = do
  m <- mkFunction argN expr
  pure $ Macros (eval <=< m)
macro _ = error "Wrong macro expression"

defmacro :: Func
defmacro (Pair (Name fname) ls@(Pair _ (Pair _ Nil))) = macro ls >>= \x -> lset (Pair (Name fname) (Pair x Nil))
defmacro _ = error "Wrong defmacro expression"

findFree :: Value -> LapseM [String]
findFree = \case
  Name x ->
    getValueM x >>= \case
      Just _ -> pure []
      Nothing -> pure [x]
  Pair x y -> concat <$> sequence [findFree x, findFree y]
  _ -> pure []

compact :: Func
compact v = do
  freeVars <- sort <$> findFree v
  let args = foldr (Pair . Name) Nil freeVars
  Function <$> mkFunction args v
