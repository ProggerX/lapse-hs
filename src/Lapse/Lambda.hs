{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ViewPatterns #-}

module Lapse.Lambda where

import Control.Lens ((.=))
import Control.Monad ((<=<))
import Control.Monad.State (get, gets, put)
import Data.Map.Strict (fromList)
import Lapse.Eval (eval)
import Lapse.Operators (lset)
import Lapse.Types (Env (Env), Func, LapseM, Scopes, Value (..))
import Lapse.Types qualified

unList :: Value m -> [Value m]
unList Nil = []
unList (Pair h t) = h : unList t
unList _ = error "Wrong lambda expression"

inScopes :: (Monad m) => Scopes m -> Value m -> LapseM m (Value m)
inScopes ss v = do
  oldScopes <- get
  #scopes .= ss
  res <- eval v
  put oldScopes
  pure res

unName :: Value m -> String
unName (Name s) = s
unName v = error $ "Expected name, but got: " ++ show v

mkFunction :: (Monad m) => [Value m] -> Value m -> LapseM m (Func m)
mkFunction argsN' expr = gets f
 where
  argsN = map unName argsN'
  f Env{scopes} args = do
    Env{scopes = innerScopes} <- get
    let argsV = unList args
        scopes' = fromList (zip argsN argsV) : scopes ++ innerScopes
    inScopes scopes' expr

lambda :: (Monad m) => Func m
lambda (unList -> [argN, expr]) =
  let argN' = unList argN
   in Function <$> mkFunction argN' expr
lambda _ = error "Wrong lambda expression"

define :: (Monad m) => Func m
define (Name fname `Pair` ls@(_ `Pair` _ `Pair` Nil)) = do
  x <- lambda ls
  lset $ Pair (Name fname) (Pair x Nil)
define _ = error "Wrong define expression"

macro :: (Monad m) => Func m
macro (unList -> [argN, expr]) =
  let argN' = unList argN
   in do
        m <- mkFunction argN' expr
        pure $ Macros $ eval <=< m
macro _ = error "Wrong macro expression"

defmacro :: (Monad m) => Func m
defmacro (Name fname `Pair` ls@(_ `Pair` _ `Pair` Nil)) = do
  x <- macro ls
  lset $ Pair (Name fname) (Pair x Nil)
defmacro _ = error "Wrong defmacro expression"
