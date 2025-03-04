{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}

module Lapse.Lambda where

import Control.Lens ((.=))
import Control.Monad ((<=<))
import Control.Monad.State (get, gets, put)
import Data.Map.Strict qualified as Map
import Lapse.Eval (eval)
import Lapse.Operators (lset)
import Lapse.Types (Env (Env), Func, LapseM, Scopes, Value (..))
import Lapse.Types qualified

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
  f Env{scopes} (List argsV) = do
    Env{scopes = innerScopes} <- get
    let scopes' = Map.fromList (zip argsN argsV) : scopes ++ innerScopes
    inScopes scopes' expr
  f _ _ = undefined

lambda :: (Monad m) => Func m
lambda (List [List argN, expr]) = Function <$> mkFunction argN expr
lambda _ = error "Wrong lambda expression"

define :: (Monad m) => Func m
define (Name fname `Pair` ls@(_ `Pair` _ `Pair` Nil)) = do
  x <- lambda ls
  lset $ Pair (Name fname) (Pair x Nil)
define _ = error "Wrong define expression"

macro :: (Monad m) => Func m
macro (List [List argN, expr]) = do
  m <- mkFunction argN expr
  pure $ Macros $ eval <=< m
macro _ = error "Wrong macro expression"

defmacro :: (Monad m) => Func m
defmacro (Name fname `Pair` ls@(_ `Pair` _ `Pair` Nil)) = do
  x <- macro ls
  lset $ List [Name fname, x]
defmacro _ = error "Wrong defmacro expression"
