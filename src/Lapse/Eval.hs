{-# LANGUAGE LambdaCase #-}

module Lapse.Eval where

import Lapse.Scopes (getValue)
import Lapse.Types (Func, ScopeM, Value (..))

lmap :: Func -> Value -> ScopeM Value
lmap f (Pair x y) = do
  fx <- f x
  lmapfy <- lmap f y
  pure (Pair fx lmapfy)
lmap _ Nil = pure Nil
lmap _ _ = error "Wrong lmap expression"

eval :: Func
eval = \case
  Nil -> pure Nil
  n@(Number _) -> pure n
  Name x -> getValue x
  (Pair x args) ->
    eval x >>= \case
      (Macros m) -> m args
      (Function f) -> lmap eval args >>= f
      _ -> error $ "Can't eval this list: " ++ show (Pair x args)
  _ -> error "Can't eval an expression"
