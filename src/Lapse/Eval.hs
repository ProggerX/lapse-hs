{-# LANGUAGE LambdaCase #-}

module Lapse.Eval where

import Lapse.Scopes (getValue)
import Lapse.Types (Func, ScopeM, Value (..))

lmap' :: Func -> Value -> ScopeM Value
lmap' f (Pair x y) = do
  fx <- f x
  lmapfy <- lmap' f y
  pure (Pair fx lmapfy)
lmap' _ Nil = pure Nil
lmap' _ _ = error "Wrong lmap expression"

eval :: Func
eval = \case
  Nil -> pure Nil
  Name x -> getValue x
  x@(Number _) -> pure x
  x@(String _) -> pure x
  x@(Function _) -> pure x
  x@(Macros _) -> pure x
  p@(Pair x args) ->
    eval x >>= \case
      (Macros m) -> m args
      (Function f) -> lmap' eval args >>= f
      _ -> error $ "Can't eval this list: " ++ show p
