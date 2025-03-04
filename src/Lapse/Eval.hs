{-# LANGUAGE LambdaCase #-}

module Lapse.Eval where

import Lapse.Scopes (getValue)
import Lapse.Types (Func, LapseM, Value (..))

lmap' :: (Monad m) => Func m -> Value m -> LapseM m (Value m)
lmap' f (Pair x y) = Pair <$> f x <*> lmap' f y
lmap' _ Nil = pure Nil
lmap' _ _ = error "Wrong lmap expression"

eval :: (Monad m) => Func m
eval p =
  case p of
    Nil -> pure Nil
    Name x -> getValue x
    Number _ -> pure p
    String _ -> pure p
    Function _ -> pure p
    Macros _ -> pure p
    Pair x args ->
      eval x >>= \case
        Macros m -> m args
        Function f -> lmap' eval args >>= f
        _ -> error $ "Can't eval this list: " ++ show p
