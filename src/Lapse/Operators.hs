{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Lapse.Operators where

import Control.Monad.State (get, lift, put)
import Data.Function (fix)
import Lapse.Eval (eval, lmap')
import Lapse.Scopes (changeValue, dropScope, newScope)
import Lapse.Types (Func, Value (..))

pureFunc :: (Value -> Value) -> Func
pureFunc = (pure .)

pureFunc' :: ((Value -> Value) -> Value -> Value) -> Func
pureFunc' = pureFunc . fix

ladd :: Func
ladd = pureFunc' \f -> \case
  Nil -> Number 0
  (Pair (Number a) b) ->
    Number
      ( a + case f b of
          Number x -> x
          _ -> undefined
      )
  _ -> undefined

lmul :: Func
lmul = pureFunc' \f -> \case
  Nil -> Number 1
  (Pair (Number a) b) ->
    Number
      ( a * case f b of
          Number x -> x
          _ -> undefined
      )
  _ -> undefined

lsub :: Func
lsub = pureFunc \case
  (Pair (Number a) (Pair (Number b) Nil)) -> Number $ a - b
  _ -> undefined

ldiv :: Func
ldiv = pureFunc \case
  (Pair (Number a) (Pair (Number b) Nil)) -> Number $ div a b
  _ -> undefined

lgrt :: Func
lgrt = pureFunc \case
  (Pair (Number a) (Pair (Number b) Nil)) -> if a > b then Number 1 else Nil
  _ -> undefined

llss :: Func
llss = pureFunc \case
  (Pair (Number a) (Pair (Number b) Nil)) -> if a < b then Number 1 else Nil
  _ -> undefined

leql :: Func
leql = pureFunc \case
  (Pair a (Pair b Nil)) -> if a == b then Number 1 else Nil
  _ -> undefined

lset :: Func
lset (Pair (Name k) (Pair v Nil)) = eval v >>= changeValue k >> pure Nil
lset _ = error "Wrong argument for set"

llet' :: Func
llet' (Pair (Pair (Pair (Name k) (Pair v Nil)) Nil) (Pair val Nil)) = do
  eval v >>= changeValue k
  eval val
llet' (Pair (Pair (Pair (Name k) (Pair v Nil)) other) c@(Pair _ Nil)) = do
  eval v >>= changeValue k
  llet' (Pair other c)
llet' _ = error "Wrong argument for let"

llet :: Func
llet v = newScope *> llet' v <* dropScope

cond :: Func
cond = \case
  Nil -> pure Nil
  (Pair (Pair c (Pair r Nil)) els) ->
    eval c >>= \case
      Nil -> cond els
      _ -> eval r
  _ -> undefined

lmap :: Func
lmap (Pair (Function f) (Pair oth Nil)) = lmap' f oth
lmap (Pair (Macros f) (Pair oth Nil)) = lmap' f oth
lmap _ = undefined

ldouble :: Func
ldouble (Number x) = pure $ Number $ x * 2
ldouble _ = undefined

llist :: Func
llist = pure

gensym :: Func
gensym Nil = lift get >>= \x -> lift $ put (x + 1) >> pure (Name (" sym" ++ show x))
gensym _ = undefined
