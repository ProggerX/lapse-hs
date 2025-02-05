{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Lapse.Operators where

import Lapse (impureFunc, impureFunc')
import Lapse.Types (Func, Value (..))

ladd :: Func
ladd = impureFunc' \f -> \case
  Nil -> Number 0
  (Pair (Number a) b) ->
    Number
      ( a + case f b of
          Number x -> x
          _ -> undefined
      )
  _ -> undefined

lmul :: Func
lmul = impureFunc' \f -> \case
  Nil -> Number 1
  (Pair (Number a) b) ->
    Number
      ( a * case f b of
          Number x -> x
          _ -> undefined
      )
  _ -> undefined

lsub :: Func
lsub = impureFunc \case
  (Pair (Number a) (Pair (Number b) Nil)) -> Number $ a - b
  _ -> undefined

ldiv :: Func
ldiv = impureFunc \case
  (Pair (Number a) (Pair (Number b) Nil)) -> Number $ div a b
  _ -> undefined

lgrt :: Func
lgrt = impureFunc \case
  (Pair (Number a) (Pair (Number b) Nil)) -> if a > b then Number 1 else Nil
  _ -> undefined

llss :: Func
llss = impureFunc \case
  (Pair (Number a) (Pair (Number b) Nil)) -> if a < b then Number 1 else Nil
  _ -> undefined

leql :: Func
leql = impureFunc \case
  (Pair a (Pair b Nil)) -> if a == b then Number 1 else Nil
  _ -> undefined
