module Lapse.Operators where

import Lapse.Types (Func, Value (..))

ladd' :: Value -> Value
ladd' Nil = Number 0
ladd' (Pair (Number a) b) =
  Number
    ( a + case ladd' b of
        Number x -> x
        _ -> undefined
    )
ladd' _ = undefined

ladd :: Func
ladd = pure . ladd'

lmul' :: Value -> Value
lmul' Nil = Number 1
lmul' (Pair (Number a) b) =
  Number
    ( a * case lmul' b of
        Number x -> x
        _ -> undefined
    )
lmul' _ = undefined

lmul :: Func
lmul = pure . lmul'

lsub' :: Value -> Value
lsub' (Pair (Number a) (Pair (Number b) Nil)) = Number $ a - b
lsub' _ = undefined

lsub :: Func
lsub = pure . lsub'

ldiv' :: Value -> Value
ldiv' (Pair (Number a) (Pair (Number b) Nil)) = Number $ div a b
ldiv' _ = undefined

ldiv :: Func
ldiv = pure . ldiv'

lgrt' :: Value -> Value
lgrt' (Pair (Number a) (Pair (Number b) Nil)) = if a > b then Number 1 else Nil
lgrt' _ = undefined

lgrt :: Func
lgrt = pure . lgrt'

llss' :: Value -> Value
llss' (Pair (Number a) (Pair (Number b) Nil)) = if a < b then Number 1 else Nil
llss' _ = undefined

llss :: Func
llss = pure . llss'

leql' :: Value -> Value
leql' (Pair a (Pair b Nil)) = if a == b then Number 1 else Nil
leql' _ = undefined

leql :: Func
leql = pure . leql'
