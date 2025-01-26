module Lapse.Operators where

import Lapse (Value (..))

ladd :: Value -> Value
ladd Nil = Number 0
ladd (Pair (Number a) b) =
  Number
    ( a + case ladd b of
        Number x -> x
        _ -> undefined
    )
ladd _ = undefined

lmul :: Value -> Value
lmul Nil = Number 1
lmul (Pair (Number a) b) =
  Number
    ( a * case lmul b of
        Number x -> x
        _ -> undefined
    )
lmul _ = undefined

lsub :: Value -> Value
lsub (Pair (Number a) (Pair (Number b) Nil)) = Number $ a - b
lsub _ = undefined

ldiv :: Value -> Value
ldiv (Pair (Number a) (Pair (Number b) Nil)) = Number $ div a b
ldiv _ = undefined

lgrt :: Value -> Value
lgrt (Pair (Number a) (Pair (Number b) Nil)) = if a > b then Number 1 else Nil
lgrt _ = undefined

llss :: Value -> Value
llss (Pair (Number a) (Pair (Number b) Nil)) = if a < b then Number 1 else Nil
llss _ = undefined

leql :: Value -> Value
leql (Pair a (Pair b Nil)) = if a == b then Number 1 else Nil
leql _ = undefined
