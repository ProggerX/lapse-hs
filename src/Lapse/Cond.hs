module Lapse.Cond where

import Lapse (Value (..))
import Lapse.Eval (eval)

cond :: Value -> Value
cond Nil = Nil
cond (Pair (Pair c (Pair r Nil)) els) = if eval c /= Nil then eval r else cond els
cond _ = undefined
