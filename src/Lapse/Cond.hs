module Lapse.Cond where

import Lapse.Eval (eval)
import Lapse.Types (Value (..))

cond :: Value -> Value
cond Nil = Nil
cond (Pair (Pair c (Pair r Nil)) els) = if eval c /= Nil then eval r else cond els
cond _ = undefined
