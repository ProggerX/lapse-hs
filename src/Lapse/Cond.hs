{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Lapse.Cond where

import Lapse (impureVal)
import Lapse.Eval (eval)
import Lapse.Types (Func, Value (..))

cond :: Func
cond = \case
  Nil -> pure Nil
  (Pair (Pair c (Pair r Nil)) els) -> if impureVal (eval c) /= Nil then eval r else cond els
  _ -> undefined
