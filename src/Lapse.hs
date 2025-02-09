module Lapse where

import Control.Monad.State (evalState)
import Lapse.Eval (eval)
import Lapse.Prelude (initState)
import Lapse.Types (ScopeM, Value (..))

list :: [Value] -> Value
list = foldr Pair Nil

numList :: [Int] -> Value
numList = list . map Number

list' :: [Value] -> Value
list' = Pair (Name "list") . list

numList' :: [Int] -> Value
numList' = list' . map Number

impureVal :: ScopeM Value -> Value
impureVal = (`evalState` initState)

runValue :: Value -> Value
runValue = impureVal . eval
