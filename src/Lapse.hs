module Lapse where

import Control.Monad.State (evalState)
import Data.Function (fix)
import Lapse.Scopes (initState)
import Lapse.Types (Func, ScopeM, Value (..))

list :: [Value] -> Value
list = foldr Pair Nil

numList :: [Int] -> Value
numList = list . map Number

impureFunc :: (Value -> Value) -> Func
impureFunc = (pure .)

impureFunc' :: ((Value -> Value) -> Value -> Value) -> Func
impureFunc' = impureFunc . fix

pureVal :: ScopeM Value -> Value
pureVal = (`evalState` initState)

pureFunc :: Func -> (Value -> Value)
pureFunc = (pureVal .)
