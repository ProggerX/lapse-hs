module Lapse where

import Control.Monad.State (evalState)
import Data.Function (fix)
import Lapse.Scopes (initState)
import Lapse.Types (Func, ScopeM, Value (..))

list :: [Value] -> Value
list = foldr Pair Nil

numList :: [Int] -> Value
numList = list . map Number

pureFunc :: (Value -> Value) -> Func
pureFunc = (pure .)

pureFunc' :: ((Value -> Value) -> Value -> Value) -> Func
pureFunc' = pureFunc . fix

impureVal :: ScopeM Value -> Value
impureVal = (`evalState` initState)

impureFunc :: Func -> (Value -> Value)
impureFunc = (impureVal .)
