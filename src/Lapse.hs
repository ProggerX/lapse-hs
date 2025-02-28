module Lapse where

import Control.Monad.State (evalState, evalStateT)
import Lapse.Eval (eval)
import Lapse.Parser (parse)
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
impureVal = (`evalState` 0) . (`evalStateT` initState)

runValue :: Value -> Value
runValue = impureVal . eval

runExpression' :: String -> [Value]
runExpression' = map (impureVal . eval) . parse

runExpression :: String -> String
runExpression = show . runExpression'
