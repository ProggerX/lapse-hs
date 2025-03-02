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

evalScopeM :: ScopeM a -> a
evalScopeM = (`evalState` 0) . (`evalStateT` initState)

runExpression :: String -> [Value]
runExpression = evalScopeM . mapM eval . parse

runExpression' :: String -> String
runExpression' = show . runExpression
