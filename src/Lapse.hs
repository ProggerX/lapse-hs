module Lapse where

import Control.Monad ((<=<))
import Control.Monad.State (evalStateT)
import Lapse.Eval (eval)
import Lapse.Modules (initIOState, replState)
import Lapse.Parser (parse)
import Lapse.Types (LapseM, Value (..))

list :: [Value m] -> Value m
list = foldr Pair Nil

numList :: [Int] -> Value m
numList = list . map Number

list' :: [Value m] -> Value m
list' = Pair (Name "list") . list

numList' :: [Int] -> Value m
numList' = list' . map Number

evalLapseMIO :: LapseM IO a -> IO a
evalLapseMIO = (`evalStateT` 0) . (`evalStateT` initIOState)

runExpression :: String -> IO [Value IO]
runExpression = evalLapseMIO . mapM eval . parse

runExpression' :: String -> IO String
runExpression' = (pure . show) <=< runExpression

evalLapseMIOR :: LapseM IO a -> IO a
evalLapseMIOR = (`evalStateT` 0) . (`evalStateT` replState)

runExpressionR :: String -> IO [Value IO]
runExpressionR = evalLapseMIOR . mapM eval . parse

runExpressionR' :: String -> IO String
runExpressionR' = (pure . show) <=< runExpressionR
