module Lapse where

import Control.Monad ((<=<))
import Control.Monad.State (evalStateT)
import Lapse.Eval (eval)
import Lapse.Modules (initIOState, initState)
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

evalLapseM :: (Monad m) => LapseM m a -> m a
evalLapseM = (`evalStateT` 0) . (`evalStateT` initState)

runExpression :: (Monad m) => String -> m [Value m]
runExpression = evalLapseM . mapM eval . parse

runExpression' :: (Monad m) => String -> m String
runExpression' = pure . show <=< runExpression

evalLapseMIO :: LapseM IO a -> IO a
evalLapseMIO = (`evalStateT` 0) . (`evalStateT` initIOState)

runExpressionIO :: String -> IO [Value IO]
runExpressionIO = evalLapseMIO . mapM eval . parse

runExpressionIO' :: String -> IO String
runExpressionIO' = (pure . show) <=< runExpressionIO
