{-# LANGUAGE DisambiguateRecordFields #-}

module Lapse where

import Control.Monad.State (evalStateT)
import Lapse.Eval (eval)
import Lapse.Modules (initIOState, initState)
import Lapse.Parser (parse)
import Lapse.Types (Env (Env), LapseM, Value (..))
import Lapse.Types qualified

numList :: [Int] -> Value m
numList = List . map Number

list' :: [Value m] -> Value m
list' = Pair (Name "list") . List

numList' :: [Int] -> Value m
numList' = list' . map Number

evalLapseM :: (Monad m) => LapseM m a -> m a
evalLapseM = (`evalStateT` Env{scopes = initState, counter = 0})

runExpression :: (Monad m) => String -> m [Value m]
runExpression = evalLapseM . mapM eval . parse

runExpression' :: (Monad m) => String -> m String
runExpression' input = show <$> runExpression input

evalLapseMIO :: LapseM IO a -> IO a
evalLapseMIO = (`evalStateT` Env{scopes = initIOState, counter = 0})

runExpressionIO :: String -> IO [Value IO]
runExpressionIO = evalLapseMIO . mapM eval . parse

runExpressionIO' :: String -> IO String
runExpressionIO' input = show <$> runExpressionIO input
