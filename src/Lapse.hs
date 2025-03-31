module Lapse where

import Control.Monad ((<=<))
import Control.Monad.State (evalStateT)
import Lapse.Eval (eval)
import Lapse.Modules (initIOState, replState)
import Lapse.Parser (parse)
import Lapse.Types (LapseM, Value (..))

list :: [Value] -> Value
list = foldr Pair Nil

numList :: [Int] -> Value
numList = list . map Number

list' :: [Value] -> Value
list' = Pair (Name "list") . list

numList' :: [Int] -> Value
numList' = list' . map Number

evalLapseM :: LapseM a -> IO a
evalLapseM = (`evalStateT` 0) . (`evalStateT` initIOState)

runExpression :: String -> IO [Value]
runExpression = evalLapseM . mapM eval . parse

evalLapseM' :: LapseM a -> IO a
evalLapseM' = (`evalStateT` 0) . (`evalStateT` replState)

runExpression' :: String -> IO String
runExpression' = (pure . show) <=< f
 where
  f = evalLapseM' . mapM eval . parse
