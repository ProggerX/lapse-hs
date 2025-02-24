module Main where

import Control.DeepSeq (force)
import Control.Exception (SomeException, catch, evaluate)
import Control.Monad (forever)
import Lapse (runExpression)
import System.IO (hFlush, stdout)

catchAny :: String -> (SomeException -> IO String) -> IO String
catchAny = catch . evaluate . force

repl :: IO ()
repl = forever $ do
  putStr "(repl@lapse)>> "
  hFlush stdout
  expr <- getLine
  result <- catchAny (runExpression expr) (pure . show)
  putStrLn result

main :: IO ()
main = repl
