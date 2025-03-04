module Main where

import Control.DeepSeq (NFData, force)
import Control.Exception (SomeException (..), catch, evaluate, onException)
import Control.Monad (forever, unless)
import Data.Function ((&))
import Lapse (runExpression', runExpressionIO)
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode), hFlush, readFile', stdout, withFile)
import Prelude hiding (print, read)

fileExists :: FilePath -> IO Bool
fileExists path =
  withFile path ReadMode (\_ -> pure True) `onException` pure False

catchAny :: (NFData (m String)) => m String -> (SomeException -> IO (m String)) -> IO (m String)
catchAny = catch . evaluate . force

repl :: IO ()
repl = read >>= eval >>= print & loop
 where
  read = do
    putStr "(repl@lapse)>> "
    hFlush stdout
    getLine

  eval expr = catchAny (runExpression' expr) (\exc -> pure [show exc])

  print = putStrLn . head

  loop = forever

executeFile :: String -> IO ()
executeFile s = do
  exists <- fileExists s
  unless exists $ error $ "No such file: " ++ s
  expr <- readFile' s
  _ <- runExpressionIO expr
  pure ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    x : _ -> executeFile x
