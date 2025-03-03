module Main where

import Control.DeepSeq (NFData, force)
import Control.Exception (SomeException (..), catch, evaluate)
import Control.Monad (forever, unless)
import Lapse (runExpression', runExpressionIO)
import Lapse.Modules (fileExists)
import System.Environment (getArgs)
import System.IO (
  hFlush,
  readFile',
  stdout,
 )

catchAny :: (NFData (m String)) => m String -> (SomeException -> IO (m String)) -> IO (m String)
catchAny = catch . evaluate . force

repl :: IO ()
repl = forever $ read' >>= eval >>= print'
 where
  read' = do
    putStr "(repl@lapse)>> "
    hFlush stdout
    getLine
  eval expr = catchAny (runExpression' expr) (pure . pure . show)
  print' = putStrLn . head

executeFile :: String -> IO ()
executeFile s = do
  exists <- fileExists s
  unless exists $ error $ "No such file: " ++ s
  expr <- readFile' s
  _ <- runExpressionIO expr
  pure ()

notEmpty :: (Foldable t) => t a -> Bool
notEmpty = not . null

main :: IO ()
main = getArgs >>= \x -> if not $ null x then executeFile $ head x else repl
