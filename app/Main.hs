module Main where

import Control.Exception (SomeException, try)
import Control.Monad (forever, unless)
import Data.Function ((&))
import Lapse (runExpression, runExpression')
import Lapse.Modules (fileExists)
import System.Environment (getArgs)
import System.IO (
  hFlush,
  readFile',
  stdout,
 )

repl :: IO ()
repl = r >>= e >>= p & l
 where
  r = do
    putStr "(repl@lapse)>> "
    hFlush stdout
    getLine
  e = runExpression'
  p x = do
    printResult <- try (putStrLn x) :: IO (Either SomeException ())
    case printResult of
      Left err -> putStrLn $ "Error: " ++ show err
      Right _ -> pure ()
  l = forever

executeFile :: String -> IO ()
executeFile s = do
  exists <- fileExists s
  unless exists $ error $ "No such file: " ++ s
  file <- readFile' s
  let expr = unwords $ filter (\x -> not (null x) && head x /= '-') $ lines file
  _ <- runExpression expr
  pure ()

notEmpty :: (Foldable t) => t a -> Bool
notEmpty = not . null

main :: IO ()
main = getArgs >>= \x -> if not $ null x then executeFile $ head x else repl
