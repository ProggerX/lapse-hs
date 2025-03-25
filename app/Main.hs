module Main where

import Control.Monad (forever, unless)
import Data.Function ((&))
import Lapse (runExpression, runExpressionR')
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
  e = runExpressionR'
  p = putStrLn
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
