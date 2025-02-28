module Main where

import Control.DeepSeq (force)
import Control.Exception (SomeException (..), catch, evaluate)
import Control.Monad (forever, unless)
import Lapse (runExpression)
import System.Environment (getArgs)
import System.IO (
  IOMode (ReadMode),
  hClose,
  hFlush,
  openFile,
  readFile',
  stdout,
 )

fileExists :: FilePath -> IO Bool
fileExists path =
  do
    handle <- openFile path ReadMode
    hClose handle
    return True
    `catch` (\(SomeException _) -> return False)

catchAny :: String -> (SomeException -> IO String) -> IO String
catchAny = catch . evaluate . force

run :: String -> IO ()
run expr = do
  result <- catchAny (runExpression expr) (pure . show)
  putStrLn result

repl :: IO ()
repl = forever $ do
  putStr "(repl@lapse)>> "
  hFlush stdout
  expr <- getLine
  run expr

executeFile :: String -> IO ()
executeFile s = do
  exists <- fileExists s
  unless exists (error $ "No such file: " ++ s)
  expr <- unwords . lines <$> readFile' s
  run expr

notEmpty :: (Foldable t) => t a -> Bool
notEmpty = not . null

main :: IO ()
main = getArgs >>= \x -> if not $ null x then executeFile $ head x else repl
