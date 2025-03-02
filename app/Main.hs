module Main where

import Control.DeepSeq (NFData, force)
import Control.Exception (SomeException (..), catch, evaluate)
import Control.Monad (forever, unless)
import Lapse (runExpression', runExpressionIO)
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

catchAny :: (NFData (m String)) => m String -> (SomeException -> IO (m String)) -> IO (m String)
catchAny = catch . evaluate . force

repl :: IO ()
repl = forever $ do
  putStr "(repl@lapse)>> "
  hFlush stdout
  expr <- getLine
  res <- catchAny (runExpression' expr) (pure . pure . show)
  putStrLn $ head res

executeFile :: String -> IO ()
executeFile s = do
  exists <- fileExists s
  unless exists (error $ "No such file: " ++ s)
  expr <- readFile' s
  _ <- runExpressionIO expr
  pure ()

notEmpty :: (Foldable t) => t a -> Bool
notEmpty = not . null

main :: IO ()
main = getArgs >>= \x -> if not $ null x then executeFile $ head x else repl
