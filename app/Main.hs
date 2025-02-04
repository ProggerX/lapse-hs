module Main where

import Control.Monad.IO.Class (liftIO)
import Lapse (Value (..), list)
import Lapse.Operators (ladd)
import Lapse.Scopes (ScopeM (..), actuallyRun, getValue, llet, lset)

main :: IO ()
main = actuallyRun test

test :: ScopeM ()
test = do
  lset $ list [Name "a", Number 4]
  lset $ list [Name "b", Number 5]
  liftIO $ putStrLn "enter key to lookup"
  n <- liftIO getLine
  g <- getValue n >>= \g -> ladd (Pair g (Pair (Number 1) Nil))
  liftIO $ putStrLn "Here is v + 1:"
  liftIO $ print g
  idk <- llet $ list [list [list [Name "b", Number 5], list [Name "c", Number 6]], Number 123]
  liftIO $ print idk
