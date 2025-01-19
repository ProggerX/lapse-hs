module Main where

import Lib

tests :: [Value]
tests =
  [ Pair (Number 1) (Pair (Number 2) (Number 3))
  , Pair (Pair (Number 1) (Number 2)) (Number 3)
  , Pair (Pair (Number 1) (Number 2)) (Pair (Number 3) Nil)
  , Number 5
  , Pair (Number 5) Nil
  ]

main :: IO ()
main = do
  let lst = Pair (Number 1) (Pair (Number 2) (Pair (Number 3) Nil))
  print lst
  mapM_ print tests
