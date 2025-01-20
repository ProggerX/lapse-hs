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

infixTests :: [Value]
infixTests =
  [ Pair (Name "+") (Pair (Number 1) (Number 2))
  , Pair (Name "+") (Pair (Number 1) (Pair (Number 2) (Number 3)))
  , Pair (Name "*") (Pair (Number 1) (Pair (Name "+") (Pair (Number 2) (Number 3))))
  , Pair (Name "+") (Pair (Number 1) (Pair (Name "*") (Pair (Number 2) (Number 3))))
  , Pair (Name "*") (Pair (Number 1) (Pair (Name "+") (Pair (Number 2) (Pair (Name "*") (Pair (Number 3) (Number 4))))))
  , Pair (Name "+") (Pair (Number 1) (Pair (Name "*") (Pair (Number 2) (Pair (Name "+") (Pair (Number 3) (Number 4))))))
  ]

main :: IO ()
main = do
  let lst = Pair (Number 1) (Pair (Number 2) (Pair (Number 3) Nil))
  print lst
  mapM_ print tests
  mapM_ printInfix infixTests
