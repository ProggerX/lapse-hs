module Main where

data Value = Nil | Number Int | Pair Value Value

instance Show Value where
  show Nil = "()"
  show (Number n) = show n
  show (Pair a b) =
    concat
      [ "("
      , show a
      , " . "
      , show b
      , ")"
      ]

main :: IO ()
main = do
  let lst = Pair (Number 1) (Pair (Number 2) (Pair (Number 3) Nil))
  print lst
