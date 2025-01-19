module Lib where

data Value = Nil | Number Int | Pair Value Value

surround :: String -> String
surround s = "(" ++ s ++ ")"

surround' :: String -> String
surround' = init . tail

instance Show Value where
  show Nil = "()"
  show (Number n) = show n
  show (Pair a Nil) = surround $ show a
  show (Pair a (Pair b c)) =
    concat
      [ "("
      , show a
      , " "
      , surround' $ show (Pair b c)
      , ")"
      ]
  show (Pair a b) =
    concat
      [ "("
      , show a
      , " . "
      , show b
      , ")"
      ]
