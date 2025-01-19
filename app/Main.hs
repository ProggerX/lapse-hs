module Main where

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
