module Lib where

import Data.Data (cast)

data Value
  = Nil
  | Number Int
  | Name String
  | Pair Value Value

surround :: String -> String
surround s = "(" ++ s ++ ")"

surround' :: String -> String
surround' = init . tail

instance Show Value where
  show Nil = "()"
  show (Number n) = show n
  show (Name s) = s
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

toInfix :: Value -> String
toInfix (Pair (Name s) (Pair v1 v2)) =
  concat
    [ toInfix v1
    , " "
    , s
    , " "
    , case cast v2 of
        Just (Pair (Name _) _) ->
          ( case s of
              "*" -> surround
              "/" -> surround
              "+" -> id
              "-" -> id
              _ -> undefined
          )
            $ toInfix
              v2
        Just (Pair _ _) -> toInfix $ Pair (Name s) v2
        _ -> toInfix v2
    ]
toInfix v = show v

printInfix :: Value -> IO ()
printInfix = putStrLn . toInfix
