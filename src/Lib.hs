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

show' :: Value -> String
show' (Pair a Nil) = show' a
show' (Pair a pr@(Pair _ _)) =
  concat
    [ show a
    , " "
    , show' pr
    ]
show' (Pair a b) =
  concat
    [ show a
    , " . "
    , show b
    ]
show' x = show x

instance Show Value where
  show Nil = "()"
  show (Number n) = show n
  show (Name s) = s
  show pr@(Pair _ _) = surround $ show' pr

toInfix :: Value -> String
toInfix (Pair x Nil) = toInfix x
toInfix (Pair (Name s) (Pair v1 v2@(Pair (Pair _ _) Nil))) =
  concat
    [ show v1
    , " "
    , s
    , " "
    , ( case s of
          "*" -> surround
          "/" -> surround
          _ -> id
      )
        $ toInfix v2
    ]
toInfix (Pair (Name s) (Pair v1 v2@(Pair _ Nil))) =
  concat
    [ show v1
    , " "
    , s
    , " "
    , toInfix v2
    ]
toInfix (Pair nm@(Name s) (Pair v1 v2@(Pair _ _))) =
  concat
    [ show v1
    , " "
    , s
    , " "
    , toInfix $ Pair nm v2
    ]
toInfix v = show v

printInfix :: Value -> IO ()
printInfix = putStrLn . toInfix

list :: [Value] -> Value
list = foldr Pair Nil
