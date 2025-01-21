module Lib where

data Value
  = Nil
  | Number Int
  | Name String
  | Pair Value Value

surround :: String -> String
surround s = "(" ++ s ++ ")"

show' :: Value -> String
show' (Pair a Nil) = show' a
show' (Pair a b) =
  show a ++ " " ++ f b
 where
  f x@(Pair _ _) = show' x
  f x = ". " ++ show x
show' x = show x

instance Show Value where
  show Nil = "()"
  show (Number n) = show n
  show (Name s) = s
  show pr@(Pair _ _) = surround $ show' pr

toInfix :: Value -> String
toInfix (Pair x Nil) = toInfix x
toInfix (Pair nm@(Name s) (Pair v1 v2@(Pair _ _))) =
  concat
    [ show v1
    , " "
    , s
    , " "
    , f v2
    ]
 where
  f (Pair (Pair _ _) Nil) =
    ( case s of
        "*" -> surround
        "/" -> surround
        _ -> id
    )
      $ toInfix v2
  f (Pair _ Nil) = toInfix v2
  f (Pair _ _) = toInfix $ Pair nm v2
  f _ = undefined
toInfix v = show v

printInfix :: Value -> IO ()
printInfix = putStrLn . toInfix

list :: [Value] -> Value
list = foldr Pair Nil
