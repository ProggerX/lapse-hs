module Lapse where

data Value
  = Nil
  | Number Int
  | Name String
  | Pair Value Value
  deriving (Eq)

list :: [Value] -> Value
list = foldr Pair Nil

numList :: [Int] -> Value
numList = list . map Number

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
