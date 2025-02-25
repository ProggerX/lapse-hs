module Lapse.Parser where

import Data.Char (isDigit)
import Lapse.Types (Value (..))

add' :: (Monoid a, Eq a) => a -> [a] -> [a]
add' s a = if s == mempty then a else s : a

tokenize' :: String -> [String] -> String -> [String]
tokenize' cur tokens (c : cs) = case c of
  ' ' -> tokenize' "" (add' cur tokens) cs
  '(' -> tokenize' "" ("(" : add' cur tokens) cs
  ')' -> tokenize' "" (")" : add' cur tokens) cs
  _ -> tokenize' (cur ++ [c]) tokens cs
tokenize' cur tokens "" = case cur of
  "" -> tokens
  _ -> cur : tokens

tokenize :: String -> [String]
tokenize = tokenize' "" []

tokenizeR :: String -> [String]
tokenizeR = reverse . tokenize

parseToken :: String -> Value
parseToken t = if all isDigit t then Number (read t) else Name t

parse' :: [Value] -> [String] -> Value
parse' stack (t : ts) = case t of
  ")" -> parse' (Nil : stack) ts
  "(" -> parse' (Pair (head stack) (head tl) : tail tl) ts
   where
    tl = tail stack
  _ -> parse' (Pair (parseToken t) (head stack) : tail stack) ts
parse' stack [] = head stack

unList :: Value -> [Value]
unList Nil = []
unList (Pair h t) = h : unList t
unList _ = undefined

parse :: String -> [Value]
parse = unList . parse' [Nil] . tokenize
