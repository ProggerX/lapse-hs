{-# LANGUAGE LambdaCase #-}

module Lapse.Parser where

import Data.Char (isDigit)
import Lapse.Types (Value (..))

add' :: (Monoid a, Eq a) => a -> [a] -> [a]
add' s a = if s == mempty then a else s : a

isSpace :: Char -> Bool
isSpace = \case
  ' ' -> True
  '\t' -> True
  '\n' -> True
  _ -> False

tokenize' :: String -> [String] -> String -> [String]
tokenize' cur tokens (c : cs) = case c of
  '(' -> tokenize' "" ("(" : add' cur tokens) cs
  ')' -> tokenize' "" (")" : add' cur tokens) cs
  '\'' -> tokenize' "" ("'" : add' cur tokens) cs
  ',' -> tokenize' "" ("," : add' cur tokens) cs
  _ ->
    if isSpace c
      then tokenize' "" (add' cur tokens) cs
      else tokenize' (cur ++ [c]) tokens cs
tokenize' cur tokens "" = case cur of
  "" -> tokens
  _ -> cur : tokens

tokenize :: String -> [String]
tokenize = tokenize' "" []

tokenizeR :: String -> [String]
tokenizeR = reverse . tokenize

parseToken :: String -> Value
parseToken t = if all isDigit t then Number (read t) else Name t

fst' :: Value -> Value
fst' (Pair v Nil) = v
fst' _ = error "parse error"

fst'' :: Value -> Value
fst'' (Pair v _) = v
fst'' _ = error "parse error"

snd'' :: Value -> Value
snd'' (Pair _ v) = v
snd'' _ = error "parse error"

parse' :: [Value] -> [String] -> Value
parse' stack (t : ts) = case t of
  ")" -> parse' (Nil : stack) ts
  "(" -> parse' (Pair (head stack) (head tl) : tail tl) ts
   where
    tl = tail stack
  "," ->
    let h = head stack
        el = fst'' h
        el' = snd'' h
        tl = tail stack
     in parse' (Pair (Pair (Name "unraw") (Pair el Nil)) el' : tl) ts
  "'" ->
    let h = head stack
        el = fst'' h
        el' = snd'' h
        tl = tail stack
     in parse' (Pair (Pair (Name "raw") (Pair el Nil)) el' : tl) ts
  "." -> parse' (fst' (head stack) : tail stack) ts
  _ -> parse' (Pair (parseToken t) (head stack) : tail stack) ts
parse' stack [] = head stack

unList :: Value -> [Value]
unList Nil = []
unList (Pair h t) = h : unList t
unList _ = error "parse error"

parse :: String -> [Value]
parse = unList . parse' [Nil] . tokenize
