module Lapse.Parser where

import Data.Char (isDigit, isSpace)
import Lapse.Types (Value (..))

add' :: (Monoid a, Eq a) => a -> [a] -> [a]
add' s a = if s == mempty then a else s : a

stringToken :: String -> String -> (String, String)
stringToken [] _ = error "Tokenize error in stringToken"
stringToken (c : cs) cur = case c of
  '"' -> (('"' : cur) ++ ['"'], cs)
  '\\' ->
    stringToken
      (tail cs)
      ( cur ++ case head cs of
          '"' -> ['"']
          'n' -> ['\n']
          'e' -> ['\ESC']
          c' -> '\\' : [c']
      )
  _ -> stringToken cs (cur ++ [c])

tokenize' :: String -> [String] -> String -> [String]
tokenize' cur tokens (c : cs) = case c of
  '(' -> tokenize' "" ("(" : add' cur tokens) cs
  ')' -> tokenize' "" (")" : add' cur tokens) cs
  '\'' -> tokenize' "" ("'" : add' cur tokens) cs
  ',' -> tokenize' "" ("," : add' cur tokens) cs
  '"' -> tokenize' "" newTokens newCs
   where
    new = stringToken cs ""
    newTokens = fst new : tokens
    newCs = snd new
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

fst' :: Value -> Value
fst' (Pair v Nil) = v
fst' _ = error "parse error"

fst'' :: Value -> Value
fst'' (Pair v _) = v
fst'' _ = error "parse error"

snd'' :: Value -> Value
snd'' (Pair _ v) = v
snd'' _ = error "parse error"

startsWith :: (Eq a) => [a] -> a -> Bool
startsWith = (==) . head

endsWith :: (Eq a) => [a] -> a -> Bool
endsWith = (==) . last

isString :: String -> Bool
isString t = (t `endsWith` '"') && (t `startsWith` '"')

trim :: String -> String
trim = init . tail

parseToken :: String -> Value
parseToken t
  | all (\c -> isDigit c || c == '-') t && t /= "-" = Number $ read t
  | all (\c -> isDigit c || c == '-' || c == '.') t && t /= "-" && t /= "." = Float $ read t
  | isString t = String $ trim t
  | otherwise = Name t

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
  "$" ->
    let h = head stack
        tl = tail stack
     in parse' (Pair h Nil : tl) ts
  "." -> parse' (fst' (head stack) : tail stack) ts
  _ -> parse' (Pair (parseToken t) (head stack) : tail stack) ts
parse' stack [] = head stack

unList :: Value -> [Value]
unList Nil = []
unList (Pair h t) = h : unList t
unList _ = error "Parse error in unList"

parse :: String -> [Value]
parse = unList . parse' [Nil] . tokenize
