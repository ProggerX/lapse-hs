module Lapse.Parser where

tokenize' :: String -> [String] -> String -> [String]
tokenize' cur tokens (c : cs) = case c of
  '(' -> []

tokenize :: String -> [String]
tokenize = tokenize' "" []
