module Lapse where

data Value
  = Nil
  | Number Int
  | Name String
  | Pair Value Value
  deriving (Eq)

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

numList :: [Int] -> Value
numList = list . map Number

ladd :: Value -> Value
ladd Nil = Number 0
ladd (Pair (Number a) b) =
  Number
    ( a + case ladd b of
        Number x -> x
        _ -> undefined
    )
ladd _ = undefined

lmul :: Value -> Value
lmul Nil = Number 1
lmul (Pair (Number a) b) =
  Number
    ( a * case lmul b of
        Number x -> x
        _ -> undefined
    )
lmul _ = undefined

lsub :: Value -> Value
lsub (Pair (Number a) (Pair (Number b) Nil)) = Number $ a - b
lsub _ = undefined

ldiv :: Value -> Value
ldiv (Pair (Number a) (Pair (Number b) Nil)) = Number $ div a b
ldiv _ = undefined

lgrt :: Value -> Value
lgrt (Pair (Number a) (Pair (Number b) Nil)) = if a > b then Number 1 else Nil
lgrt _ = undefined

llss :: Value -> Value
llss (Pair (Number a) (Pair (Number b) Nil)) = if a < b then Number 1 else Nil
llss _ = undefined

leql :: Value -> Value
leql (Pair a (Pair b Nil)) = if a == b then Number 1 else Nil
leql _ = undefined

eval :: Value -> Value
eval = id

cond :: Value -> Value
cond Nil = Nil
cond (Pair (Pair c (Pair r Nil)) els) = if eval c /= Nil then r else cond els
cond _ = undefined
