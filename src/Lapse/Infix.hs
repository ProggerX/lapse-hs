module Lapse.Infix where

import Lapse (Value (..), surround)

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
