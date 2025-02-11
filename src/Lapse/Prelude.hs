module Lapse.Prelude where

import Data.Map.Strict (empty, fromList)
import Lapse.Eval (eval)
import Lapse.Operators
import Lapse.Types (Scope, Scopes, Value (..))

prelude :: Scope
prelude =
  fromList
    [ ("+", Function ladd)
    , ("*", Function lmul)
    , ("/", Function ldiv)
    , ("-", Function lsub)
    , ("<", Function llss)
    , (">", Function lgrt)
    , ("==", Function leql)
    , ("let", Macros llet)
    , ("set", Macros lset)
    , ("cond", Macros cond)
    , ("map", Function lmap)
    , ("double", Function ldouble)
    , ("list", Function llist)
    , ("gensym", Function gensym)
    , ("eval", Function eval)
    ]

initState :: Scopes
initState = [empty, prelude]
