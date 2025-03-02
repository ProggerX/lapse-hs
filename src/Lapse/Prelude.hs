module Lapse.Prelude where

import Data.Map.Strict (empty, fromList)
import Lapse.Eval (eval)
import Lapse.Lambda (define, defmacro, lambda, macro)
import Lapse.Operators
import Lapse.Types (Scope, Scopes, Value (..))

prelude :: (Monad m) => Scope m
prelude =
  fromList
    [ ("+", Function ladd)
    , ("*", Function lmul)
    , ("/", Function ldiv)
    , ("-", Function lsub)
    , ("<", Function llss)
    , (">", Function lgrt)
    , ("==", Function leql)
    , ("^", Function lpow)
    , ("sqrt", Function lsqr)
    , ("let", Macros llet)
    , ("set", Macros lset)
    , ("cond", Macros cond)
    , ("map", Function lmap)
    , ("double", Function ldouble)
    , ("list", Function llist)
    , ("gensym", Function gensym)
    , ("eval", Function leval)
    , ("nil", Nil)
    , ("raw", Macros lraw)
    , ("fst", Function lfst)
    , ("snd", Function lsnd)
    , ("fact", Function lfac)
    , ("concat", Function lcon)
    , ("show", Function lshow)
    , ("lambda", Macros lambda)
    , ("defn", Macros define)
    , ("macro", Macros macro)
    , ("defmacro", Macros defmacro)
    ]

ioPrelude :: Scope IO
ioPrelude =
  fromList
    [ ("print", Function lprint)
    , ("getline", Function lgetl)
    ]

initState :: (Monad m) => Scopes m
initState = [empty, prelude]

initIOState :: Scopes IO
initIOState = [empty, prelude, ioPrelude]
