module Lapse.Lambda where

import Control.Monad ((<=<))
import Control.Monad.State (get, gets, put)
import Data.Map.Strict (fromList)
import Lapse.Eval (eval)
import Lapse.Operators (lset)
import Lapse.Types (Func, LapseM, Scopes, Value (..))

unList :: Value m -> [Value m]
unList Nil = []
unList (Pair h t) = h : unList t
unList _ = error "Wrong lambda expression"

inScopes :: (Monad m) => Scopes m -> Value m -> LapseM m (Value m)
inScopes ss v = do
  oldScopes <- get
  put ss
  res <- eval v
  put oldScopes
  pure res

unName :: Value m -> String
unName (Name s) = s
unName v = error $ "Expected name, but got: " ++ show v

mkFunction :: forall m. (Monad m) => Value m -> Value m -> LapseM m (Func m)
mkFunction argsN'' expr = gets f
 where
  f :: (Monad m) => Scopes m -> Func m
  f ss args = do
    ns <- get
    let
      argsV = unList args
      argsN' = unList argsN''
      argsN = map unName argsN'
      ss' = fromList (zip argsN argsV) : ss ++ ns
     in
      inScopes ss' expr

lambda :: (Monad m) => Func m
lambda (Pair argN (Pair expr Nil)) =
  Function <$> mkFunction argN expr
lambda _ = error "Wrong lambda expression"

define :: (Monad m) => Func m
define (Pair (Name fname) ls@(Pair _ (Pair _ Nil))) = lambda ls >>= \x -> lset (Pair (Name fname) (Pair x Nil))
define _ = error "Wrong define expression"

macro :: (Monad m) => Func m
macro (Pair argN (Pair expr Nil)) = do
  m <- mkFunction argN expr
  pure $ Macros (eval <=< m)
macro _ = error "Wrong macro expression"

defmacro :: (Monad m) => Func m
defmacro (Pair (Name fname) ls@(Pair _ (Pair _ Nil))) = macro ls >>= \x -> lset (Pair (Name fname) (Pair x Nil))
defmacro _ = error "Wrong defmacro expression"
