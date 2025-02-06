module Lapse.Scopes where

import Control.Monad.State (get, gets, put)
import Data.Map.Strict ((!?))
import Data.Map.Strict qualified as Map
import Lapse.Eval (eval)
import Lapse.Types (Func, ScopeM, Scopes, Value (..))

initState :: Scopes
initState = [Map.empty]

newScope :: ScopeM ()
newScope = get >>= put . (Map.empty :)

dropScope :: ScopeM ()
dropScope = get >>= put . tail

changeValue :: String -> Value -> ScopeM ()
changeValue k v = do
  st <- get
  case st of
    (s : ss) -> put (Map.insert k v s : ss)
    _ -> undefined

getValue' :: String -> Scopes -> Value
getValue' k (s : ss) = case s !? k of
  Nothing -> getValue' k ss
  Just x -> x
getValue' _ [] = error "getValue: no such key!"

getValue :: String -> ScopeM Value
getValue = gets . getValue'

lset :: Value -> ScopeM ()
lset (Pair (Name k) (Pair v Nil)) = changeValue k v
lset _ = error "Wrong argument for set"

llet' :: Func
llet' (Pair (Pair (Pair (Name k) (Pair v Nil)) Nil) (Pair val Nil)) = do
  changeValue k v
  eval val
llet' (Pair (Pair (Pair (Name k) (Pair v Nil)) other) c@(Pair _ Nil)) = do
  changeValue k v
  llet' (Pair other c)
llet' _ = error "Wrong argument for let"

llet :: Func
llet v = newScope *> llet' v <* dropScope
