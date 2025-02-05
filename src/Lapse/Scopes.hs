module Lapse.Scopes where

import Control.Monad.State (State, get, gets, put)
import Data.Map.Strict (Map, (!?))
import Data.Map.Strict qualified as Map
import Lapse (Value (..))
import Lapse.Eval (eval)

type Scope = Map String Value
type Scopes = [Scope]

type ScopeM a = State Scopes a

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
getValue k = gets (getValue' k)

lset :: Value -> ScopeM ()
lset (Pair (Name k) (Pair v Nil)) = changeValue k v
lset _ = error "Wrong argument for set"

llet' :: Value -> ScopeM Value
llet' (Pair (Pair (Pair (Name k) (Pair v Nil)) Nil) (Pair val Nil)) = do
  changeValue k v
  pure $ eval val
llet' (Pair (Pair (Pair (Name k) (Pair v Nil)) other) c@(Pair _ Nil)) = do
  changeValue k v
  llet' (Pair other c)
llet' _ = error "Wrong argument for let"

llet :: Value -> ScopeM Value
llet v = do
  newScope
  res <- llet' v
  dropScope
  pure res
