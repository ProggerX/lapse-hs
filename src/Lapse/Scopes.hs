module Lapse.Scopes where

import Data.Map.Strict (Map, (!?))
import Data.Map.Strict qualified as Map
import Lapse (Value (..))

type Scope = Map String Value
type Scopes = [Scope]

newScope :: Scopes -> Scopes
newScope ss = Map.empty : ss

changeValue :: String -> Value -> Scopes -> Scopes
changeValue k v (s : ss) = Map.insert k v s : ss
changeValue _ _ [] = undefined

getValue :: String -> Scopes -> Value
getValue k (s : ss) = case s !? k of
  Nothing -> getValue k ss
  Just x -> x
getValue _ [] = Nil
