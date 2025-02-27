{-# LANGUAGE LambdaCase #-}

module Lapse.Scopes where

import Control.Monad.State (get, gets, put)
import Data.Map.Strict ((!?))
import Data.Map.Strict qualified as Map
import Lapse.Types (ScopeM, Scopes, Value (..))

newScope :: ScopeM ()
newScope = get >>= put . (Map.empty :)

dropScope :: ScopeM ()
dropScope = get >>= put . tail

changeValue :: String -> Value -> ScopeM ()
changeValue k v =
  get >>= \case
    (s : ss) -> put (Map.insert k v s : ss)
    _ -> undefined

getValue' :: String -> Scopes -> Value
getValue' k (s : ss) = case s !? k of
  Nothing -> getValue' k ss
  Just x -> x
getValue' k [] = error $ "getValue: no such key: " ++ k ++ "!"

getValue :: String -> ScopeM Value
getValue = gets . getValue'
