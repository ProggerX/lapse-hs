{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Lapse.Scopes where

import Control.Lens ((%=), _head)
import Control.Monad.State (gets)
import Data.Generics.Labels ()
import Data.Map.Strict ((!?))
import Data.Map.Strict qualified as Map
import Lapse.Types (Env (Env), LapseM, Scope, Scopes, Value (..))
import Lapse.Types qualified

newScope :: (Monad m) => LapseM m ()
newScope = #scopes %= (Map.empty :)

addScope :: (Monad m) => Scope m -> LapseM m ()
addScope s = #scopes %= (s :)

addScopes :: (Monad m) => Scopes m -> LapseM m ()
addScopes ss = #scopes %= (ss ++)

dropScope :: (Monad m) => LapseM m ()
dropScope = #scopes %= drop 1

changeValue :: (Monad m) => String -> Value m -> LapseM m ()
changeValue k v = #scopes . _head %= Map.insert k v

getValue' :: String -> Scopes m -> Value m
getValue' k (s : ss) =
  case s !? k of
    Nothing -> getValue' k ss
    Just x -> x
getValue' k [] = error $ "getValue: no such key: " ++ k ++ "!"

getValue :: (Monad m) => String -> LapseM m (Value m)
getValue k = gets \Env{scopes} -> getValue' k scopes
