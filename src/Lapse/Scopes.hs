{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}

module Lapse.Scopes where

import Control.Arrow ((>>>))
import Control.Lens ((%=), _head)
import Control.Monad.State (gets)
import Data.Generics.Labels ()
import Data.Map.Strict ((!?))
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)

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
getValue' k =
  mapMaybe (!? k) >>> \case
    [] -> error $ "getValue: no such key: " ++ k ++ "!"
    v : _ -> v

getValue :: (Monad m) => String -> LapseM m (Value m)
getValue k = gets \Env{scopes} -> getValue' k scopes
