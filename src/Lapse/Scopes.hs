{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Lapse.Scopes where

import Control.Monad.State (gets, modify)
import Data.Map.Strict ((!?))
import Data.Map.Strict qualified as Map
import Lapse.Types (LapseM, Scope, Scopes, Value (..))

newScope :: (Monad m) => LapseM m ()
newScope = modify (Map.empty :)

addScope :: (Monad m) => Scope m -> LapseM m ()
addScope = modify . (:)

addScopes :: (Monad m) => Scopes m -> LapseM m ()
addScopes = modify . (++)

dropScope :: (Monad m) => LapseM m ()
dropScope = modify $ drop 1

changeValue :: (Monad m) => String -> Value m -> LapseM m ()
changeValue k v =
  modify \case
    (s : ss) -> Map.insert k v s : ss
    _ -> undefined

getValue' :: String -> Scopes m -> Value m
getValue' k (s : ss) = case s !? k of
  Nothing -> getValue' k ss
  Just x -> x
getValue' k [] = error $ "getValue: no such key: " ++ k ++ "!"

getValue :: (Monad m) => String -> LapseM m (Value m)
getValue = gets . getValue'
