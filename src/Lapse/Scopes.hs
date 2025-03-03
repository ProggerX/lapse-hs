{-# LANGUAGE LambdaCase #-}

module Lapse.Scopes where

import Control.Monad.State (get, gets, put)
import Data.Map.Strict ((!?))
import Data.Map.Strict qualified as Map
import Lapse.Types (LapseM, Scope, Scopes, Value (..))

newScope :: (Monad m) => LapseM m ()
newScope = get >>= put . (Map.empty :)

addScope :: (Monad m) => Scope m -> LapseM m ()
addScope = (get >>=) . (put .) . (:)

addScopes :: (Monad m) => Scopes m -> LapseM m ()
addScopes = (get >>=) . (put .) . (++) . foldr (:) []

dropScope :: (Monad m) => LapseM m ()
dropScope = get >>= put . tail

changeValue :: (Monad m) => String -> Value m -> LapseM m ()
changeValue k v =
  get >>= \case
    (s : ss) -> put (Map.insert k v s : ss)
    _ -> undefined

getValue' :: String -> Scopes m -> Value m
getValue' k (s : ss) = case s !? k of
  Nothing -> getValue' k ss
  Just x -> x
getValue' k [] = error $ "getValue: no such key: " ++ k ++ "!"

getValue :: (Monad m) => String -> LapseM m (Value m)
getValue = gets . getValue'
