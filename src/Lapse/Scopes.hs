{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Lapse.Scopes where

import Control.Monad.State (gets, modify)
import Data.Map.Strict ((!?))
import Data.Map.Strict qualified as Map
import Lapse.Types (LapseM, Scope, Scopes, Value (..))

newScope :: LapseM ()
newScope = modify (Map.empty :)

addScope :: Scope -> LapseM ()
addScope = modify . (:)

addScopes :: Scopes -> LapseM ()
addScopes = modify . (++)

dropScope :: LapseM ()
dropScope = modify $ drop 1

changeValue :: String -> Value -> LapseM ()
changeValue k v =
  modify \case
    (s : ss) -> Map.insert k v s : ss
    _ -> undefined

getValue' :: String -> Scopes -> Value
getValue' k (s : ss) = case s !? k of
  Nothing -> getValue' k ss
  Just x -> x
getValue' k [] = error $ "getValue: no such key: " ++ k ++ "!"

getValueM' :: String -> Scopes -> Maybe Value
getValueM' k (s : ss) = case s !? k of
  Nothing -> getValueM' k ss
  Just x -> Just x
getValueM' _ [] = Nothing

getValue :: String -> LapseM Value
getValue = gets . getValue'

getValueM :: String -> LapseM (Maybe Value)
getValueM = gets . getValueM'
