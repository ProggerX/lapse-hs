module Lapse.Scopes where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, StateT, get, gets, put)
import Data.Map.Strict (Map, (!?))
import Data.Map.Strict qualified as Map
import Lapse (Value (..), list)
import Lapse.Eval (eval)

type Scope = Map String Value
type Scopes = [Scope]

newtype ScopeM m a = ScopeM {runScopeM :: StateT Scopes m a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState Scopes
    , MonadIO
    )

initState :: Scopes
initState = [Map.empty]

newScope :: (MonadState Scopes m) => m ()
newScope = get >>= put . (Map.empty :)

dropScope :: (MonadState Scopes m) => m ()
dropScope = get >>= put . tail

changeValue :: (MonadState Scopes m) => String -> Value -> m ()
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

getValue :: (MonadState Scopes m) => String -> m Value
getValue k = gets (getValue' k)

lset :: (MonadState Scopes m) => Value -> m ()
lset (Pair (Name k) (Pair v Nil)) = changeValue k v
lset _ = error "Wrong argument for set"

llet' :: (MonadState Scopes m) => Value -> m Value
llet' (Pair (Pair (Pair (Name k) (Pair v Nil)) Nil) (Pair val Nil)) = do
  changeValue k v
  pure $ eval val
llet' (Pair (Pair (Pair (Name k) (Pair v Nil)) other) c@(Pair _ Nil)) = do
  changeValue k v
  llet' (Pair other c)
llet' _ = error "Wrong argument for let"

llet :: (MonadState Scopes m) => Value -> m Value
llet v = do
  newScope
  res <- llet' v
  dropScope
  pure res

test :: (MonadState Scopes m, MonadIO m) => m ()
test = do
  lset $ list [Name "a", Number 4]
  lset $ list [Name "b", Number 5]
  liftIO $ putStrLn "enter key to lookup"
  n <- liftIO getLine
  g <- getValue n
  liftIO $ print g
  idk <- llet $ list [list [list [Name "b", Number 5], list [Name "c", Number 6]], Number 123]
  liftIO $ print idk
