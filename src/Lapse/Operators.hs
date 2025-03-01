{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Lapse.Operators where

import Control.Monad.State (evalState, evalStateT, get, lift, put)
import Data.Function (fix)
import Lapse.Eval (eval, lmap')
import Lapse.Scopes (changeValue, dropScope, newScope)
import Lapse.Types (Func, ScopeM, Scopes, Value (..))

pureFunc :: (Value -> Value) -> Func
pureFunc = (pure .)

pureFunc' :: ((Value -> Value) -> Value -> Value) -> Func
pureFunc' = pureFunc . fix

ladd :: Func
ladd = pureFunc' \f -> \case
  Nil -> Number 0
  (Pair (Number a) b) ->
    Number
      ( a + case f b of
          Number x -> x
          _ -> undefined
      )
  _ -> undefined

lmul :: Func
lmul = pureFunc' \f -> \case
  Nil -> Number 1
  (Pair (Number a) b) ->
    Number
      ( a * case f b of
          Number x -> x
          _ -> undefined
      )
  _ -> undefined

lsub :: Func
lsub = pureFunc \case
  (Pair (Number a) (Pair (Number b) Nil)) -> Number $ a - b
  _ -> undefined

ldiv :: Func
ldiv = pureFunc \case
  (Pair (Number a) (Pair (Number b) Nil)) -> Number $ div a b
  _ -> undefined

lgrt :: Func
lgrt = pureFunc \case
  (Pair (Number a) (Pair (Number b) Nil)) -> if a > b then Number 1 else Nil
  _ -> undefined

llss :: Func
llss = pureFunc \case
  (Pair (Number a) (Pair (Number b) Nil)) -> if a < b then Number 1 else Nil
  _ -> undefined

leql :: Func
leql = pureFunc \case
  (Pair a (Pair b Nil)) -> if a == b then Number 1 else Nil
  _ -> undefined

lset :: Func
lset (Pair (Name k) (Pair v Nil)) = eval v >>= changeValue k >> pure Nil
lset _ = error "Wrong argument for set"

llet' :: Func
llet' (Pair (Pair (Pair (Name k) (Pair v Nil)) Nil) (Pair val Nil)) = do
  eval v >>= changeValue k
  eval val
llet' (Pair (Pair (Pair (Name k) (Pair v Nil)) other) c@(Pair _ Nil)) = do
  eval v >>= changeValue k
  llet' (Pair other c)
llet' _ = error "Wrong argument for let"

llet :: Func
llet v = newScope *> llet' v <* dropScope

cond :: Func
cond = \case
  Nil -> pure Nil
  (Pair (Pair c (Pair r Nil)) els) ->
    eval c >>= \case
      Nil -> cond els
      _ -> eval r
  _ -> undefined

lmap :: Func
lmap (Pair (Function f) (Pair oth Nil)) = lmap' f oth
lmap (Pair (Macros f) (Pair oth Nil)) = lmap' f oth
lmap _ = undefined

ldouble :: Func
ldouble (Number x) = pure $ Number $ x * 2
ldouble _ = undefined

llist :: Func
llist = pure

gensym :: Func
gensym Nil = lift get >>= \x -> lift $ put (x + 1) >> pure (Name (" sym" ++ show x))
gensym _ = undefined

impureVal' :: Scopes -> ScopeM Value -> Value
impureVal' st = (`evalState` 0) . (`evalStateT` st)

lraw :: Func
lraw (Pair v Nil) = get >>= \st -> f st v
 where
  f :: Scopes -> Func
  f _ (Pair (Name "unraw") (Pair b Nil)) = eval b
  f st (Pair a b) = pure (Pair (impureVal' st $ f st a) (impureVal' st $ f st b))
  f _ x = pure x
lraw _ = undefined

lfst :: Func
lfst (Pair (Pair v _) Nil) = pure v
lfst _ = error "Not a pair"
lsnd :: Func
lsnd (Pair (Pair _ v) Nil) = pure v
lsnd _ = error "Not a pair"

lpow :: Func
lpow = pureFunc \case
  (Pair (Number a) (Pair (Number b) Nil)) -> Number $ a ^ b
  _ -> undefined

lsqr :: Func
lsqr = pureFunc \case
  (Pair (Number a) Nil) -> Number $ floor (sqrt $ fromIntegral a :: Double)
  _ -> undefined

fact :: Int -> Int
fact 0 = 1
fact x = x * fact (x - 1)

lfac :: Func
lfac = pureFunc \case
  (Pair (Number a) Nil) -> Number $ fact a
  _ -> undefined

lcon :: Func
lcon = pureFunc' \f -> \case
  (Pair s@(String _) Nil) -> s
  (Pair (String s) b) -> case f b of
    (String b') -> String $ s ++ b'
    _ -> error "Concat error"
  _ -> error "Concat error"

lshow :: Func
lshow (Pair x Nil) = pure $ String $ show x
lshow _ = error "Show need exactly one argument"
