{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Lapse.Operators where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, lift, put)
import Data.Function (fix)
import Data.Map.Strict (Map, empty, insert, (!))
import Lapse.Eval (eval, lmap')
import Lapse.Parser (parse)
import Lapse.Scopes (changeValue, dropScope, newScope)
import Lapse.Types (Func, Value (..))
import System.IO (hFlush, stdout)

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

lraw :: Func
lraw (Pair v Nil) = f v
 where
  f :: Func
  f (Pair (Name "unraw") (Pair b Nil)) = eval b
  f (Pair a b) = do
    fa <- f a
    fb <- f b
    pure (Pair fa fb)
  f x = pure x
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

lprint :: Func
lprint (Pair (String v) Nil) = liftIO $ putStrLn v >> pure Nil
lprint (Pair v Nil) = liftIO $ print v >> pure Nil
lprint _ = error "Print need exactly one argument"

lwrite :: Func
lwrite (Pair (String v) Nil) = liftIO $ putStr v >> pure Nil
lwrite (Pair v Nil) = liftIO $ (putStr . show) v >> pure Nil
lwrite _ = error "Write need exactly one argument :: String"

lflush :: Func
lflush = const $ liftIO $ hFlush stdout >> pure Nil

lgetl :: Func
lgetl = const $ liftIO $ String <$> getLine

lread :: Func
lread (Pair (String s) Nil) = pure $ head $ parse s
lread _ = error "Read need exactly one argument :: String"

unList :: Value -> [Value]
unList Nil = []
unList (Pair h t) = h : unList t
unList _ = error "unList got wrong list"

leval :: Func
leval = last . map eval . unList

unDict :: Value -> Map String Value
unDict (Dict d) = d
unDict _ = undefined

ldict :: Func
ldict Nil = pure $ Dict empty
ldict (Pair (Pair k (Pair v Nil)) rest) = do
  v' <- eval v
  Dict . insert k' v' . unDict <$> ldict rest
 where
  k' = case k of
    String s -> s
    Name s -> s
    _ -> error "Key should be either string or name"
ldict _ = error "Wrong dict expression"

llkp :: Func
llkp (Pair k' (Pair d' Nil)) = do
  a <- eval d'
  let d = case a of
        Dict x -> x
        _ -> error "Wrong lookup expression. Syntax: lookup <key> <dict>"
  case k' of
    String k -> pure $ d ! k
    Name k -> pure $ d ! k
    _ -> llkp Nil
llkp _ = error "Wrong lookup expression. Syntax: lookup <key> <dict>"
