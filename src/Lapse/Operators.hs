{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ViewPatterns #-}

module Lapse.Operators where

import Control.Arrow ((>>>))
import Control.Lens ((<<+=))
import Control.Monad.IO.Class (liftIO)
import Data.Function (fix)
import System.IO (hFlush, stdout)

import Lapse.Eval (eval, lmap')
import Lapse.Parser (parse)
import Lapse.Scopes (changeValue, dropScope, newScope)
import Lapse.Types (Func, Value (..))

pureFunc :: (Monad m) => (Value m -> Value m) -> Func m
pureFunc = (pure .)

pureFunc' :: (Monad m) => ((Value m -> Value m) -> Value m -> Value m) -> Func m
pureFunc' = pureFunc . fix

ladd :: (Monad m) => Func m
ladd =
  pureFunc' \f -> \case
    Nil -> Number 0
    Pair (Number a) b -> Number $ a + unNumberUnsafe (f b)
    _ -> undefined

unNumberUnsafe :: Value m -> Int
unNumberUnsafe = \case
  Number x -> x
  _ -> undefined

lmul :: (Monad m) => Func m
lmul = pureFunc' \f -> \case
  Nil -> Number 1
  Pair (Number a) b -> Number $ a * unNumberUnsafe (f b)
  _ -> undefined

lsub :: (Monad m) => Func m
lsub =
  pureFunc $
    unList >>> \case
      [Number a, Number b] -> Number $ a - b
      _ -> undefined

ldiv :: (Monad m) => Func m
ldiv =
  pureFunc $
    unList >>> \case
      [Number a, Number b] -> Number $ div a b
      _ -> undefined

lgrt :: (Monad m) => Func m
lgrt =
  pureFunc $
    unList >>> \case
      [Number a, Number b] -> if a > b then Number 1 else Nil
      _ -> undefined

llss :: (Monad m) => Func m
llss =
  pureFunc $
    unList >>> \case
      [Number a, Number b] -> if a < b then Number 1 else Nil
      _ -> undefined

leql :: (Monad m) => Func m
leql = pureFunc \case
  (Pair a (Pair b Nil)) -> if a == b then Number 1 else Nil
  _ -> undefined

lset :: (Monad m) => Func m
lset (unList -> [Name k, v]) = do
  eval v >>= changeValue k
  pure Nil
lset _ = error "Wrong argument for set"

llet' :: (Monad m) => Func m
llet' (Pair (Pair (Pair (Name k) (Pair v Nil)) Nil) (Pair val Nil)) = do
  eval v >>= changeValue k
  eval val
llet' (Pair (Pair (Pair (Name k) (Pair v Nil)) other) c@(Pair _ Nil)) = do
  eval v >>= changeValue k
  llet' (Pair other c)
llet' _ = error "Wrong argument for let"

llet :: (Monad m) => Func m
llet v = newScope *> llet' v <* dropScope

cond :: (Monad m) => Func m
cond = \case
  Nil -> pure Nil
  (Pair (Pair c (Pair r Nil)) els) ->
    eval c >>= \case
      Nil -> cond els
      _ -> eval r
  _ -> undefined

lmap :: (Monad m) => Func m
lmap (Pair (Function f) (Pair oth Nil)) = lmap' f oth
lmap (Pair (Macros f) (Pair oth Nil)) = lmap' f oth
lmap _ = undefined

ldouble :: (Monad m) => Func m
ldouble (Number x) = pure $ Number $ x * 2
ldouble _ = undefined

llist :: (Monad m) => Func m
llist = pure

gensym :: (Monad m) => Func m
gensym Nil = do
  x <- #counter <<+= 1
  pure $ Name $ " sym" ++ show x
gensym _ = undefined

lraw :: (Monad m) => Func m
lraw (Pair v Nil) = f v
 where
  f :: (Monad m) => Func m
  f (Pair (Name "unraw") (Pair b Nil)) = eval b
  f (Pair a b) = do
    fa <- f a
    fb <- f b
    pure (Pair fa fb)
  f x = pure x
lraw _ = undefined

lfst :: (Monad m) => Func m
lfst (Pair (Pair v _) Nil) = pure v
lfst _ = error "Not a pair"
lsnd :: (Monad m) => Func m
lsnd (Pair (Pair _ v) Nil) = pure v
lsnd _ = error "Not a pair"

lpow :: (Monad m) => Func m
lpow = pureFunc \case
  (Pair (Number a) (Pair (Number b) Nil)) -> Number $ a ^ b
  _ -> undefined

lsqr :: (Monad m) => Func m
lsqr = pureFunc \case
  (Pair (Number a) Nil) -> Number $ floor (sqrt $ fromIntegral a :: Double)
  _ -> undefined

fact :: Int -> Int
fact 0 = 1
fact x = x * fact (x - 1)

lfac :: (Monad m) => Func m
lfac = pureFunc \case
  (Pair (Number a) Nil) -> Number $ fact a
  _ -> undefined

lcon :: (Monad m) => Func m
lcon = pureFunc' \f -> \case
  (Pair s@(String _) Nil) -> s
  (Pair (String s) b) -> case f b of
    (String b') -> String $ s ++ b'
    _ -> error "Concat error"
  _ -> error "Concat error"

lshow :: (Monad m) => Func m
lshow (Pair x Nil) = pure $ String $ show x
lshow _ = error "Show need exactly one argument"

lprint :: Func IO
lprint (Pair (String v) Nil) = liftIO $ putStrLn v >> pure Nil
lprint (Pair v Nil) = liftIO $ print v >> pure Nil
lprint _ = error "Print need exactly one argument"

lwrite :: Func IO
lwrite (Pair (String v) Nil) = liftIO $ putStr v >> pure Nil
lwrite (Pair v Nil) = liftIO $ (putStr . show) v >> pure Nil
lwrite _ = error "Write need exactly one argument :: String"

lflush :: Func IO
lflush = const $ liftIO $ hFlush stdout >> pure Nil

lgetl :: Func IO
lgetl = const $ liftIO $ String <$> getLine

lread :: (Monad m) => Func m
lread (Pair (String s) Nil) = pure $ head $ parse s
lread _ = error "Read need exactly one argument :: String"

unList :: Value m -> [Value m]
unList Nil = []
unList (Pair h t) = h : unList t
unList _ = error "unList got wrong list"

leval :: (Monad m) => Func m
leval = last . map eval . unList
