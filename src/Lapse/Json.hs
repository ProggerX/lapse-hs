module Lapse.Json where

import Data.Aeson qualified as A
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Map.Strict (fromList)
import Data.Maybe (fromMaybe)
import Lapse.Types (Func, Scope, Value (..))

mod :: Scope IO
mod =
  fromList
    [ ("encode", Function lenc)
    , ("decode", Function ldec)
    ]

lenc :: (Monad m) => Func m
lenc (Pair v Nil) = pure $ String $ unpack $ A.encode v
lenc _ = error "encode accepts only one value"

ldec :: (Monad m) => Func m
ldec (Pair (String s) Nil) = pure $ fromMaybe Nil $ A.decode $ pack s
ldec _ = error "decode accepts only one string"
