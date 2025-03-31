module Lapse.Modules.Json where

import Data.Aeson qualified as A
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Map.Strict (fromList)
import Data.Maybe (fromMaybe)
import Lapse.Types (Func, Scope, Value (..))

mod :: Scope
mod =
  fromList
    [ ("encode", Function lenc)
    , ("decode", Function ldec)
    ]

lenc :: Func
lenc (Pair v Nil) = pure $ String $ unpack $ A.encode v
lenc _ = error "encode accepts only one value"

ldec :: Func
ldec (Pair (String s) Nil) = pure $ fromMaybe Nil $ A.decode $ pack s
ldec _ = error "decode accepts only one string"
