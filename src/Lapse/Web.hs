{-# LANGUAGE OverloadedStrings #-}

module Lapse.Web where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Char8 qualified as BC
import Data.Map.Strict (fromList)
import Lapse.Types (Func, Scope, Value (..))
import Network.Wreq (responseBody, responseStatus, statusCode)
import Network.Wreq qualified as W

mod :: Scope IO
mod =
  fromList
    [ ("get", Function lget)
    , ("body", Function lbody)
    , ("status", Function lstat)
    ]

lget :: Func IO
lget (Pair (String s) Nil) = do
  r <- liftIO $ W.get s
  pure
    WResponse
      { status = r ^. responseStatus . statusCode
      , body = r ^. responseBody
      }
lget _ = error "HTTP get needs exactly one argument :: String"

lbody :: Func IO
lbody (Pair (WResponse{body}) Nil) = pure $ String $ BC.unpack body
lbody _ = error "body needs exactly one argument :: WResponse"

lstat :: Func IO
lstat (Pair (WResponse{status}) Nil) = pure $ Number status
lstat _ = error "status needs exactly one argument :: WResponse"
