{-# LANGUAGE OverloadedStrings #-}

module Lapse.Web where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BC
import Data.Map.Strict (fromList)
import Data.Typeable (cast)
import Lapse.Types (Func, Scope, TBox (..), Value (..))
import Network.Wreq (responseBody, responseStatus, statusCode)
import Network.Wreq qualified as W

data WResponse = WResponse {status :: Int, body :: ByteString} deriving (Eq)

instance Show WResponse where
  show WResponse{status} = "WResponse, status: " ++ show status

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
  pure $
    External $
      TBox $
        WResponse
          { status = r ^. responseStatus . statusCode
          , body = r ^. responseBody
          }
lget _ = error "HTTP get needs exactly one argument :: String"

lbody :: Func IO
lbody (Pair (External (TBox x)) Nil) =
  case cast @_ @WResponse x of
    Just r -> pure $ String $ BC.unpack $ body r
    Nothing -> lbody Nil
lbody _ = error "body needs exactly one argument :: WResponse"

lstat :: Func IO
lstat (Pair (External (TBox x)) Nil) =
  case cast @_ @WResponse x of
    Just r -> pure $ String $ BC.unpack $ body r
    Nothing -> lstat Nil
lstat _ = error "status needs exactly one argument :: WResponse"
