{-# LANGUAGE OverloadedStrings #-}

module Lapse.Modules.Web.Client where

import Control.Lens ((.~), (^.))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BCL
import Data.CaseInsensitive (mk)
import Data.Text qualified as T
import Data.Typeable (cast)
import Lapse.Lambda (UnList (..), unList)
import Lapse.Modules.Web.Types (WRequest (..), WResponse (..))
import Lapse.Types (Func, TBox (..), Value (..), ext)
import Network.Wreq (responseBody, responseStatus, statusCode)
import Network.Wreq qualified as W

mkR :: String -> Func IO
mkR m (Pair (String s) Nil) =
  pure $
    ext $
      WRequest
        { url = s
        , params = []
        , headers = []
        , method = m
        , rbody = BS.empty
        }
mkR _ _ = error "HTTP get needs exactly one argument :: String"

lget :: Func IO
lget = mkR "GET"

lpost :: Func IO
lpost = mkR "POST"

unString :: Value m -> String
unString (String s) = s
unString _ = error "Expected String, got non-string"

lparam :: Func IO
lparam (Pair (String k) (Pair vs (Pair (External (TBox req)) Nil))) =
  case cast @_ @WRequest req of
    Just r@WRequest{params} -> pure $ ext r{params = (k, v) : params}
     where
      v = case unList vs of
        Single (String s) -> [s]
        Proper ss -> map unString ss
        _ -> error "withParam error, value should be either proper list or one string"
    _ -> lparam Nil
lparam _ = error "withParam error, valid syntax: 'withParam <key> <values> <request>'"

lheader :: Func IO
lheader (Pair (String k) (Pair vs (Pair (External (TBox req)) Nil))) =
  case cast @_ @WRequest req of
    Just r@WRequest{headers} -> pure $ ext r{headers = (k, v) : headers}
     where
      v = case unList vs of
        Single (String s) -> [s]
        Proper ss -> map unString ss
        _ -> error "withHeader error, value should be either proper list or one string"
    _ -> lheader Nil
lheader _ = error "withHeader error, valid syntax: 'withHeader <key> <values> <request>'"

lrbody :: Func IO
lrbody (Pair b (Pair (External (TBox req)) Nil)) =
  case cast @_ @WRequest req of
    Just r@WRequest{} -> pure $ ext r{rbody = f b}
     where
      f (String s) = BCL.pack s
      f x = BCL.pack $ show x
    _ -> lrbody Nil
lrbody _ = error "withBody error, valid syntax: 'withBody <body> <request>'"

lsend :: Func IO
lsend (Pair (External (TBox req)) Nil) = case cast @_ @WRequest req of
  Just WRequest{url, params, headers, method, rbody} ->
    do
      r <- liftIO f
      pure $
        ext
          WResponse
            { status = r ^. responseStatus . statusCode
            , body = r ^. responseBody
            }
   where
    opts' = foldr (\(k, v) -> W.param (T.pack k) .~ map T.pack v) W.defaults params
    opts = foldr (\(k, v) -> W.header (mk $ BC.pack k) .~ map BC.pack v) opts' headers
    f =
      case method of
        "GET" -> W.getWith opts url
        "POST" -> W.postWith opts url rbody
        x -> error $ "Unsupported method: " ++ x
  _ -> lsend Nil
lsend _ = error "Send error, valid syntax: 'send <request>'"

lbody :: Func IO
lbody (Pair (External (TBox x)) Nil) =
  case cast @_ @WResponse x of
    Just r -> pure $ String $ BCL.unpack $ body r
    Nothing -> lbody Nil
lbody _ = error "body needs exactly one argument :: WResponse"

lstat :: Func IO
lstat (Pair (External (TBox x)) Nil) =
  case cast @_ @WResponse x of
    Just r -> pure $ String $ BCL.unpack $ body r
    Nothing -> lstat Nil
lstat _ = error "status needs exactly one argument :: WResponse"
