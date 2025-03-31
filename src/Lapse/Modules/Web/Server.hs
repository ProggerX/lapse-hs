{-# LANGUAGE BlockArguments #-}

module Lapse.Modules.Web.Server where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy.Char8 qualified as BCL
import Data.Map.Strict (elems, empty, fromList, keys, (!?))
import Data.Typeable (cast)
import Lapse.Modules.Web.Types (URL, WBody, WServer (..))
import Lapse.Types (Func, TBox (..), Value (..), ext)
import Network.HTTP.Types (status200, status400, status404, urlDecode)
import Network.Wai qualified as W
import Network.Wai.Handler.Warp qualified as WRP

parseUrlPath :: String -> URL
parseUrlPath url =
  let (path, query) = break (== '?') url
      params = fromList $ parseQuery (drop 1 query)
   in (path, params)
 where
  parseQuery "" = []
  parseQuery q = map splitParam (splitOn '&' q)

  splitParam param =
    let (key, val) = break (== '=') param
     in (key, drop 1 val)

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delimiter str =
  let (start, rest) = break (== delimiter) str
   in start : case rest of
        [] -> []
        (_ : xs) -> splitOn delimiter xs

respond :: WServer -> URL -> WBody -> IO W.Response
respond WServer{routesGET, routesPOST} (url, params) =
  case routesGET !? url of
    Just (crNames, f) -> do
      let names = keys params
      let values = elems params
      let names' = zip names crNames
      let validNames = foldr ((&&) . uncurry (==)) True names'
      let validLengths = (length names == length crNames) && (length values == length names)
      if validNames && validLengths
        then const do
          res <- f values
          pure $ W.responseLBS status200 [] res
        else const $ pure $ W.responseLBS status400 [] $ BCL.pack "Bad Request"
    Nothing -> case routesPOST !? url of
      Just (crNames, f) -> do
        let names = keys params
        let values = elems params
        let names' = zip names crNames
        let validNames = foldr ((&&) . uncurry (==)) True names'
        let validLengths = (length names == length crNames) && (length values == length names)
        if validNames && validLengths
          then \body -> do
            res <- f body values
            pure $ W.responseLBS status200 [] res
          else const $ pure $ W.responseLBS status400 [] $ BCL.pack "Bad Request"
      Nothing -> const $ pure $ W.responseLBS status404 [] $ BCL.pack $ "No such endpoint: " ++ url

lserve :: Func
lserve (Pair (External (TBox srv)) Nil) =
  case cast @_ @WServer srv of
    Just s@WServer{port} ->
      liftIO
        ( WRP.run port \req res ->
            let path r = BC.unpack $ urlDecode False $ W.rawPathInfo r <> W.rawQueryString r
             in W.strictRequestBody req >>= \body -> respond s (parseUrlPath $ path req) (BCL.unpack body) >>= res
        )
        >> pure Nil
    Nothing -> lserve Nil
lserve _ = error "serve expected exactly one argument :: WServer"

lserver :: Func
lserver (Pair (Number port) Nil) = pure $ ext WServer{port, routesGET = empty, routesPOST = empty}
lserver _ = error "Expected integer port as argument to server"
