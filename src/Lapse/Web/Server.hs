{-# LANGUAGE BlockArguments #-}

module Lapse.Web.Server where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (evalStateT, get, lift)
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy.Char8 qualified as BCL
import Data.Map.Strict (elems, empty, fromList, insert, keys, (!?))
import Data.Typeable (cast)
import Lapse.Lambda (UnList (..), unList)
import Lapse.Types (Func, TBox (..), Value (..), ext)
import Lapse.Web.Types (URL, WServer (..))
import Network.HTTP.Types (status200, status400, status404, urlDecode)
import Network.Wai qualified as W
import Network.Wai.Handler.Warp qualified as WRP

unString :: Value m -> String
unString (String s) = s
unString _ = error "Error: expected string but got not string"

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

unString' :: Value m -> String
unString' (String s) = s
unString' v = show v

lroutG :: Func IO
lroutG (Pair (String url) (Pair args' (Pair (Function f) (Pair (External (TBox srv)) Nil)))) = do
  st <- get
  cnt <- lift get
  case cast @_ @WServer srv of
    Just s@WServer{routesGET} ->
      let tm x =
            pure $
              ext
                s
                  { routesGET =
                      insert
                        url
                        ( x
                        , (`evalStateT` cnt)
                            . (`evalStateT` st)
                            . fmap (BCL.pack . unString')
                            . f
                            . foldr (Pair . String) Nil
                        )
                        routesGET
                  }
       in case unList args' of
            Single (String arg) -> tm [arg]
            Proper args -> tm $ map unString args
            _ -> lroutG Nil
    Nothing -> lroutG Nil
lroutG _ = error "routeGET error, valid syntax: routeGET <url> <args> <func>"

respond :: WServer -> URL -> IO W.Response
respond WServer{routesGET} (url, params) =
  case routesGET !? url of
    Just (crNames, f) -> do
      let names = keys params
      let values = elems params
      let names' = zip names crNames
      let validNames = foldr ((&&) . uncurry (==)) True names'
      let validLengths = (length names == length crNames) && (length values == length names)
      if validNames && validLengths
        then do
          res <- f values
          pure $ W.responseLBS status200 [] res
        else pure $ W.responseLBS status400 [] $ BCL.pack "Bad Request"
    Nothing -> pure $ W.responseLBS status404 [] $ BCL.pack $ "No such endpoint: " ++ url

lserve :: Func IO
lserve (Pair (External (TBox srv)) Nil) =
  case cast @_ @WServer srv of
    Just s@WServer{port} ->
      liftIO
        ( WRP.run port \req res ->
            let path r = BC.unpack $ urlDecode False $ W.rawPathInfo r <> W.rawQueryString r
             in respond s (parseUrlPath $ path req) >>= res
        )
        >> pure Nil
    Nothing -> lserve Nil
lserve _ = error "serve expected exactly one argument :: WServer"

lserver :: Func IO
lserver (Pair (Number port) Nil) = pure $ ext WServer{port, routesGET = empty}
lserver _ = error "Expected integer port as argument to server"
