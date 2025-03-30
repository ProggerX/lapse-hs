module Lapse.Modules.Web.Types where

import Data.ByteString.Lazy qualified as BS
import Data.Map.Strict (Map)

data WRequest = WRequest
  { headers :: [(String, [String])]
  , params :: [(String, [String])]
  , url :: String
  , method :: String
  , rbody :: BS.ByteString
  }
  deriving (Eq)
instance Show WRequest where
  show WRequest{method, url} = method ++ " request to" ++ url

data WResponse = WResponse {status :: Int, body :: BS.ByteString} deriving (Eq)
instance Show WResponse where
  show WResponse{status} = "WResponse, status: " ++ show status

type URL = (String, Map String String)

type WBody = String

data WServer = WServer
  { port :: Int
  , routesGET :: Map String ([String], [String] -> IO BS.ByteString)
  , routesPOST :: Map String ([String], WBody -> [String] -> IO BS.ByteString)
  }

instance Eq WServer where
  _ == _ = error "Can't compare servers"

instance Show WServer where
  show WServer{port} = "WServer on port" ++ show port
