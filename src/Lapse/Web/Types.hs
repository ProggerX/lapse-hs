module Lapse.Web.Types where

import Data.ByteString.Lazy qualified as BS

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
