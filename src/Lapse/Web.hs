module Lapse.Web where

import Data.Map.Strict (fromList)
import Lapse.Types (Scope, Value (..))
import Lapse.Web.Client

mod :: Scope IO
mod =
  fromList
    [ ("get", Function lget)
    , ("post", Function lpost)
    , ("withParam", Function lparam)
    , ("withHeader", Function lheader)
    , ("withBody", Function lrbody)
    , ("send", Function lsend)
    , ("resBody", Function lbody)
    , ("resStatus", Function lstat)
    ]
