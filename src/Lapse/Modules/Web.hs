module Lapse.Modules.Web where

import Data.Map.Strict (fromList)
import Lapse.Types (Scope, Value (..))
import Lapse.Modules.Web.Client
import Lapse.Modules.Web.Server (lserve, lserver)
import Lapse.Modules.Web.Server.Get (lroutG)
import Lapse.Modules.Web.Server.Post (lroutP)

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
    , -- --------------------------------
      ("serve", Function lserve)
    , ("routeGET", Function lroutG)
    , ("routePOST", Function lroutP)
    , ("server", Function lserver)
    ]
