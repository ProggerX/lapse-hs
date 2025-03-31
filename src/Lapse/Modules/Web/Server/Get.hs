module Lapse.Modules.Web.Server.Get where

import Control.Monad.State (evalStateT, get, lift)
import Data.ByteString.Lazy.Char8 qualified as BCL
import Data.Map.Strict (insert)
import Data.Typeable (cast)
import Lapse.Lambda (UnList (..), unList)
import Lapse.Modules.Web.Types (WServer (..))
import Lapse.Types (Func, TBox (..), Value (..), ext)

unString :: Value -> String
unString (String s) = s
unString _ = error "Error: expected string but got not string"

unString' :: Value -> String
unString' (String s) = s
unString' v = show v

lroutG :: Func
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
