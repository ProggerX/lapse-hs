module Lapse.Modules.Web.Server.Post where

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

lroutP :: Func
lroutP (Pair (String url) (Pair args' (Pair (Function f) (Pair (External (TBox srv)) Nil)))) = do
  st <- get
  cnt <- lift get
  case cast @_ @WServer srv of
    Just s@WServer{routesPOST} ->
      let tm x =
            pure $
              ext
                s
                  { routesPOST =
                      insert
                        url
                        ( x
                        , \body params ->
                            (`evalStateT` cnt)
                              . (`evalStateT` st)
                              . fmap (BCL.pack . unString')
                              . f
                              $ foldr (Pair . String) Nil (body : params)
                        )
                        routesPOST
                  }
       in case unList args' of
            Single (String arg) -> tm [arg]
            Proper args -> tm $ map unString args
            _ -> lroutP Nil
    Nothing -> lroutP Nil
lroutP _ = error "routeGET error, valid syntax: routeGET <url> <args> <func>"
