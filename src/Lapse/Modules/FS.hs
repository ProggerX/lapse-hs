module Lapse.Modules.FS where

import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict (fromList)
import Lapse.Types (Func, Scope, Value (..))
import System.IO (readFile')

mod :: Scope
mod =
  fromList
    [ ("readF", Function lreadF)
    , ("writeF", Function lwriteF)
    , ("appendF", Function lappendF)
    ]

lreadF :: Func
lreadF (Pair (String fname) Nil) = String <$> liftIO (readFile' fname)
lreadF _ = error "readF accepts only one argument - filename"

lwriteF :: Func
lwriteF (Pair (String fname) (Pair (String contents) Nil)) = Nil <$ liftIO (writeFile fname contents)
lwriteF _ = error "writeF accepts only two arguments - filename and contents"

lappendF :: Func
lappendF (Pair (String fname) (Pair (String contents) Nil)) = Nil <$ liftIO (appendFile fname contents)
lappendF _ = error "writeF accepts only two arguments - filename and contents"
