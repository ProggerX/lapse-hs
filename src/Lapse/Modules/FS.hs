{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Lapse.Modules.FS where

import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.Map.Strict (fromList)
import Lapse.Types (Func, Scope, Value (..))
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory, setCurrentDirectory)
import System.IO (readFile')

mod :: Scope
mod =
  fromList
    [ ("readF", Function lreadF)
    , ("writeF", Function lwriteF)
    , ("appendF", Function lappendF)
    , ("chdir", Function chdir)
    , ("lsdir", Function lsdir)
    ]

pattern Str :: String -> Value
pattern Str x = (Pair (String x) Nil)

chdir :: Func
chdir (Str dname) =
  liftIO $
    doesDirectoryExist dname >>= \case
      True -> setCurrentDirectory dname >> pure Nil
      False -> error $ "directory '" ++ dname ++ "' does not exist"
chdir _ = error "chdir accepts only one string - directory name"

lsdir :: Func
lsdir (Str dname) =
  liftIO $
    doesDirectoryExist dname >>= \case
      True -> listDirectory dname <&> foldr (Pair . String) Nil
      False -> error $ "directory '" ++ dname ++ "' does not exist"
lsdir Nil =
  liftIO $ getCurrentDirectory >>= fmap (foldr (Pair . String) Nil) . listDirectory
lsdir _ = error "lsdir accepts only one or zero arguments"

lreadF :: Func
lreadF (Str fname) = String <$> liftIO (readFile' fname)
lreadF _ = error "readF accepts only one argument - filename"

lwriteF :: Func
lwriteF (Pair (String fname) (Pair (String contents) Nil)) = Nil <$ liftIO (writeFile fname contents)
lwriteF _ = error "writeF accepts only two arguments - filename and contents"

lappendF :: Func
lappendF (Pair (String fname) (Pair (String contents) Nil)) = Nil <$ liftIO (appendFile fname contents)
lappendF _ = error "writeF accepts only two arguments - filename and contents"
