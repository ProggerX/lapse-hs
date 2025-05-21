{-# LANGUAGE TemplateHaskell #-}

module Lapse.Modules where

import Control.Exception (onException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (evalStateT, runStateT)
import Data.ByteString.Char8 qualified as BC
import Data.FileEmbed (embedFileRelative)
import Data.Map.Strict (Map, empty, fromList, (!?))
import Lapse.Eval (eval)
import Lapse.Lambda (compact, define, defmacro, lambda, macro)
import Lapse.Modules.FS qualified as FS
import Lapse.Modules.Json qualified as Json
import Lapse.Modules.Web qualified as Web
import Lapse.Operators
import Lapse.Parser (parse)
import Lapse.Scopes (addScope, addScopes)
import Lapse.Types (Func, LapseM, Scope, Scopes, Value (..))
import System.IO (
  IOMode (ReadMode),
  readFile',
  withFile,
 )

std :: Scope
std =
  fromList
    [ ("+", Function ladd)
    , ("*", Function lmul)
    , ("/", Function ldiv)
    , ("-", Function lsub)
    , ("<", Function llss)
    , (">", Function lgrt)
    , ("==", Function leql)
    , ("not", Function lnot)
    , ("^", Function lpow)
    , ("sqrt", Function lsqr)
    , ("let", Macros llet)
    , ("set", Macros lset)
    , ("cond", Macros cond)
    , ("map", Function lmap)
    , ("double", Function ldouble)
    , ("list", Function llist)
    , ("gensym", Function gensym)
    , ("eval", Function leval)
    , ("do", Function leval)
    , ("nil", Nil)
    , ("raw", Macros lraw)
    , ("fst", Function lfst)
    , ("snd", Function lsnd)
    , ("fact", Function lfac)
    , ("concat", Function lcon)
    , ("show", Function lshow)
    , ("lambda", Macros lambda)
    , ("defn", Macros define)
    , ("macro", Macros macro)
    , ("defmacro", Macros defmacro)
    , ("read", Function lread)
    , ("dict", Macros ldict)
    , ("lookup", Macros llkp)
    , ("insert", Macros lins)
    , ("float", Function lflt)
    , ("floor", Function lfloor)
    , ("throw", Function lthr)
    , ("compact", Macros compact)
    ]

io :: Scope
io =
  fromList
    [ ("print", Function lprint)
    , ("write", Function lwrite)
    , ("getline", Function lgetl)
    , ("flush", Function lflush)
    ]

builtins :: Map String Scope
builtins =
  fromList
    [ ("std", std)
    , ("io", io)
    , ("web", Web.mod)
    , ("json", Json.mod)
    , ("fs", FS.mod)
    ]

lapseBuiltins :: Map String String
lapseBuiltins =
  fromList
    [ ("gcode", BC.unpack $(embedFileRelative "modules/gcode.lp"))
    , ("colors", BC.unpack $(embedFileRelative "modules/colors.lp"))
    ]

fileExists :: FilePath -> IO Bool
fileExists path =
  withFile path ReadMode (\_ -> pure True) `onException` pure False

getScopesIO' :: LapseM a -> IO Scopes
getScopesIO' = (snd <$>) . (`evalStateT` 0) . (`runStateT` initIOState)

getScopesIO :: String -> IO Scopes
getScopesIO = getScopesIO' . mapM eval . parse

limport :: Func
limport (Pair (String s) Nil) = case builtins !? s of
  Just x -> addScope x >> pure Nil
  Nothing -> case lapseBuiltins !? s of
    Just x -> Nil <$ (liftIO (getScopesIO x) >>= addScopes)
    Nothing -> do
      exists <- liftIO $ fileExists s
      if exists
        then do
          fileText <- liftIO $ readFile' s
          scopes <- liftIO $ getScopesIO fileText
          addScopes scopes
          pure Nil
        else error $ "Can't find module: " ++ s
limport (Pair (String s) a) = limport (Pair (String s) Nil) >> limport a
limport _ = error "import argument must be string"

prelude :: Scope
prelude =
  fromList
    [("import", Macros limport)]

replState :: Scopes
replState = [empty, prelude, std]

initIOState :: Scopes
initIOState = [empty, prelude]
