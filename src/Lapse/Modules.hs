module Lapse.Modules where

import Control.Exception (onException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (evalStateT, runStateT)
import Data.Map.Strict (Map, empty, fromList, (!?))
import Lapse.Eval (eval)
import Lapse.Modules.Json qualified as Json
import Lapse.Lambda (define, defmacro, lambda, macro)
import Lapse.Operators
import Lapse.Parser (parse)
import Lapse.Scopes (addScope, addScopes)
import Lapse.Types (Func, LapseM, Scope, Scopes, Value (..))
import Lapse.Modules.Web qualified as Web
import System.IO (
  IOMode (ReadMode),
  readFile',
  withFile,
 )

std :: (Monad m) => Scope m
std =
  fromList
    [ ("+", Function ladd)
    , ("*", Function lmul)
    , ("/", Function ldiv)
    , ("-", Function lsub)
    , ("<", Function llss)
    , (">", Function lgrt)
    , ("==", Function leql)
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
    ]

io :: Scope IO
io =
  fromList
    [ ("print", Function lprint)
    , ("write", Function lwrite)
    , ("getline", Function lgetl)
    , ("flush", Function lflush)
    ]

builtins :: Map String (Scope IO)
builtins =
  fromList
    [ ("std", std)
    , ("io", io)
    , ("web", Web.mod)
    , ("json", Json.mod)
    ]

fileExists :: FilePath -> IO Bool
fileExists path =
  withFile path ReadMode (\_ -> pure True) `onException` pure False

getScopesIO' :: LapseM IO a -> IO (Scopes IO)
getScopesIO' = (snd <$>) . (`evalStateT` 0) . (`runStateT` initIOState)

getScopesIO :: String -> IO (Scopes IO)
getScopesIO = getScopesIO' . mapM eval . parse

limport :: Func IO
limport (Pair (String s) Nil) = case builtins !? s of
  Just x -> addScope x >> pure Nil
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

prelude :: Scope IO
prelude =
  fromList
    [("import", Macros limport)]

replState :: Scopes IO
replState = [empty, prelude, std]

initIOState :: Scopes IO
initIOState = [empty, prelude]
