{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Pudding.Core (
  module Pudding.Parse,
  module Pudding.Core,
  get,
  catchError,
  throwError,
  tell,
  ) where

import Control.Monad.Error (MonadError, ErrorT, catchError, throwError)
import Control.Monad.State (MonadState, StateT, get, put, modify)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Writer (MonadWriter, WriterT, tell)
import Data.ByteString.Char8 as BC (ByteString, unpack)
import Data.Map as Map (Map, lookup, insert)
import Data.Vector as V (Vector, (!?))
import Pudding.Imports
import Pudding.Parse

-- $setup
-- >>> import Data.Conduit
-- >>> import Data.Conduit.List
-- >>> :set -XOverloadedStrings


data PValue = PVNumber Double
            | PVBool Bool
            | PVString ByteString
            deriving (Eq, Show)

showPV :: PValue -> String
showPV (PVNumber n) = show n
showPV (PVBool b) = show b
showPV (PVString s) = unpack s

data PState = Run
            | Compile ByteString [PToken]
            | NewWord
            deriving (Show)

type TokenBlock = Vector PToken

type CompileProc = PToken -> [PToken] -> Either String [PToken]

data Meaning m = NormalWord ByteString (PProc m) CompileProc
               | CompileOnlyWord ByteString (PProc m) CompileProc
               | ImmediateWord ByteString (PProc m)
               | UserDefinedWord ByteString TokenBlock -- 今はまだxtのみ。あとから定義できるワードの種類を増やす予定

data CallBlock = CallBlock
                 { pcStashed :: Int
                 , word :: ByteString
                 , tokenBlock :: TokenBlock
                 }

data Environment m = Environment
                     { stack :: [PValue]
                     , wordMap :: Map ByteString (Meaning m)
                     , state :: PState
                     , pc :: Int
                     , callStack :: [CallBlock]
                     }

newtype EnvT m a = EnvT { runEnvT :: ErrorT String (WriterT [ByteString] (StateT (Environment m) m)) a }
                 deriving (Functor, Applicative, Monad, MonadIO, MonadState (Environment m), MonadWriter [ByteString], MonadError String)

--type Env = EnvT Identity

type PProc m = EnvT m ()

-- basic environment operators

-- |
-- 失敗だったら状態を戻して失敗を伝える、成功だったらそのまま
transaction :: Monad m => (String -> String) -> EnvT m a -> EnvT m a
transaction msg m = do
  origEnv <- get
  catchError m $ (put origEnv >>) . throwError . msg

push :: Monad m => PValue -> EnvT m ()
push d = do
  env@Environment { stack = s } <- get
  put env { stack = d:s }

pop :: Monad m => EnvT m PValue
pop = do
  env@Environment { stack = s } <- get
  case s of
    (a:as) -> put env { stack = as } >> return a
    _ -> throwError "empty stack"

popN :: Monad m => Int -> EnvT m [PValue]
popN = flip replicateM pop

setState :: Monad m => PState -> EnvT m ()
setState s = modify $ \e -> e { state = s }

getState :: Monad m => EnvT m PState
getState = liftM state get

cpush :: CompileProc
cpush a b = Right $ a : b

nativeProcedure :: Monad m => ByteString -> PProc m -> Meaning m
nativeProcedure name p = NormalWord name p cpush

pushCallStack :: Monad m => ByteString -> TokenBlock -> EnvT m ()
pushCallStack n tb = do
  env <- get
  put env { callStack = makeCallBlock env : callStack env,
            pc = 0 }
    where
      makeCallBlock :: Environment m -> CallBlock
      makeCallBlock env = CallBlock (pc env) n tb

popCallStack :: Monad m => EnvT m ()
popCallStack = do
  env <- get
  put env { callStack = tail $ callStack env,
            pc = pcStashed . head $ callStack env }

modifyPc :: Monad m => (Int -> Int) -> EnvT m ()
modifyPc f = modify $ \env -> env { pc = f $ pc env }

incPc :: Monad m => EnvT m ()
incPc = modifyPc succ

getPc :: Monad m => EnvT m Int
getPc = liftM pc get

inTopLevel :: Monad m => EnvT m Bool
inTopLevel = do
  env <- get
  return . null $ callStack env

insertWord :: Monad m => ByteString -> Meaning m -> EnvT m ()
insertWord name meaning = do
  env <- get
  put $ env { wordMap = Map.insert name meaning $ wordMap env }

data PContainer m = PValue PValue
                  | PProc ByteString (PProc m)

lookupXt :: Monad m => ByteString -> EnvT m (PProc m)
lookupXt x = do
  env <- get
  case Map.lookup x $ wordMap env of
    Just (NormalWord _ p _) -> return p
    Just (ImmediateWord _ p) -> return p
    Just (UserDefinedWord name x') -> return $ evalXt name x'
    Just (CompileOnlyWord _ p _) ->
      do
        top <- inTopLevel
        if top
          then throwError $ "Can't execute: " ++ unpack x
          else return p
    Nothing -> throwError $ "undefined word: " ++ unpack x

evalXt :: Monad m => ByteString -> TokenBlock -> PProc m
evalXt name xt = pushCallStack name xt >> eval' xt
  where
    eval' :: Monad m => TokenBlock -> PProc m
    eval' tb = do
      c <- getPc
      maybe f s $ tb !? c
      where
        s :: Monad m => PToken -> PProc m
        s t = do
          eval t
          incPc
          eval' tb

        f :: Monad m => PProc m
        f = popCallStack

fromToken :: Monad m => PToken -> EnvT m (PContainer m)
fromToken (PNumber x) = return . PValue $ PVNumber x
fromToken (PBool x) = return . PValue $ PVBool x
fromToken (PString x) = return . PValue $ PVString x
fromToken (PWord x) = liftM (PProc x) $ lookupXt x

run :: Monad m => PToken -> EnvT m ()
run t = fromToken t >>= eval'
  where
    eval' :: Monad m => PContainer m -> EnvT m ()
    eval' (PValue x) = push x
    eval' (PProc _ p) = p

lookupCt :: Monad m => ByteString -> EnvT m (Either (PProc m) CompileProc)
lookupCt x = do
  env <- get
  case Map.lookup x $ wordMap env of
    Just (NormalWord _ _ f) -> return $ Right f
    Just (ImmediateWord _ p) -> return $ Left p
    Just (UserDefinedWord _ _) -> return $ Right cpush -- ユーザ定義ワードのコンパイル時意味は「実行時にこのワードを実行」のみ。forthと違い、実行時に解決される。(ワードを再定義した場合既存のワードの動作が変更される)
    Just (CompileOnlyWord _ _ f) -> return $ Right f
    Nothing -> throwError $ "undefined word: " ++ unpack x

pushToken :: Monad m => PToken -> EnvT m ()
pushToken t = do
  Compile name ts <- getState
  setState . Compile name $ t : ts

updateTokens :: Monad m => ([PToken] -> Either String [PToken]) -> EnvT m ()
updateTokens f = do
  Compile name ts <- getState
  either throwError (setState . Compile name) $ f ts

compile :: Monad m => PToken -> EnvT m ()
compile t@(PWord w) = do
  x <- lookupCt w
  case x of
    Left p -> p
    Right cp -> updateTokens (cp t)
compile t = pushToken t

newWord :: Monad m => PToken -> EnvT m ()
newWord (PWord name) = setState (Compile name [])
newWord _ = throwError "specify a new word"

eval :: Monad m => PToken -> EnvT m ()
eval t = do
  s <- getState
  case s of
    Run -> run t
    NewWord -> newWord t
    Compile _ _ -> compile t
