{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pudding (
  conduitPuddingParser,
  conduitPuddingEvaluator,
  evalEnvT,
  initEnv,
  ) where

import Control.Applicative (Applicative, (<$>), pure)
import Control.Monad (liftM)
import Control.Monad.Error (MonadError, ErrorT, catchError, throwError, runErrorT)
import Control.Monad.State (MonadState, StateT, get, put, execStateT, runStateT, modify)
import Control.Monad.Trans (MonadTrans, MonadIO, lift, liftIO)
import Control.Monad.Writer (MonadWriter, WriterT, execWriterT, tell)
import Data.ByteString.Char8 as BC (ByteString, pack, unpack, append, concat)
import Data.Conduit as C (Conduit, await, yield, transPipe)
import qualified Data.Conduit.List as CL (mapM, concatMapAccum)
import Data.Map as Map (Map, fromList, lookup)
import Prelude hiding (div)
import Pudding.Parse
import System.IO


data PData = PDNumber Double
           | PDBool Bool
           | PDString ByteString
           deriving (Eq, Show)

data PState = Run
            | Compile

data Environment = Environment
                   { stack :: [PData]
                   , wordMap :: MonadIO m => Map ByteString (PProc m)
                   , state :: PState
                   }

newtype EnvT m a = EnvT { runEnvT :: ErrorT String (StateT Environment m) a }
                 deriving (Functor, Applicative, Monad, MonadIO, MonadState Environment, MonadError String)

instance MonadTrans EnvT where
  lift = undefined

type PProc m = EnvT m ()

-- environment operators

-- |
-- 初期環境をとってEnvTを実行し、新しい環境を得る
evalEnvT :: Monad m => Environment -> EnvT m () -> m (Either String (), Environment)
evalEnvT init program = flip runStateT init . runErrorT . runEnvT $ program

-- basic environment monad operators

-- |
-- 失敗だったら状態を戻して失敗を伝える、成功だったらそのまま
transaction :: Monad m => (String -> String) -> EnvT m a -> EnvT m a
transaction msg m = do
  origEnv <- get
  catchError m $ (put origEnv >>) . throwError . msg

push :: Monad m => PData -> EnvT m ()
push d = do
  env@Environment { stack = s } <- get
  put env { stack = d:s }

pop :: Monad m => EnvT m PData
pop = do
  env@Environment { stack = s } <- get
  case s of
    (a:as) -> put env { stack = as } >> return a
    _ -> throwError "empty stack"

setState :: Monad m => PState -> EnvT m ()
setState s = modify $ \e -> e { state = s }

ok :: MonadIO m => ByteString -> EnvT m ()
ok msg = liftIO . putStrLn $ "> " ++ unpack msg

ng :: MonadIO m => ByteString -> EnvT m ()
ng msg = liftIO . hPutStrLn stderr $ "*** " ++ unpack msg

-- pudding procedure

showTop :: MonadIO m => PProc m
showTop = pop >>= ok . pack . show

showStack :: MonadIO m => PProc m
showStack = get >>= ok . pack . show . stack

numericOp2 :: Monad m => (a -> PData) -> String -> (Double -> Double -> a) -> PProc m
numericOp2 ctor name op = transaction (const msg) $ do
  PDNumber a <- pop
  PDNumber b <- pop
  push . ctor $ op b a
  where
    msg = name ++ " needs 2 Numbers"

booleanOp2 :: Monad m => (a -> PData) -> String -> (Bool -> Bool -> a) -> PProc m
booleanOp2 ctor name op = transaction (const msg) $ do
  PDBool a <- pop
  PDBool b <- pop
  push . ctor $ op b a
  where
    msg = name ++ " nees 2 Booleans"

booleanOp1 :: Monad m => (a -> PData) -> String -> (Bool -> a) -> PProc m
booleanOp1 ctor name op = transaction (const msg) $ do
  PDBool a <- pop
  push . ctor $ op a
  where
    msg = name ++ " nees 1 Boolean"

dup :: Monad m => PProc m
dup = do
  x <- pop
  push x
  push x

initEnv :: Environment
initEnv = Environment { stack = []
                      , wordMap = fromList [(".", showTop)
                                           ,(".s", showStack)
                                           ,("dup", dup)
                                           ,("+", numericOp2 PDNumber "+" (+))
                                           ,("-", numericOp2 PDNumber "-" (-))
                                           ,("*", numericOp2 PDNumber "*" (*))
                                           ,("/", numericOp2 PDNumber "/" (/))
                                           ,("==", numericOp2 PDBool "==" (==))
                                           ,("!=", numericOp2 PDBool "!=" (/=))
                                           ,("<", numericOp2 PDBool "<" (<))
                                           ,("<=", numericOp2 PDBool "<=" (<=))
                                           ,(">", numericOp2 PDBool ">" (>))
                                           ,(">=", numericOp2 PDBool ">=" (>=))
                                           ,("&&", booleanOp2 PDBool "&&" (&&))
                                           ,("||", booleanOp2 PDBool "||" (||))
                                           ,("!", booleanOp1 PDBool "!" not)
                                           ]
                      , state = Run
                      }


data PContainer m = PData PData
                  | PProc ByteString (PProc m)

lookupWord :: MonadIO m => ByteString -> EnvT m (PProc m)
lookupWord x =  maybe (throwError $ "undefined word: " ++ unpack x) return . Map.lookup x . wordMap =<< get

fromToken :: MonadIO m => PToken -> EnvT m (PContainer m)
fromToken (PNumber x) = return . PData $ PDNumber x
fromToken (PBool x) = return . PData $ PDBool x
fromToken (PString x) = return . PData $ PDString x
fromToken (PWord x) = liftM (PProc x) (lookupWord x)

-- |
-- >>> :m +Data.Conduit Data.Conduit.List
-- >>> runResourceT $ sourceList [PNumber 1.0,PNumber 2.0,PNumber 3.0, PWord $ pack ".s", PWord $ pack "+", PWord $ pack "+", PWord $ pack "."] $= conduitPuddingEvaluator $$ consume
-- ["> [PDNumber 3.0,PDNumber 2.0,PDNumber 1.0]\n","> PDNumber 6.0\n"]
conduitPuddingEvaluator :: Monad m => Conduit PToken (EnvT m) ()
conduitPuddingEvaluator = undefined

compile :: Conduit PToken (EnvT m) ()
compile = undefined -- がんばって書けばいいはず

execute :: MonadIO m => Conduit PToken (EnvT m) ()
execute = await >>= maybe (return ()) step
  where
    step t = do
      lift $ (fromToken t >>= eval) `catchError` handler
      execute -- わざと再帰的に書いている。あとでcompileを呼ぶため。

    eval :: Monad m => PContainer m -> EnvT m ()
    eval (PData x) = push x
    eval (PProc _ p) = p

    handler :: MonadIO m => String -> EnvT m ()
    handler = ng . pack
