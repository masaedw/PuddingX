{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pudding (
  conduitPuddingParser,
  conduitPuddingEvaluator,
  ) where

import Control.Applicative (Applicative, (<$>), pure)
import Control.Monad.Error (MonadError, ErrorT, catchError, throwError)
import Control.Monad.State (MonadState, StateT, get, put, runState, modify)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Writer (MonadWriter, WriterT, tell)
import Data.ByteString.Char8 as BC (ByteString, pack, unpack, append, concat)
import Data.Conduit as C (Conduit)
import qualified Data.Conduit.List as CL (mapM)
import Data.Map as Map (Map, fromList, lookup)
import Prelude hiding (div)
import Pudding.Parse


data PData = PDNumber Double
           | PDBool Bool
           | PDString ByteString
           deriving (Eq, Show)

data PState = Run
            | Compile

data Environment = Environment
                   { stack :: [PData]
                   , wordMap :: Monad m => Map ByteString (PProc m)
                   , state :: PState
                   }

newtype EnvT m a = EnvT { runEnvT :: WriterT ByteString (ErrorT String (StateT Environment m)) a }
                 deriving (Functor, Applicative, Monad, MonadIO, MonadState Environment, MonadError String, MonadWriter ByteString)

type PProc m = EnvT m [ByteString]

-- basic environment operators

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

ok :: Monad m => ByteString -> EnvT m ()
ok msg = tell $ BC.concat ["> ", msg, "\n"]

ng :: Monad m => ByteString -> EnvT m ()
ng msg = tell $ BC.concat ["*** ", msg, "\n"]

-- pudding procedure

showTop :: Monad m => PProc m
showTop = pop >>= return . pure . pack . show

showStack :: Monad m => PProc m
showStack = get >>= return . pure . pack . show . stack

numericOp2 :: Monad m => (a -> PData) -> String -> (Double -> Double -> a) -> PProc m
numericOp2 ctor name op = transaction (const msg) $ do
  PDNumber a <- pop
  PDNumber b <- pop
  push . ctor $ op b a
  return []
  where
    msg = name ++ " needs 2 Numbers"

booleanOp2 :: Monad m => (a -> PData) -> String -> (Bool -> Bool -> a) -> PProc m
booleanOp2 ctor name op = transaction (const msg) $ do
  PDBool a <- pop
  PDBool b <- pop
  push . ctor $ op b a
  return []
  where
    msg = name ++ " nees 2 Booleans"

booleanOp1 :: Monad m => (a -> PData) -> String -> (Bool -> a) -> PProc m
booleanOp1 ctor name op = transaction (const msg) $ do
  PDBool a <- pop
  push . ctor $ op a
  return []
  where
    msg = name ++ " nees 1 Boolean"

dup :: Monad m => PProc m
dup = do
  x <- pop
  push x
  push x
  return []

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

lookupWord :: Monad m => ByteString -> EnvT m (PProc m)
lookupWord x =  maybe (throwError $ "undefined word: " ++ unpack x) return . Map.lookup x . wordMap =<< get

fromToken :: Monad m => PToken -> EnvT m (PContainer m)
fromToken (PNumber x) = return . PData $ PDNumber x
fromToken (PBool x) = return . PData $ PDBool x
fromToken (PString x) = return . PData $ PDString x
fromToken (PWord x) = lookupWord x >>= return . PProc x

-- |
-- >>> :m +Data.Conduit Data.Conduit.List
-- >>> runResourceT $ sourceList [PNumber 1.0,PNumber 2.0,PNumber 3.0, PWord $ pack ".s", PWord $ pack "+", PWord $ pack "+", PWord $ pack "."] $= conduitPuddingEvaluator $$ consume
-- ["> [PDNumber 3.0,PDNumber 2.0,PDNumber 1.0]\n","> PDNumber 6.0\n"]
conduitPuddingEvaluator :: (Monad m, Functor m) => Conduit PToken m ByteString
conduitPuddingEvaluator = undefined

conduitPuddingEvaluator' :: (Monad m, Functor m) => Conduit PToken (EnvT m) ByteString
conduitPuddingEvaluator' = CL.mapM $ \t -> (fromToken t >>= eval) `catchError` handler
   where
    eval :: (Monad m, Functor m) => PContainer m -> EnvT m ByteString
    eval (PData x) = push x >> return ""
    eval (PProc _ p) = BC.concat <$> map ((append "> ") . (`append` "\n")) <$> p

    handler :: (Monad m) => String -> EnvT m ByteString
    handler e = return $ pack ("*** " ++ e)  where
