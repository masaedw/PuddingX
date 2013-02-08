{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pudding (
  conduitPuddingParser,
  conduitPuddingEvaluator,
  ) where

import Control.Applicative (Applicative, (<$>), pure)
import Control.Monad.Error (MonadError, ErrorT, runErrorT, catchError, throwError)
import Control.Monad.State (MonadState, StateT, State, get, put, runState, modify)
import Control.Monad.Trans (MonadIO)
import Data.ByteString.Char8 as BC (ByteString, pack, append)
import Data.Conduit as C (Conduit, (=$=))
import qualified Data.Conduit.List as CL (map, concatMapAccum)
import Data.Functor.Identity (Identity)
import Data.Map as Map (Map, fromList, lookup)
import Data.Tuple (swap)
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
                   , wordMap :: Map ByteString PProc
                   , state :: PState
                   }

newtype EnvT m a = EnvT { runEnvT :: ErrorT String (StateT Environment m) a }
                 deriving (Functor, Applicative, Monad, MonadIO, MonadState Environment, MonadError String)

type Env = EnvT Identity

type PProc = Env [ByteString]

-- basic environment operators

-- |
-- 失敗だったら状態を戻して失敗を伝える、成功だったらそのまま
transaction :: (String -> String) -> Env a -> Env a
transaction msg m = do
  origEnv <- get
  catchError m $ (put origEnv >>) . throwError . msg

push :: PData -> Env ()
push d = do
  env@Environment { stack = s } <- get
  put env { stack = d:s }

pop :: Env PData
pop = do
  env@Environment { stack = s } <- get
  case s of
    (a:as) -> put env { stack = as } >> return a
    _ -> throwError "empty stack"

-- pudding procedure

showTop :: PProc
showTop = pure . pack . show <$> pop

showStack :: PProc
showStack = pure . pack . show . stack <$> get

numericOp2 :: (a -> PData) -> String -> (Double -> Double -> a) -> PProc
numericOp2 ctor name op = transaction (const msg) $ do
  PDNumber a <- pop
  PDNumber b <- pop
  push . ctor $ op b a
  return []
  where
    msg = name ++ " needs 2 Numbers"

dup :: PProc
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
                                           ]
                      , state = Run
                      }


data PContainer = PData PData
                | PProc ByteString PProc

lookupWord :: ByteString -> Env PProc
lookupWord x =  maybe (throwError "undefined word") return . Map.lookup x . wordMap =<< get

fromToken :: PToken -> Env PContainer
fromToken (PNumber x) = return . PData $ PDNumber x
fromToken (PBool x) = return . PData $ PDBool x
fromToken (PString x) = return . PData $ PDString x
fromToken (PWord x) = PProc x <$> lookupWord x

-- |
-- >>> :m +Data.Conduit Data.Conduit.List
-- >>> runResourceT $ sourceList [PNumber 1.0,PNumber 2.0,PNumber 3.0, PWord $ pack ".s", PWord $ pack "+", PWord $ pack "+", PWord $ pack "."] $= conduitPuddingEvaluator $$ consume
-- ["> [PDNumber 3.0,PDNumber 2.0,PDNumber 1.0]\n","> PDNumber 6.0\n"]
conduitPuddingEvaluator :: Monad m => Conduit PToken m ByteString
conduitPuddingEvaluator = CL.concatMapAccum step initEnv =$= CL.map (`append` "\n")
  where
    step :: PToken -> Environment -> (Environment, [ByteString])
    step t e = swap $ runState s e
      where
        s :: State Environment [ByteString]
        s = either (pure . pack . ("*** "++)) id <$> (runErrorT . runEnvT) (fromToken t >>= eval)

    eval :: PContainer -> Env [ByteString]
    eval (PData x) = push x >> return []
    eval (PProc _ p) = map (append "> ") <$> p
