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
import Control.Monad (liftM)
import Data.ByteString.Char8 as BC (ByteString, pack, append, unpack)
import Data.Conduit as C (Conduit, (=$=))
import qualified Data.Conduit.List as CL (map, concatMapAccum)
import Data.Functor.Identity (Identity)
import Data.Map as Map (Map, fromList, lookup, insert)
import Data.Tuple (swap)
import Data.Vector as V (Vector, (!?), fromList)
import Prelude hiding (div)
import Pudding.Parse


data PValue = PVNumber Double
            | PVBool Bool
            | PVString ByteString
            deriving (Eq, Show)

data PState = Run
            | Compile ByteString [PToken]
            | NewWord

type TokenBlock = Vector PToken

data Meaning = NormalWord ByteString PProc -- 今はまだxtのみ。あとからct追加予定
             | CompileOnlyWord ByteString PProc -- 今はまだxtのみ。あとからct追加予定
             | ImmediateWord ByteString PProc -- xtのみ。";"がこれになるらしい
             | UserDefinedWord ByteString TokenBlock -- 今はまだxtのみ。あとから定義できるワードの種類を増やす予定

data CallBlock = CallBlock
                 { pcStashed :: Int
                 , word :: ByteString
                 , tokenBlock :: TokenBlock
                 }

data Environment = Environment
                   { stack :: [PValue]
                   , wordMap :: Map ByteString Meaning
                   , state :: PState
                   , pc :: Int
                   , callStack :: [CallBlock]
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

push :: PValue -> Env ()
push d = do
  env@Environment { stack = s } <- get
  put env { stack = d:s }

pop :: Env PValue
pop = do
  env@Environment { stack = s } <- get
  case s of
    (a:as) -> put env { stack = as } >> return a
    _ -> throwError "empty stack"

setState :: PState -> Env ()
setState s = modify $ \e -> e { state = s }

getState :: Env PState
getState = liftM state get

nativeProcedure :: ByteString -> PProc -> Meaning
nativeProcedure = NormalWord

pushCallStack :: ByteString -> TokenBlock -> Env ()
pushCallStack n tb = do
  env <- get
  put env { callStack = makeCallBlock env : callStack env,
            pc = 0 }
    where
      makeCallBlock :: Environment -> CallBlock
      makeCallBlock env = CallBlock (pc env) n tb

popCallStack :: Env ()
popCallStack = do
  env <- get
  put env { callStack = tail $ callStack env,
            pc = pcStashed . head $ callStack env }

setPc :: Int -> Env ()
setPc i = do
  env <- get
  put env { pc = i }

incPc :: Env ()
incPc = getPc >>= \i -> setPc $ succ i

getPc :: Env Int
getPc = liftM pc get

inTopLevel :: Env Bool
inTopLevel = do
  env <- get
  return . null $ callStack env

insertWord :: ByteString -> Meaning -> Env ()
insertWord name meaning = do
  env <- get
  put $ env { wordMap = Map.insert name meaning $ wordMap env }

-- pudding procedure

showTop :: PProc
showTop = pure . pack . show <$> pop

showStack :: PProc
showStack = pure . pack . show . stack <$> get

showCallStack :: PProc
showCallStack = pure . pack . show . map word . callStack <$> get

numericOp2 :: (a -> PValue) -> String -> (Double -> Double -> a) -> PProc
numericOp2 ctor name op = transaction (const msg) $ do
  PVNumber a <- pop
  PVNumber b <- pop
  push . ctor $ op b a
  return []
  where
    msg = name ++ " needs 2 Numbers"

booleanOp2 :: (a -> PValue) -> String -> (Bool -> Bool -> a) -> PProc
booleanOp2 ctor name op = transaction (const msg) $ do
  PVBool a <- pop
  PVBool b <- pop
  push . ctor $ op b a
  return []
  where
    msg = name ++ " needs 2 Booleans"

booleanOp1 :: (a -> PValue) -> String -> (Bool -> a) -> PProc
booleanOp1 ctor name op = transaction (const msg) $ do
  PVBool a <- pop
  push . ctor $ op a
  return []
  where
    msg = name ++ " needs 1 Boolean"

dup :: PProc
dup = do
  x <- pop
  push x
  push x
  return []

jump :: PProc
jump = do
  PVNumber a <- pop
  PVBool cond <- pop
  if cond
    then getPc >>= (\i -> setPc $ i + floor a) >> return []
    else return []

nop :: PProc
nop = return []

startCompile :: PProc
startCompile = setState NewWord >> return []

endCompile :: PProc
endCompile = do
  Compile name tokens <- getState
  setState Run
  insertWord name . UserDefinedWord name . V.fromList $ reverse tokens
  return []

initEnv :: Environment
initEnv = Environment { stack = []
                      , wordMap = Map.fromList [(".", nativeProcedure "." showTop)
                                               ,(".s", nativeProcedure ".s" showStack)
                                               ,(".cs", nativeProcedure ".cs" showCallStack)
                                               ,("dup", nativeProcedure "dup" dup)
                                               ,("+", nativeProcedure "+" $ numericOp2 PVNumber "+" (+))
                                               ,("-", nativeProcedure "-" $ numericOp2 PVNumber "-" (-))
                                               ,("*", nativeProcedure "*" $ numericOp2 PVNumber "*" (*))
                                               ,("/", nativeProcedure "/" $ numericOp2 PVNumber "/" (/))
                                               ,("==", nativeProcedure "==" $ numericOp2 PVBool "==" (==))
                                               ,("!=", nativeProcedure "!=" $ numericOp2 PVBool "!=" (/=))
                                               ,("<", nativeProcedure "<" $ numericOp2 PVBool "<" (<))
                                               ,("<=", nativeProcedure "<=" $ numericOp2 PVBool "<=" (<=))
                                               ,(">", nativeProcedure ">" $ numericOp2 PVBool ">" (>))
                                               ,(">=", nativeProcedure ">=" $ numericOp2 PVBool ">=" (>=))
                                               ,("&&", nativeProcedure "&&" $ booleanOp2 PVBool "&&" (&&))
                                               ,("||", nativeProcedure "||" $ booleanOp2 PVBool "||" (||))
                                               ,("!", nativeProcedure "!" $ booleanOp1 PVBool "!" not)
                                               ,("nop", nativeProcedure "nop" nop)
                                               ,(":", nativeProcedure ":" startCompile)
                                               ,(";", ImmediateWord ";" endCompile)
                                               ,("jump", CompileOnlyWord "jump" jump)
                                               ,("_test", UserDefinedWord "_test" $ V.fromList [PWord ".cs", PNumber 3, PNumber 3, PWord "*", PWord "."])
                                               ,("_testJump1", UserDefinedWord "_testJump1" $ V.fromList [PWord ".cs", PBool True, PNumber 3, PWord "jump", PString "a", PString "b", PString "c", PString "d"])
                                               ,("_testJump2", UserDefinedWord "_testJump2" $ V.fromList [PWord ".cs", PBool False, PNumber 3, PWord "jump", PString "a", PString "b", PString "c", PString "d"])
                                               ]
                      , state = Run
                      , pc = 0
                      , callStack = []
                      }


data PContainer = PValue PValue
                | PProc ByteString PProc

lookupXt :: ByteString -> Env PProc
lookupXt x = do
  env <- get
  case Map.lookup x $ wordMap env of
    Just (NormalWord _ p) -> return p
    Just (ImmediateWord _ p) -> return p
    Just (UserDefinedWord name x') -> return $ evalXt name x'
    Just (CompileOnlyWord _ p) ->
      do
        top <- inTopLevel
        if top
          then throwError $ "Can't execute: " ++ unpack x
          else return p
    Nothing -> throwError $ "undefined word: " ++ unpack x

evalXt :: ByteString -> TokenBlock -> PProc
evalXt name xt = pushCallStack name xt >> eval' xt
  where
    eval' :: TokenBlock -> PProc
    eval' tb = do
      c <- getPc
      maybe f s $ tb !? c
      where
        s :: PToken -> PProc
        s t = do
          result <- eval t
          incPc
          liftM (result++) $ eval' tb

        f :: PProc
        f = popCallStack >> return []

fromToken :: PToken -> Env PContainer
fromToken (PNumber x) = return . PValue $ PVNumber x
fromToken (PBool x) = return . PValue $ PVBool x
fromToken (PString x) = return . PValue $ PVString x
fromToken (PWord x) = PProc x <$> lookupXt x

eval :: PToken -> Env [ByteString]
eval t = fromToken t >>= eval'
  where
    eval' :: PContainer -> Env [ByteString]
    eval' (PValue x) = push x >> return []
    eval' (PProc _ p) = p


-- |
-- >>> :m +Data.Conduit Data.Conduit.List
-- >>> runResourceT $ sourceList [PNumber 1.0,PNumber 2.0,PNumber 3.0, PWord $ pack ".s", PWord $ pack "+", PWord $ pack "+", PWord $ pack "."] $= conduitPuddingEvaluator $$ consume
-- ["> [PVNumber 3.0,PVNumber 2.0,PVNumber 1.0]\n","> PVNumber 6.0\n"]
conduitPuddingEvaluator :: Monad m => Conduit PToken m ByteString
conduitPuddingEvaluator = CL.concatMapAccum step initEnv =$= CL.map (`append` "\n")
  where
    step :: PToken -> Environment -> (Environment, [ByteString])
    step t e = swap $ runState s e
      where
        s :: State Environment [ByteString]
        s = either (pure . pack . ("*** "++)) id <$> (runErrorT . runEnvT) (map (append "> ") <$> eval t)
