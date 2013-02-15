{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pudding (
  conduitPuddingParser,
  conduitPuddingEvaluator,
  ) where

import Control.Applicative (Applicative, (<$>), pure)
import Control.Monad (liftM)
import Control.Monad.Error (MonadError, ErrorT, runErrorT, catchError, throwError)
import Control.Monad.State (MonadState, StateT, State, get, put, runState, modify)
import Control.Monad.Trans (MonadIO)
import Data.ByteString.Char8 as BC (ByteString, pack, append, unpack)
import Data.Conduit as C (Conduit, (=$=))
import qualified Data.Conduit.List as CL (map, concatMapAccum)
import Data.Functor.Identity (Identity)
import Data.List (intersperse)
import Data.Map as Map (Map, fromList, lookup, insert)
import Data.Tuple (swap)
import Data.Vector as V (Vector, (!?), fromList)
import Prelude hiding (div)
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

data Meaning = NormalWord ByteString PProc CompileProc
             | CompileOnlyWord ByteString PProc CompileProc
             | ImmediateWord ByteString PProc
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
nativeProcedure name p = NormalWord name p cpush

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
showTop = pure . pack . showPV <$> pop

showStack :: PProc
showStack = pure . pack . ('[':) . (++"]") . concat . intersperse ", " . map showPV . stack <$> get

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

pdrop :: PProc
pdrop = do
  _ <- pop
  return []

dup :: PProc
dup = do
  x <- pop
  push x
  push x
  return []

dup2 :: PProc
dup2 = do
  x <- pop
  y <- pop
  push y
  push x
  push y
  push x
  return []

jump :: PProc
jump = jump' `catchError` (return $ throwError "stack top is not Boolean")
  where
    jump' :: PProc
    jump' = do
      PVNumber a <- pop
      PVBool cond <- pop
      if not cond
        then getPc >>= (\i -> setPc $ i + floor a) >> return []
        else return []

fjump :: PProc
fjump = do
  PVNumber a <- pop
  getPc >>= (\i -> setPc $ i + floor a) >> return []

nop :: PProc
nop = return []

pswap :: PProc
pswap = do
  a <- pop
  b <- pop
  push b
  push a
  return []

-- |
-- >>> cthen (PWord "then") [PNumber 1, PNumber 2, PWord "if", PBool True]
-- Right [PNumber 1.0,PNumber 2.0,PWord "jump",PNumber 2.0,PBool True]
-- >>> cthen (PWord "then") [PNumber 1, PWord "else", PNumber 2, PWord "if", PBool True]
-- Right [PNumber 1.0,PWord "fjump",PNumber 1.0,PNumber 2.0,PWord "jump",PNumber 3.0,PBool True]
-- >>> cthen (PWord "then") [PNumber 1, PNumber 1, PNumber 1, PWord "else", PNumber 2, PWord "if", PBool True]
-- Right [PNumber 1.0,PNumber 1.0,PNumber 1.0,PWord "fjump",PNumber 3.0,PNumber 2.0,PWord "jump",PNumber 3.0,PBool True]
-- >>> cthen (PWord "then") [PNumber 1, PWord "else", PNumber 2, PNumber 2, PNumber 2, PWord "if", PBool True]
-- Right [PNumber 1.0,PWord "fjump",PNumber 1.0,PNumber 2.0,PNumber 2.0,PNumber 2.0,PWord "jump",PNumber 5.0,PBool True]
cthen :: CompileProc
cthen _ a =
  case break (== PWord "if") a of
    (_,[]) -> Left "then requires if"
    (b,_:c) ->
      case break (== PWord "else") b of
        (_,[]) -> Right $ b ++ [PWord "jump", PNumber . fromIntegral $ length b] ++ c
        (d,_:e) -> Right $ d ++ [PWord "fjump", PNumber . fromIntegral $ length d] ++ e ++ [PWord "jump", PNumber . fromIntegral $ length e + 2] ++ c

cpush :: CompileProc
cpush a b = Right $ a : b

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
                                               ,("drop", nativeProcedure "drop" pdrop)
                                               ,("dup", nativeProcedure "dup" dup)
                                               ,("dup2", nativeProcedure "dup2" dup2)
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
                                               ,("mod", nativeProcedure "mod" . numericOp2 PVNumber "mod" $ \a b -> fromIntegral (mod (floor a) (floor b) :: Integer))
                                               ,("nop", nativeProcedure "nop" nop)
                                               ,("swap", nativeProcedure "swap" pswap)
                                               ,(":", nativeProcedure ":" startCompile)
                                               ,(";", ImmediateWord ";" endCompile)
                                               ,("jump", CompileOnlyWord "jump" jump cpush)
                                               ,("fjump", CompileOnlyWord "fjump" fjump cpush)
                                               ,("if", CompileOnlyWord "if" nop cpush)
                                               ,("else", CompileOnlyWord "else" nop cpush)
                                               ,("then", CompileOnlyWord "then" nop cthen)
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

run :: PToken -> Env [ByteString]
run t = fromToken t >>= eval'
  where
    eval' :: PContainer -> Env [ByteString]
    eval' (PValue x) = push x >> return []
    eval' (PProc _ p) = p

lookupCt :: ByteString -> Env (Either PProc CompileProc)
lookupCt x = do
  env <- get
  case Map.lookup x $ wordMap env of
    Just (NormalWord _ _ f) -> return $ Right f
    Just (ImmediateWord _ p) -> return $ Left p
    Just (UserDefinedWord _ _) -> return $ Right cpush -- ユーザ定義ワードのコンパイル時意味は「実行時にこのワードを実行」のみ。forthと違い、実行時に解決される。(ワードを再定義した場合既存のワードの動作が変更される)
    Just (CompileOnlyWord _ _ f) -> return $ Right f
    Nothing -> throwError $ "undefined word: " ++ unpack x

pushToken :: PToken -> Env ()
pushToken t = do
  Compile name ts <- getState
  setState . Compile name $ t : ts

updateTokens :: ([PToken] -> Either String [PToken]) -> Env ()
updateTokens f = do
  Compile name ts <- getState
  either throwError (setState . Compile name) $ f ts

compile :: PToken -> Env [ByteString]
compile t@(PWord w) = do
  x <- lookupCt w
  case x of
    Left p -> p
    Right cp -> updateTokens (cp t) >> return []
compile t = pushToken t >> return []

newWord :: PToken -> Env [ByteString]
newWord (PWord name) = setState (Compile name []) >> return []
newWord _ = throwError "specify a new word"

eval :: PToken -> Env [ByteString]
eval t = do
  s <- getState
  case s of
    Run -> run t
    NewWord -> newWord t
    Compile _ _ -> compile t

-- |
-- >>> runResourceT $ sourceList [PNumber 1.0,PNumber 2.0,PNumber 3.0, PWord $ pack ".s", PWord $ pack "+", PWord $ pack "+", PWord $ pack "."] $= conduitPuddingEvaluator $$ consume
-- ["> [3.0, 2.0, 1.0]\n","> 6.0\n"]
conduitPuddingEvaluator :: Monad m => Conduit PToken m ByteString
conduitPuddingEvaluator = CL.concatMapAccum step initEnv =$= CL.map (`append` "\n")
  where
    step :: PToken -> Environment -> (Environment, [ByteString])
    step t e = swap $ runState s e
      where
        s :: State Environment [ByteString]
        s = either (pure . pack . ("*** "++)) id <$> (runErrorT . runEnvT) (map (append "> ") <$> eval t)
