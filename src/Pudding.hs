{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Pudding (
  conduitPuddingParser,
  conduitPuddingEvaluator,
  ) where

import Control.Applicative (Applicative, (<$>), pure)
import Control.Monad (liftM, unless)
import Control.Monad.Error (MonadError, ErrorT, runErrorT, catchError, throwError)
import Control.Monad.State (MonadState, StateT, get, put, runStateT, modify)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Writer (MonadWriter, WriterT, execWriterT, tell)
import Data.ByteString.Char8 as BC (ByteString, pack, append, unpack)
import Data.Conduit as C (Conduit, (=$=))
import qualified Data.Conduit.List as CL (map, concatMapAccumM)
import Data.List (intercalate)
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

setState :: Monad m => PState -> EnvT m ()
setState s = modify $ \e -> e { state = s }

getState :: Monad m => EnvT m PState
getState = liftM state get

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

setPc :: Monad m => Int -> EnvT m ()
setPc i = do
  env <- get
  put env { pc = i }

incPc :: Monad m => EnvT m ()
incPc = getPc >>= \i -> setPc $ succ i

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

-- pudding procedure

ok :: Monad m => ByteString -> PProc m
ok msg = tell . pure $ append "> " msg

ng :: Monad m => ByteString -> PProc m
ng msg = tell . pure $ append "*** " msg

showTop :: Monad m => PProc m
showTop = ok . pack . showPV =<< pop

showStack :: Monad m => PProc m
showStack = ok . pack . ('[':) . (++"]") . intercalate ", " . map showPV . stack =<< get

showCallStack :: Monad m => PProc m
showCallStack = ok . pack . show . map word . callStack =<< get

numericOp2 :: Monad m => (a -> PValue) -> String -> (Double -> Double -> a) -> PProc m
numericOp2 ctor name op = transaction (const msg) $ do
  PVNumber a <- pop
  PVNumber b <- pop
  push . ctor $ op b a
  where
    msg = name ++ " needs 2 Numbers"

booleanOp2 :: Monad m => (a -> PValue) -> String -> (Bool -> Bool -> a) -> PProc m
booleanOp2 ctor name op = transaction (const msg) $ do
  PVBool a <- pop
  PVBool b <- pop
  push . ctor $ op b a
  where
    msg = name ++ " needs 2 Booleans"

booleanOp1 :: Monad m => (a -> PValue) -> String -> (Bool -> a) -> PProc m
booleanOp1 ctor name op = transaction (const msg) $ do
  PVBool a <- pop
  push . ctor $ op a
  where
    msg = name ++ " needs 1 Boolean"

pdrop :: Monad m => PProc m
pdrop = pop >> return ()

nip :: Monad m => PProc m
nip = do
  a <- pop
  _ <- pop
  push a

dup :: Monad m => PProc m
dup = do
  x <- pop
  push x
  push x

over :: Monad m => PProc m
over = do
  w2 <- pop
  w1 <- pop
  push w1
  push w2
  push w1

tuck :: Monad m => PProc m
tuck = do
  w2 <- pop
  w1 <- pop
  push w2
  push w1
  push w2

dup2 :: Monad m => PProc m
dup2 = do
  x <- pop
  y <- pop
  push y
  push x
  push y
  push x

jump :: Monad m => PProc m
jump = jump' `catchError` return (throwError "stack top is not Boolean")
  where
    jump' :: Monad m => PProc m
    jump' = do
      PVNumber a <- pop
      PVBool cond <- pop
      unless cond $
        getPc >>= setPc . (+ floor a)

fjump :: Monad m => PProc m
fjump = do
  PVNumber a <- pop
  getPc >>= setPc . (+ floor a)

nop :: Monad m => PProc m
nop = return ()

pswap :: Monad m => PProc m
pswap = do
  a <- pop
  b <- pop
  push b
  push a

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

startCompile :: Monad m => PProc m
startCompile = setState NewWord

endCompile :: Monad m => PProc m
endCompile = do
  Compile name tokens <- getState
  setState Run
  insertWord name . UserDefinedWord name . V.fromList $ reverse tokens

initEnv :: Monad m => Environment m
initEnv = Environment { stack = []
                      , wordMap = Map.fromList [(".", nativeProcedure "." showTop)
                                               ,(".s", nativeProcedure ".s" showStack)
                                               ,(".cs", nativeProcedure ".cs" showCallStack)
                                               ,("drop", nativeProcedure "drop" pdrop)
                                               ,("dup", nativeProcedure "dup" dup)
                                               ,("nip", nativeProcedure "nip" nip)
                                               ,("over", nativeProcedure "over" over)
                                               ,("tuck", nativeProcedure "tuck" tuck)
                                               ,("2dup", nativeProcedure "2dup" dup2)
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

-- |
-- >>> runResourceT $ sourceList [PNumber 1.0,PNumber 2.0,PNumber 3.0, PWord $ pack ".s", PWord $ pack "+", PWord $ pack "+", PWord $ pack "."] $= conduitPuddingEvaluator $$ consume
-- ["> [3.0, 2.0, 1.0]\n","> 6.0\n"]
conduitPuddingEvaluator :: (Applicative m, Monad m) => Conduit PToken m ByteString
conduitPuddingEvaluator = CL.concatMapAccumM step initEnv =$= CL.map (`append` "\n")
  where
    step :: (Applicative m, Monad m) => PToken -> Environment m -> m (Environment m, [ByteString])
    step t e = swap <$> runStateT s e
      where
        s :: (Applicative m, Monad m) => StateT (Environment m) m [ByteString]
        s = execWriterT $ do
          Right result <- runErrorT . runEnvT $ eval t `catchError` (ng . pack)
          pure result
