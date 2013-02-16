{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Pudding (
  conduitPuddingParser,
  conduitPuddingEvaluator,
  ) where

import Control.Monad.Error (runErrorT)
import Control.Monad.State (StateT, runStateT)
import Control.Monad.Writer (execWriterT)
import Data.ByteString.Char8 as BC (ByteString, pack, append)
import Data.Conduit as C (Conduit, (=$=))
import qualified Data.Conduit.List as CL (map, concatMapAccumM)
import Data.Map as Map (fromList)
import Data.Tuple (swap)
import Data.Vector as V (fromList)
import Prelude hiding (div)
import Pudding.Core
import Pudding.Words

-- $setup
-- >>> import Data.Conduit
-- >>> import Data.Conduit.List
-- >>> :set -XOverloadedStrings

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


-- |
-- >>> runResourceT $ sourceList [PNumber 1.0,PNumber 2.0,PNumber 3.0, PWord $ pack ".s", PWord $ pack "+", PWord $ pack "+", PWord $ pack "."] $= conduitPuddingEvaluator $$ consume
-- ["> [3.0, 2.0, 1.0]\n","> 6.0\n"]
conduitPuddingEvaluator :: Monad m => Conduit PToken m ByteString
conduitPuddingEvaluator = CL.concatMapAccumM step initEnv =$= CL.map (`append` "\n")
  where
    step :: Monad m => PToken -> Environment m -> m (Environment m, [ByteString])
    step t e = return . swap =<< runStateT s e
      where
        s :: Monad m => StateT (Environment m) m [ByteString]
        s = execWriterT $ do
          Right result <- runErrorT . runEnvT $ eval t `catchError` (ng . pack)
          return result
