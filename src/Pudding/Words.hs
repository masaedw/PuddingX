{-# LANGUAGE OverloadedStrings #-}

module Pudding.Words where

import Data.ByteString.Char8 as BC (ByteString, pack, append)
import Data.List (intercalate)
import Data.Vector (fromList)
import Pudding.Core
import Pudding.Imports


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
  [PVNumber a, PVNumber b] <- replicateM 2 pop
  push . ctor $ op b a
  where
    msg = name ++ " needs 2 Numbers"

booleanOp2 :: Monad m => (a -> PValue) -> String -> (Bool -> Bool -> a) -> PProc m
booleanOp2 ctor name op = transaction (const msg) $ do
  [PVBool a, PVBool b] <- replicateM 2 pop
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
  [a, _] <- replicateM 2 pop
  push a

dup :: Monad m => PProc m
dup = do
  x <- pop
  replicateM_ 2 $ push x

over :: Monad m => PProc m
over = do
  [w2, w1] <- replicateM 2 pop
  mapM_ push [w1, w2, w1]

tuck :: Monad m => PProc m
tuck = do
  [w2, w1] <- replicateM 2 pop
  mapM_ push [w2, w1, w2]

dup2 :: Monad m => PProc m
dup2 = do
  [x, y] <- replicateM 2 pop
  mapM_ push [y, x, y, x]

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

startCompile :: Monad m => PProc m
startCompile = setState NewWord

endCompile :: Monad m => PProc m
endCompile = do
  Compile name tokens <- getState
  setState Run
  insertWord name . UserDefinedWord name . fromList $ reverse tokens

