{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Pudding where

import Control.Applicative ((<|>),(<$>), (*>), (<*))
import Control.Monad.Error (ErrorT, runErrorT, catchError, throwError)
import Control.Monad.State (State, get, put, runState)
import Control.Monad.Trans.Resource (MonadThrow)
import Data.Attoparsec.ByteString (Parser, Result, IResult(..), choice, many')
import qualified Data.Attoparsec.ByteString as A (takeWhile1)
import Data.Attoparsec.Char8 as AC (char, string, double, feed, parse, isSpace_w8, skipSpace, sepBy, satisfy, notInClass)
import Data.ByteString.Char8 as BC (ByteString, pack, append)
import Data.Conduit as C (Conduit, (=$=))
import qualified Data.Conduit.List as CL (map, concatMapAccum)
import Data.Functor ((<$))
import Data.Map as Map (Map, fromList, lookup)
import Data.Tuple (swap)
import Prelude hiding (div)

data PToken = PWord ByteString
            | PNumber Double
            | PBool Bool
            | PString ByteString
            deriving (Show, Eq)

-- | token parser
--
-- >>> parseOnly pToken . pack $ "123.5"
-- Right (PNumber 123.5)
-- >>> parseOnly pToken . pack $ "\"aabbcc\""
-- Right (PString "aabbcc")
-- >>> parseOnly pToken . pack $ "abc"
-- Right (PWord "abc")
pToken :: Parser PToken
pToken = PNumber <$> double
         <|> PBool <$> choice [True <$ string "true"
                              ,False <$ string "false"]
         <|> PString <$> pString
         <|> PWord <$> A.takeWhile1 (not . isSpace_w8)

-- | string paresr
--
-- >>> parseOnly pString . pack $ show ""
-- Right ""
-- >>> parseOnly pString . pack $ show "abc"
-- Right "abc"
-- >>> parseOnly pString . pack $ show "'"
-- Right "'"
-- >>> parseOnly pString . pack $ "\"\\\"\\\\\\0\\a\\b\\f\\n\\r\\t\""
-- Right "\"\\\NUL\a\b\f\n\r\t"
pString :: Parser ByteString
pString = char '"' *> (pack <$> many' (pEscape <|> pChar)) <* char '"'

pChar :: Parser Char
pChar = AC.satisfy $ AC.notInClass "\"\\"

-- | espace char parser
--
-- >>> parseOnly pEscape $ pack "\\\""
-- Right '"'
-- >>> parseOnly pEscape $ pack "\\\\"
-- Right '\\'
-- >>> parseOnly pEscape $ pack "\\0"
-- Right '\NUL'
-- >>> parseOnly pEscape $ pack "\\a"
-- Right '\a'
-- >>> parseOnly pEscape $ pack "\\t"
-- Right '\t'
pEscape :: Parser Char
pEscape = char '\\' *> (unEscape <$> AC.satisfy (`elem` "\"\\0abfnrt"))
  where
    unEscape '"' = '"'
    unEscape '\\' = '\\'
    unEscape '0' = '\0'
    unEscape 'a' = '\a'
    unEscape 'b' = '\b'
    unEscape 'f' = '\f'
    unEscape 'n' = '\n'
    unEscape 'r' = '\r'
    unEscape 't' = '\t'
    unEscape a = error $ "unknown character: " ++ [a]

-- |
-- >>> :m +Data.Conduit Data.Conduit.List
-- >>> runResourceT $ sourceList [pack "aaa 1", pack "2 3"] $= conduitPuddingParser $$ consume
-- [PWord "aaa",PNumber 1.0,PNumber 2.0,PNumber 3.0]
conduitPuddingParser :: MonadThrow m => Conduit ByteString m PToken
conduitPuddingParser = CL.concatMapAccum step ""
  where
    step :: ByteString -> ByteString -> (ByteString, [PToken])
    step input rest = case parseFeed parser (append rest input) of
      Done t r -> (t, r)
      Fail { } -> ("", [])
      _ -> error "Partial should not happen"

    parser :: Parser [PToken]
    parser = skipSpace >> pToken `sepBy` skipSpace

    parseFeed :: Parser [PToken] -> ByteString -> Result [PToken]
    parseFeed p i = feed (parse p i) ""

data PData = PDNumber Double
           | PDBool Bool
           | PDString ByteString
           deriving (Eq, Show)

--type PProc = [PData] -> Either ByteString ([ByteString], [PData])
type PProc = Env [ByteString]

data Environment = Environment
                   { stack :: [PData]
                   , wordMap :: Map ByteString PProc
                   }

showTop :: PProc
showTop = (:[]) . pack . show <$> pop

showStack :: PProc
showStack = (:[]) . pack . show . stack <$> get

type Op2 = PData -> PData -> Either String PData

numericOp2 :: (a -> PData) -> String -> (Double -> Double -> a) -> PProc
numericOp2 ctor name op = transaction (const msg) $ do
  a <- pop
  b <- pop
  either throwError (\x -> push x >> return []) $ op' a b
  where
    op' :: Op2
    op' (PDNumber a) (PDNumber b) = return . ctor $ op b a
    op' _ _ = throwError msg

    msg = name ++ " needs 2 Numbers"

plus :: PProc
plus = numericOp2 PDNumber "+" (+)

minus :: PProc
minus = numericOp2 PDNumber "-" (-)

mul :: PProc
mul = numericOp2 PDNumber "*" (*)

div :: PProc
div = numericOp2 PDNumber "/" (/)

eq :: PProc
eq = numericOp2 PDBool "==" (==)

ne :: PProc
ne = numericOp2 PDBool "!=" (/=)

lt :: PProc
lt = numericOp2 PDBool "<" (<)

le :: PProc
le = numericOp2 PDBool "<=" (<=)

gt :: PProc
gt = numericOp2 PDBool ">" (>)

ge :: PProc
ge = numericOp2 PDBool ">=" (>=)

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
                                           ,("+", plus)
                                           ,("-", minus)
                                           ,("*", mul)
                                           ,("/", div)
                                           ,("dup", dup)
                                           ,("==", eq)
                                           ,("!=", ne)
                                           ,("<", lt)
                                           ,("<=", le)
                                           ,(">", gt)
                                           ,(">=", ge)
                                           ]
                      }

type Env = ErrorT String (State Environment)

-- |
-- 失敗だったら状態を戻して失敗を伝える、成功だったらそのまま
transaction :: (String -> String) -> Env a -> Env a
transaction msg m = do
  origEnv <- get
  catchError m $ (put origEnv >>) . throwError . msg

push :: PData -> Env ()
push d = do
  env@(Environment s _) <- get
  put env { stack = d:s }

pop :: Env PData
pop = do
  env@(Environment s _) <- get
  case s of
    (a:as) -> put env { stack = as } >> return a
    _ -> throwError "empty stack"

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
        s = either ((:[]) . pack . ("*** "++)) id <$> runErrorT (fromToken t >>= eval)

    eval :: PContainer -> Env [ByteString]
    eval (PData x) = push x >> return []
    eval (PProc _ p) = map (append "> ") <$> p
