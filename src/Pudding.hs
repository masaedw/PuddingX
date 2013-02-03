{-# LANGUAGE OverloadedStrings #-}
module Pudding where

import Control.Applicative ((<|>),(<$>), (*>), (<*))
import Control.Monad.Trans.Resource (MonadThrow)
import Data.Attoparsec.ByteString (Parser, many')
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.Char8 as AC hiding (space)
import Data.ByteString.Char8 as BC (ByteString, pack, append)
import Data.Conduit as C (Conduit, GLInfConduit)
import Data.Conduit.List as CL (map)
import Data.Conduit.Util (conduitState, ConduitStateResult(..))
import Data.Functor ((<$))
import Data.Map as Map (Map, fromList)
import GHC.Word (Word8)
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
         <|> PWord <$> (A.takeWhile1 $ not . isSpace_w8)

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
pEscape = char '\\' *> (unEscape <$> (AC.satisfy (`elem` "\"\\0abfnrt")))
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

conduitPuddingParser :: MonadThrow m => Conduit ByteString m [PToken]
conduitPuddingParser = conduitState "" push close
  where
    push :: MonadThrow m => ByteString -> ByteString -> m (ConduitStateResult ByteString ByteString [PToken])
    push rest "" = case parseOnly parser rest of
      Right r -> return $ StateFinished Nothing [r]
      Left _ -> return $ StateFinished Nothing []
    push rest input = case parseOnly parser (append rest input) of
      Right r -> return $ StateProducing "" [r]
      Left _ -> return $ StateProducing "" []

    parser = (skipSpace >> pToken `sepBy` skipSpace)

    close :: MonadThrow m => ByteString -> m [[PToken]]
    close rest = case parseOnly parser rest of
      Right r -> return [r]
      Left _ -> return []

data PData = PDNumber Double
           | PDBool Bool
           | PDString ByteString
           deriving (Eq, Show)

type PProc = [PData] -> Either ByteString (ByteString, [PData])

data Environment = Environment
                   { stack :: [PData]
                   , wordMap :: Map ByteString PProc
                   }

showTop :: PProc
showTop (s:xs) = Right ((pack $ show s), xs)
showTop s = Left "empty stack"

showStack :: PProc
showStack s = Right ((pack $ show s), s)

plus :: PProc
plus ((PDNumber a):(PDNumber b):xs) = Right ("", (PDNumber $ b+a):xs)
plus ((PDString a):(PDString b):xs) = Right ("", (PDString $ append b a):xs)
plus _ = Left "+ needs 2 operands"

minus :: PProc
minus ((PDNumber a):(PDNumber b):xs) = Right ("", (PDNumber $ b-a):xs)
minus _ = Left "- needs 2 operands"

mul :: PProc
mul ((PDNumber a):(PDNumber b):xs) = Right ("", (PDNumber $ b*a):xs)
mul _ = Left "* needs 2 operands"

div :: PProc
div ((PDNumber a):(PDNumber b):xs) = Right ("", (PDNumber $ b/a):xs)
div _ = Left "/ needs 2 operands"

initEnv :: Environment
initEnv = Environment { stack = []
                      , wordMap = fromList [(".", showTop)
                                           ,(".s", showStack)
                                           ,("+", plus)
                                           ,("-", minus)
                                           ,("*", mul)
                                           ,("/", div)
                                           ]
                      }

conduitPuddingEvaluator :: Monad m => Conduit PToken m ByteString
conduitPuddingEvaluator = conduitState initEnv push close
  where
    push = undefined
    close = undefined
