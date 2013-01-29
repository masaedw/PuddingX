module Pudding where

import Control.Applicative ((<|>),(<$>))
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString as A (choice, string, takeWhile)
import Data.Attoparsec.Char8 as AC (double, isSpace_w8)
import Data.ByteString.Char8 as BC (ByteString)
import Data.Conduit as C (GLConduit, GLInfConduit, (=$=), mapOutput)
import Data.Functor ((<$))
import Data.Conduit.Attoparsec (conduitParser)
import Control.Monad.Trans.Resource

data PToken = PWord ByteString
            | PNumber Double
            | PBool Bool
            | PString ByteString
            deriving (Show, Eq)

parser :: Parser PToken
parser = PNumber <$> double
         <|> PBool <$> choice [True <$ string "true"
                              ,False <$ string "false"]
         <|> PWord <$> (A.takeWhile $ not . isSpace_w8)

conduitPuddingParser :: MonadThrow m => GLInfConduit ByteString m PToken
conduitPuddingParser = mapOutput snd $ conduitParser parser

conduitPuddingEvaluator :: GLConduit PToken m ByteString
conduitPuddingEvaluator = undefined
