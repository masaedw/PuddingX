module Pudding where

import Control.Applicative ((<|>),(<$>))
import Control.Monad.Trans.Resource (MonadThrow)
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString as A (choice, string, takeWhile)
import Data.Attoparsec.Char8 as AC (double, isSpace_w8)
import Data.ByteString.Char8 as BC (ByteString, pack)
import Data.Conduit as C (GLConduit, GLInfConduit, (=$=), mapOutput)
import Data.Conduit.Attoparsec (conduitParser)
import Data.Conduit.List as CL (map)
import Data.Functor ((<$))

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

-- temporary implementation
conduitPuddingEvaluator :: Monad m => GLInfConduit PToken m ByteString
conduitPuddingEvaluator = CL.map $ pack . show
