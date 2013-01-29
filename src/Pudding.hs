module Pudding where

import Control.Applicative ((<|>),(<$>))
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString as A (choice, string, takeWhile)
import Data.Attoparsec.Char8 as AC (double, isSpace_w8)
import Data.ByteString.Char8 as BC (ByteString)
import Data.Conduit as C (GLConduit)
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

parse :: GLConduit ByteString m PToken
parse = undefined

eval :: GLConduit PToken m ByteString
eval = undefined
