module Main (main) where

import Data.Conduit (($=),($$))
import Data.Conduit as C (runResourceT)
import Data.Conduit.Binary as CB (sourceHandle, sinkHandle)
import Pudding (parse, eval)
import System.IO

main :: IO ()
main = runResourceT
       $ sourceHandle stdin
       $= parse
       $= eval
       $$ sinkHandle stdout
