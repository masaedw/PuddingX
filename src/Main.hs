module Main (main) where

import Data.Conduit (($=),($$))
import Data.Conduit as C (runResourceT)
import Data.Conduit.Binary as CB (sourceHandle, sinkHandle, lines)
import Data.Conduit.List as CL (concatMap)
import Pudding (conduitPuddingParser, conduitPuddingEvaluator)
import System.IO

main :: IO ()
main = runResourceT
       $ sourceHandle stdin
       $= conduitPuddingParser
       $= conduitPuddingEvaluator
       $$ sinkHandle stdout
