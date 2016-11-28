module Main where

import RhythmTree
import Exporter
-- import Euterpea
import Importer
import System.Environment

main = do
    fn <- getArgs
    music <- readMidi $ head fn
    print $ fromEuterpea music