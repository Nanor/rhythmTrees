module Main where

import RhythmTree
import Exporter
import Euterpea
import Importer
import System.Environment

main = do
    fn <- getArgs
    music <- readMidi $ head fn
    -- play music
    let tree = fromEuterpea music
    print tree
    play $ toEuterpea tree