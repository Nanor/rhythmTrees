module Main where

import RhythmTree
import Exporter
import Euterpea
import Importer
import System.Environment

import TestFile

main = do
    fn <- getArgs
    music <- readMidi $ head fn
    play music
    print $ fromEuterpea music