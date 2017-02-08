module Main where

import RhythmTree
import Exporter
import Euterpea (play)
import Importer
import Markov
import System.Environment
import Control.Monad

main = do
    method : args <- getArgs

    case method of
        "compare" -> do
            let [path1, path2] = args
            music1 <- readMidi path1
            music2 <- readMidi path2
            let tree1 = toRhythmTree $ head music1
            let tree2 = toRhythmTree $ head music2
            print $ compareTrees tree1 tree2
        "tree" -> do
            let [path] = args
            music <- readMidi path
            print $ toRhythmTree $ head music
        "draw" -> do
            let [path] = args
            music <- readMidi path
            putStr $ toAscii $ toRhythmTree $ head music
        "play" -> do
            let [path] = args
            music <- readMidi path
            play $ head music
        "markov" -> do
            let [path] = args
            music <- readMidi path
            let bars = toRhythmTrees $ head music
            let transitions = generateTransition bars
            let treeGen = generateTree transitions
            trees <- replicateM 10 treeGen
            play $ toEuterpea 10 $ Branch trees
            print $ Branch trees
