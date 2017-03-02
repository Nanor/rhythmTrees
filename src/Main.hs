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
            let bars1 = toRhythmTrees $ head music1
            let bars2 = toRhythmTrees $ head music2
            -- print $ (\ l -> sum l / fromIntegral (length l)) [compareTrees x y | x <- bars1, y <- bars2]
            print $ compareTrees (Branch bars1) (Branch bars2)
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
            let matchMethod : paths = args
            matcher <- case matchMethod of
                "siblings" -> return depthArityIndexMatcher
                "path" -> return pathMatcher
            music <- mapM readMidi paths 
            let bars = concatMap toRhythmTrees $ concat music
            let transitions = generateTransition bars matcher
            let treeGen = generateTree transitions
            bars <- replicateM 10 treeGen
            putStr $ toAscii $ Branch bars
            play $ toEuterpea 10 $ Branch bars
