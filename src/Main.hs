module Main where

import RhythmTree as RT
import Exporter
import Euterpea
import Importer
import System.Environment

main = do
    method : args <- getArgs

    case method of
        "compare" -> do
            let [path1, path2] = args
            music1 <- readMidi path1
            music2 <- readMidi path2
            let tree1 = fromEuterpea $ head music1
            let tree2 = fromEuterpea $ head music2
            print $ compareTrees tree1 tree2
        "makeTree" -> do
            let [path] = args
            music <- readMidi path
            print $ fromEuterpea $ head music

    -- print $ toLilypond (Branch [Branch [Single RT.Note, Single RT.Note], Single RT.Rest])
    -- fn <- getArgs
    -- music <- readMidi $ head fn
    -- -- play music
    -- (putStr . unlines) $ map (toAscii . fromEuterpea) music