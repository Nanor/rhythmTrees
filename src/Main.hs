module Main where

import RhythmTree
import Exporter
import Euterpea

main = do
    tree <- randomTree
    print tree
    (play . toEuterpea) tree