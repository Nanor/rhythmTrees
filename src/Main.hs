module Main where

import Music.Lilypond as LP
import Euterpea as E
import Text.Pretty
import Data.Ratio
import System.Random

import RhythmTree as RT

getDurations :: RhythmTree -> [(RhythmElement, Rational)]
getDurations tree = inner tree 3
    where inner (Single e) n = [(e, n)]
          inner (Branch xs) n = concatMap (\ t -> inner t (n / toRational (length xs))) xs

toEuterpea :: RhythmTree -> E.Music E.Pitch
toEuterpea tree = Modify (E.Tempo 2) (line (map inner (getDurations tree)))
    where inner (RT.Note, n) = E.note n (E.C, 3)
          inner (RT.Rest, n) = E.rest n

toLilypond :: RhythmTree -> String
toLilypond tree = (show . pretty) (New "Test" Nothing (Sequential (map inner (getDurations tree))))
    where inner (RT.Note, n) = LP.Note (NotePitch (LP.Pitch (LP.C, 0, 0)) Nothing) (Just (Duration n)) []
          inner (RT.Rest, n) = LP.Rest (Just (Duration n)) []

-- main :: IO ()
-- main = do
--     (print . toLilypond) testTree
--     (play . toEuterpea) testTree

main = do
    tree <- RT.randomTree
    print tree
    (play . toEuterpea) tree