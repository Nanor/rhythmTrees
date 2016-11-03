module Main where

import Music.Lilypond as Lilypond
import Euterpea
import Text.Pretty
import Data.Ratio

import Tree

getDurations :: RhythmTree -> [(RhythmElement, Rational)]
getDurations tree = inner tree 3
    where inner (Single e) n = [(e, n)]
          inner (Branch xs) n = concatMap (\ t -> inner t (n / ((toRational (length xs))))) xs

toEuterpea :: RhythmTree -> Euterpea.Music Euterpea.Pitch
toEuterpea tree = Modify (Euterpea.Tempo 2) (line (map inner (getDurations tree)))
    where inner (NoteElement, n) = Euterpea.note n (Euterpea.C, 3)
          inner (RestElement, n) = Euterpea.rest n

toLilypond :: RhythmTree -> String
toLilypond tree = (show . pretty) (New "Test" Nothing (Sequential (map inner (getDurations tree))))
    where inner (NoteElement, n) = (Lilypond.Note (NotePitch (Lilypond.Pitch (Lilypond.C, 0, 0)) Nothing) (Just (Duration n)) [])
          inner (RestElement, n) = (Lilypond.Rest (Just (Duration n)) [])

main = do
    (print . toLilypond) testTree
    (play . toEuterpea) testTree
