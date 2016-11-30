module Exporter (toLilypond, toEuterpea) where

import Music.Lilypond as LP
import Euterpea as E
import Text.Pretty

import RhythmTree as RT

-- Converts a RhythmTree into a list of RhythmElements and their durations
getDurations :: RhythmTree -> [(RhythmElement, Rational)]
getDurations tree = removeTies $ toList tree 1
    where
        toList :: RhythmTree -> Rational -> [(RhythmElement, Rational)]
        toList (Single e) n = [(e, n)]
        toList (Branch xs) n = concatMap (\ t -> toList t (n / toRational (length xs))) xs
        removeTies :: [(RhythmElement, Rational)] -> [(RhythmElement, Rational)] 
        removeTies (x@(xType, xDur) : y@(yType, yDur) : xs) | yType == RT.Tie = removeTies $ (xType, xDur + yDur) : xs
                                                            | otherwise       = x : removeTies (y : xs)
        removeTies [x] = [x]
        removeTies [] = []

-- Converts a RhythmTree into a Euterpea Music1 format for audio playback
toEuterpea :: RhythmTree -> Music1
toEuterpea tree = line (map inner (getDurations tree))
    where inner (RT.Note, n) = E.note n ((E.C, 3), [])
          inner (RT.Rest, n) = E.rest n

-- Converts a RhythmTree into a string to be complied into Lilypond
toLilypond :: RhythmTree -> String
toLilypond tree = (show . pretty) (New "RhythmicStaff" Nothing (Sequential (map inner (getDurations tree))))
    where inner (RT.Note, n) = LP.Note (NotePitch (LP.Pitch (LP.C, 0, 4)) Nothing) (Just (Duration n)) []
          inner (RT.Rest, n) = LP.Rest (Just (Duration n)) []
          