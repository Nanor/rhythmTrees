module Exporter (toLilypond, toEuterpea) where

import Music.Lilypond as LP
import Euterpea as E
import Text.Pretty

import RhythmTree as RT

-- Converts a RhythmTree into a list of RhythmElements and their durations
getDurations :: RhythmTree -> [(RhythmElement, Rational)]
getDurations tree = inner tree 1
    where inner (Single e) n = [(e, n)]
          inner (Branch xs) n = concatMap (\ t -> inner t (n / toRational (length xs))) xs

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
          