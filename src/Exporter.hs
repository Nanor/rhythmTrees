module Exporter (toLilypond, toEuterpea, toAscii) where

import Music.Lilypond as LP
import Euterpea as E
import Text.Pretty
import Data.Tree
import Data.Tree.Pretty
import RhythmTree as RT
import Data.Ratio

-- Converts a RhythmTree into a list of RhythmElements and their durations
getDurations :: Rational -> RhythmTree -> [(RhythmElement, Rational)]
getDurations bars tree = removeTies $ toList tree bars
    where
        toList :: RhythmTree -> Rational -> [(RhythmElement, Rational)]
        toList (Single e) n = [(e, n)]
        toList (Branch xs) n = concatMap (\ t -> toList t (n / toRational (length xs))) xs
        removeTies :: [(RhythmElement, Rational)] -> [(RhythmElement, Rational)] 
        removeTies ((RT.Tie, n) : xs) = (RT.Rest, n) : removeTies xs
        removeTies (x@(xType, xDur) : y@(yType, yDur) : xs) | yType == RT.Tie = removeTies $ (xType, xDur + yDur) : xs
                                                            | otherwise       = x : removeTies (y : xs)
        removeTies [x] = [x]
        removeTies [] = []

-- Converts a RhythmTree into a Euterpea Music1 format for audio playback
toEuterpea :: Rational -> RhythmTree -> Music1
toEuterpea bars tree = line (map inner (getDurations bars tree))
    where inner (RT.Note, n) = E.note n ((E.C, 3), [])
          inner (RT.Rest, n) = E.rest n

-- Converts a RhythmTree into a string to be complied into Lilypond
toLilypond :: Rational -> RhythmTree -> String
toLilypond bars tree = (show . pretty) (New "RhythmicStaff" Nothing (Sequential (map inner (getDurations bars tree))))
    where inner (RT.Note, n) = LP.Note (NotePitch (LP.Pitch (LP.C, 0, 4)) Nothing) (Just (Duration n)) []
          inner (RT.Rest, n) = LP.Rest (Just (Duration n)) []
          
toAscii :: RhythmTree -> String
toAscii = drawTree . convert
    where
        convert (Branch b) = Node "" $ map convert b
        convert (Single x) = Node (show x) []

-- Serialise the RhythmTree as a naive representation of the datatype
toRawFile :: [RhythmTree] -> FilePath -> IO ()
toRawFile ts fp = writeFile fp (show ts)
