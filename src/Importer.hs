module Importer where

import Euterpea as E
import Codec.Midi
import RhythmTree as RT
import Data.List
import Data.Maybe

-- Reads from a midi file into a Euterpea piece of music
readMidi :: FilePath -> IO Music1
readMidi fn = do
    file <- importFile fn
    case file of 
        Left err -> error err
        Right midi -> do
            let Midi _ timeDiv tracks = midi
            return $ fromMidi $ Midi SingleTrack timeDiv [tracks !! 1]

-- Turns a piece of music into a RhythmTree
fromEuterpea :: Music1 -> RhythmTree
fromEuterpea = toRhythmTree . unpack

-- Turns a sequence of note, duration pairs into a RhythmTree
toRhythmTree :: [(RhythmElement, Rational)] -> RhythmTree
toRhythmTree [(x, _)] = Single x
toRhythmTree xs = Branch $ map toRhythmTree $ splitEqually xs

-- Splits a list of RhythmElements and durations into equal duration sublists, or errors
splitEqually :: [(RhythmElement, Rational)] -> [[(RhythmElement, Rational)]]
splitEqually l = inner l (snd $ head l) (tail l)
    where
        inner l _ [] = error $ "Could not split list: " ++ show l --TODO: use ties so that [1%4, 3%4] can be represented as a tree
        inner l n xs = fromMaybe (inner l (n + snd (head xs)) (tail xs)) (splitIntoN l n)

splitIntoN :: [(RhythmElement, Rational)] -> Rational -> Maybe [[(RhythmElement, Rational)]]
splitIntoN = inner []
    where
        inner [] [] _ = Just []
        inner xs l n | sum (map snd xs) == n             = maybe Nothing (\ res -> Just $ xs : res) (inner [] l n)
                     | (sum (map snd xs) > n) || null l  = Nothing
                     | otherwise                         = inner (xs ++ [head l]) (tail l) n

-- Turns a piece of music into a sequence of notes and their durations
unpack :: Music1 -> [(RhythmElement, Rational)]
unpack music = removeNulls . toDurations . sortNotes . fst $ toPositions 0 music
    where
        -- Turns a piece of music into a list of RhythmElement and the point it time it's played
        toPositions :: Rational -> Music1 -> ([(RhythmElement, Rational)], Rational)
        toPositions n (Modify _ x) = toPositions n x
        toPositions n (x :=: y) = (lX ++ lY, max tX tY)
            where
                (lX, tX) = toPositions n x
                (lY, tY) = toPositions n y
        toPositions n (x :+: y) = (lX ++ lY, tY)
            where
                (lX, tX) = toPositions n x
                (lY, tY) = toPositions tX y
        toPositions n x@(Prim _) = ([(noteType x, n), (RT.Rest, n + duration)], n + duration)
                           where
                               duration = dur x
                               noteType (Prim (E.Note _ _)) = RT.Note
                               noteType (Prim (E.Rest _)) = RT.Rest
        -- Sorts a list of RhythmElements and their timings in order of timing, with the note coming last for simultaneous notes 
        sortNotes :: [(RhythmElement, Rational)] -> [(RhythmElement, Rational)]
        sortNotes = sortOn snd . reverse . sortOn fst
        -- Turns a list of RhythmElements and their timings into a list of RhythmElements and their durations in sequence
        toDurations :: [(RhythmElement, Rational)] -> [(RhythmElement, Rational)]
        toDurations [] = []
        toDurations [x] = []
        toDurations (x:xs) = (fst x, snd (head xs) - snd x) : toDurations xs
        -- Removes any zero length RhythmElements
        removeNulls :: [(RhythmElement, Rational)] -> [(RhythmElement, Rational)]
        removeNulls = filter ((> 0) . snd)