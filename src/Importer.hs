module Importer where

import Euterpea as E
import Codec.Midi
import RhythmTree as RT
import Data.List
import Data.Maybe
import Data.Ratio
import Control.Arrow

-- Reads from a midi file into a Euterpea piece of music
readMidi :: FilePath -> IO [Music1]
readMidi fn = do
    file <- importFile fn
    case file of 
        Left err -> error err
        Right midi -> do
            let Midi _ timeDiv tracks = midi
            return $ filter (/= Prim (E.Rest (0 % 1))) $ map (\ t -> fromMidi $ Midi SingleTrack timeDiv [t]) tracks

-- Turns a Euterpea Music1 into a RhythmTree
toRhythmTree :: Music1 -> RhythmTree
toRhythmTree = Branch . toRhythmTrees

toRhythmTrees :: Music1 -> [RhythmTree]
toRhythmTrees = map (simplify . inner) . splitIntoBars . unpack
    where
        inner :: [(RhythmElement, Rational)] -> RhythmTree
        inner [(x, _)] = Single x
        inner xs = Branch . map inner $ splitEqually xs

splitIntoBars :: [(RhythmElement, Rational)] -> [[(RhythmElement, Rational)]] 
splitIntoBars l = inner [] l 1
    where
        inner [] [] _ = []
        inner xs l n | sum (map snd xs) == n = xs : inner [] l n
                     | sum (map snd xs) > n  = (init xs ++ [(fst (last xs), n - sum (map snd (init xs)))]) : inner [] ((RT.Tie, sum (map snd xs) - n) : l) n
                     | null l                = [xs ++ [(RT.Rest, n - sum (map snd xs))]]
                     | otherwise             = inner (xs ++ [head l]) (tail l) n

-- Splits a list of RhythmElements and durations into equal duration sublists, or errors
splitEqually :: [(RhythmElement, Rational)] -> [[(RhythmElement, Rational)]]
splitEqually l = inner l (snd $ head l) (possibleSplits (snd $ head l) (sum $ map snd l))
    where
        inner l _ [] = factorize l
        inner l n xs = fromMaybe (inner l (head xs) (tail xs)) (splitIntoN l n)
        possibleSplits :: Rational -> Rational -> [Rational]
        possibleSplits smallest total = takeWhile (>= smallest) [total / n | n <- [2..]] 

factorize :: [(RhythmElement, Rational)] -> [[(RhythmElement, Rational)]]
factorize l = map (: []) $ concatMap inner l
    where
        inner :: (RhythmElement, Rational) -> [(RhythmElement, Rational)]
        inner (noteType, duration) = (noteType, divisor) : replicate (fromInteger $ numerator (duration / divisor) - 1) (RT.Tie, divisor)
        divisor = foldl gcdRat 1 $ map snd l
        gcdRat a b = gcd (numerator a) (numerator b) % lcm (denominator a) (denominator b)

splitIntoN :: [(RhythmElement, Rational)] -> Rational -> Maybe [[(RhythmElement, Rational)]]
splitIntoN = inner []
    where
        inner [] [] _ = Just []
        inner xs l n | sum (map snd xs) == n             = maybe Nothing (\ res -> Just $ xs : res) (inner [] l n)
                     | (sum (map snd xs) > n) || null l  = Nothing
                     | otherwise                         = inner (xs ++ [head l]) (tail l) n

-- Turns a piece of music into a sequence of notes and their durations
unpack :: Music1 -> [(RhythmElement, Rational)]
unpack = removeNulls . toDurations . sort . adjust . ((RT.Rest, 0) :) . toPositions
    where 
        toPositions :: Music1 -> [(RhythmElement, Rational)]
        toPositions (Modify _ m) = toPositions m
        toPositions (Modify _ (Prim (E.Rest 0)) :+: m) = toPositions m
        toPositions (p@(Prim _) :=: y) = (convert p, 0) : toPositions y
        toPositions (p@(Prim _) :+: y) = (convert p, 0) : map (fst &&& (+ dur p) . snd) (toPositions y)
        toPositions (p@(Prim _)) = [(convert p, 0), (RT.Rest, dur p)]
        toPositions (x :=: y) = toPositions x ++ toPositions y
        toPositions m = error (take 1000 $ show m)
        convert :: Music1 -> RhythmElement
        convert (Prim (E.Note _ _)) = RT.Note
        convert (Prim (E.Rest _)) = RT.Rest
        toDurations :: [(RhythmElement, Rational)] -> [(RhythmElement, Rational)]
        toDurations [] = []
        toDurations [(RT.Rest, _)] = []
        toDurations (x:xs) = (fst x, snd (head xs) - snd x) : toDurations xs
        sort = sortOn snd . reverse . sortOn fst
        adjust :: [(RhythmElement, Rational)] -> [(RhythmElement, Rational)]
        adjust = map (fst &&& ((% 16) . round . fromRational . (* 16) . snd))
        -- Removes any zero length RhythmElements
        removeNulls :: [(RhythmElement, Rational)] -> [(RhythmElement, Rational)]
        removeNulls = filter ((> 0) . snd)
