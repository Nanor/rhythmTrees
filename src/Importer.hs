module Importer where

import Euterpea as E
import Codec.ByteString.Parser
import Codec.Midi
import Data.ByteString.Lazy as ByteString (readFile)
import RhythmTree as RT
import Data.List
import Data.List.Split (chunksOf)

readMidi :: FilePath -> IO Music1
readMidi fn = do
    file <- importFile fn
    case file of 
        Left err -> error err
        Right midi -> do
            let Midi _ timeDiv tracks = midi
            return $ fromMidi $ Midi SingleTrack timeDiv [head tracks]

fromEuterpea :: Music1 -> RhythmTree
fromEuterpea music = inner $ unpack music

inner :: [(RhythmElement, Rational)] -> RhythmTree
inner [(x, _)] = Single x
inner xs = Branch $ maybe (error "No valid tree") (map inner) $ find (\ (x:xs) -> all (== dur x) (map dur xs)) (partitions xs)
    where
        dur :: [(a, Rational)] -> Rational
        dur l = sum $ map snd l
        partitions :: [a] -> [[[a]]]
        partitions [] = [[]]
        partitions (x:xs) = [[x]:p | p <- partitions xs]
                        ++ [(x:ys):yss | (ys:yss) <- partitions xs]

unpack :: Music1 -> [(RhythmElement, Rational)]
unpack x@(Prim _) | dur x == 0 = []
                  | otherwise  = [convert x]
                  where
                      convert (Prim (E.Note dur _)) = (RT.Note, dur)
                      convert (Prim (E.Rest dur)) = (RT.Rest, dur)
unpack (Modify _ x) = unpack x
unpack (x :+: y) = unpack x ++ unpack y
unpack (x :=: _) = unpack x