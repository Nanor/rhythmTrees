module Markov where

import RhythmTree
import Data.List
import Control.Arrow
import System.Random

characteriseTrees :: [RhythmTree] -> [(Int, Int, Int, Int)]
characteriseTrees = concatMap characteriseTree

characteriseTree :: RhythmTree -> [(Int, Int, Int, Int)]
characteriseTree = inner 0 0 0
    where
        inner :: Int -> Int -> Int -> RhythmTree -> [(Int, Int, Int, Int)]
        inner depth arity childNo (Single x) = [(depth, arity, childNo, convert (Single x))]
        inner depth arity childNo (Branch b) = (depth, arity, childNo, convert (Branch b)) : concat (mapInd (flip (inner (depth + 1) (length b))) b)
            where mapInd f l = zipWith f l [0..]
        convert :: RhythmTree -> Int
        convert (Single Note) = 0
        convert (Single Rest) = -1
        convert (Single Tie) = -2
        convert (Branch b) = length b

generateTransition :: [RhythmTree] -> (Int, Int, Int) -> [(Int, Float)]
generateTransition ts c = normalise $ count $ map (\ (_, _, _, n) -> n) matchingNotes
    where
        normalise l = map (\ x -> (fst x, fromIntegral (snd x) / fromIntegral (sum (map snd l)))) l
        matchingNotes = filter (matches c) $ characteriseTrees ts
        matches (a1, b1, c1) (a2, b2, c2, _) = a1 == a2 && b1 == b2 && c1 == c2
        count = map (head &&& length) . group . sort

selectNote :: [(Int, Float)] -> Float -> Int
selectNote [(n, _)] _ = n
selectNote ((x, y):xs) n | n <= y    = x
                         | otherwise = selectNote xs (n - y)

generateTree :: ((Int, Int, Int) -> [(Int, Float)]) -> IO RhythmTree
generateTree f = do
    tree <- genTree 0 0 0
    return $ simplify tree
        where
            genTree :: Int -> Int -> Int -> IO RhythmTree
            genTree depth arity childNo = do
                v <- randomIO :: (Random t, Fractional t) => IO t
                let element = selectNote (f (depth, arity, childNo)) v
                if element == 0 then
                    return $ Single Note
                else if element == (-1) then
                    return $ Single Rest
                else if element == (-2) then
                    return $ Single Tie
                else do
                    b <- mapM (genTree (depth + 1) element) [0..element-1]
                    return $ Branch b
              