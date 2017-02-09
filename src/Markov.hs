module Markov where

import RhythmTree
import Data.List
import Control.Arrow
import System.Random

characteriseTrees :: [RhythmTree] -> [([(Int, Int)], Int)]
characteriseTrees = concatMap characteriseTree

characteriseTree :: RhythmTree -> [([(Int, Int)], Int)]
characteriseTree = inner []
    where
        inner :: [(Int, Int)] -> RhythmTree -> [([(Int, Int)], Int)]
        inner path (Single x) = [(path, convert (Single x))]
        inner path (Branch b) = (path, convert (Branch b)) : concat (mapInd (\ s i -> inner (path ++ [(i, length b)]) s) b)
            where mapInd f l = zipWith f l [0..]
        convert :: RhythmTree -> Int
        convert (Single Note) = 0
        convert (Single Rest) = -1
        convert (Single Tie) = -2
        convert (Branch b) = length b

generateTransition :: [RhythmTree] -> ([(Int, Int)] -> [(Int, Int)] -> Bool) -> [(Int, Int)] -> [(Int, Float)]
generateTransition trees matcher path = normalise $ count $ map snd matchingNotes
    where
        normalise l = map (\ x -> (fst x, fromIntegral (snd x) / fromIntegral (sum (map snd l)))) l
        matchingNotes = filter (matcher path . fst) $ characteriseTrees trees
        count = map (head &&& length) . group . sort

depthArityIndexMatcher :: [(Int, Int)] -> [(Int, Int)] -> Bool
depthArityIndexMatcher path1 path2 = length path1 == length path2 && (null path1 && null path2 || last path1 == last path2)

pathMatcher :: [(Int, Int)] -> [(Int, Int)] -> Bool
pathMatcher = (==)

selectNote :: [(Int, Float)] -> Float -> Int
selectNote [(n, _)] _ = n
selectNote ((x, y):xs) n | n <= y    = x
                         | otherwise = selectNote xs (n - y)

generateTree :: ([(Int, Int)] -> [(Int, Float)]) -> IO RhythmTree
generateTree f = do
    tree <- genTree []
    return $ simplify tree
        where
            genTree :: [(Int, Int)] -> IO RhythmTree
            genTree path = do
                v <- randomIO :: (Random t, Fractional t) => IO t
                let element = selectNote (f path) v
                if element == 0 then
                    return $ Single Note
                else if element == (-1) then
                    return $ Single Rest
                else if element == (-2) then
                    return $ Single Tie
                else do
                    b <- mapM (\i -> genTree (path ++ [(i, element)])) [0..element-1]
                    return $ Branch b
              