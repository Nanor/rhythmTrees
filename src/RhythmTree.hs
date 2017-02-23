module RhythmTree where

import Control.Monad
import Control.Monad.Random
import Data.List.Split
import Data.List

data RhythmElement = Note | Tie | Rest
    deriving (Show, Eq, Enum, Bounded, Ord, Read)

data RhythmTree = Single RhythmElement | Branch [RhythmTree]
    deriving (Show, Eq, Read)

contents :: RhythmTree -> [RhythmTree]
contents (Branch a) = a
contents (Single a) = [Single a]

randomTree :: IO RhythmTree
randomTree = do
    tree <- evalRandIO (genSub 4)
    return $ simplify tree
        where 
            genSub :: MonadRandom t => Int -> t RhythmTree
            genSub n = do 
                v <- getRandomR (0,n)
                if v == 0
                    then do
                        i <- getRandomR (0, length ([minBound .. maxBound] :: [RhythmElement]) -1)
                        return $ Single $ toEnum i
                    else do
                        s <- replicateM v (genSub (n-1))
                        return $ Branch s

-- Takes a RhythmTree and returns a normalized version of it
simplify :: RhythmTree -> RhythmTree
simplify tree | tree == simpleTree = tree
              | otherwise          = simplify simpleTree
              where simpleTree = simplifyOnce tree
              
simplifyOnce :: RhythmTree -> RhythmTree
simplifyOnce (Branch [a]) = a
simplifyOnce (Branch a) = Branch $ map simplifyOnce $ 
    (colapseWhen (all (== Single Rest)) . colapseWhen (all (== Single Tie) . tail) . removeTiedRests . concatBranches) a
        where
            -- Takes a list of trees and attempts to take strings of rests and colapse them into a single rest
            colapseWhen :: ([RhythmTree] -> Bool) -> [RhythmTree] -> [RhythmTree]
            colapseWhen fn b = maybe b (map (colapseIf fn)) (splitSo fn)
                where
                    -- Turns a list of all rests into a single rest
                    colapseIf :: ([RhythmTree] -> Bool) -> [RhythmTree] -> RhythmTree
                    colapseIf fn b | fn b      = head b
                                   | otherwise = Branch b
                    -- Returns a partition of branches where at least one of the partitions is all rests, or nothing
                    splitSo :: ([RhythmTree] -> Bool) -> Maybe [[RhythmTree]]
                    splitSo fn = find (any fn) $ split b
                    -- Returns a list of factors of n
                    factors :: Int -> [Int]
                    factors n = [i | i <-[1..n], mod n i == 0]
                    -- Returns all ways of splitting a list into smaller equally sized lists
                    split :: [a] -> [[[a]]]
                    split l = [chunksOf n l | n <- (tail . factors . length) l]
            -- Takes multiple branches and turns them into a single branch if they're all the same length
            concatBranches :: [RhythmTree] -> [RhythmTree]
            concatBranches b@(x:xs) | all (== (length . contents) x) (map (length . contents) xs) = concatMap contents b
                                    | otherwise = b
            -- Replaces a tie that follows a rest with a rest
            removeTiedRests :: [RhythmTree] -> [RhythmTree]
            removeTiedRests [] = []
            removeTiedRests [x] = [x]
            removeTiedRests (x:y:xs) | (x == Single Rest) && (y == Single Tie) = x : removeTiedRests (Single Rest : xs)
                                     | otherwise                               = x : removeTiedRests (y:xs)
simplifyOnce (Single a) = Single a

compareTrees :: RhythmTree -> RhythmTree -> Int
compareTrees (Single a) (Single b) | a == b    = 0
                                   | otherwise = 1
compareTrees (Single a) (Branch b) = sum (map (compareTrees (Single a)) b)
compareTrees (Branch a) (Single b) = compareTrees (Single b) (Branch a)
compareTrees (Branch a) (Branch b) = inner a b (length a) (length b)
    where
        inner :: [RhythmTree] -> [RhythmTree] -> Int -> Int -> Int
        inner a b x y = inner1 x y
            where
                inner1 a b = map inner2 [(i,j) | i <- [0..x], j <- [0..y]] !! (a * (y + 1) + b)
                inner2 :: (Int, Int) -> Int
                inner2 (0, 0) = 0
                inner2 (i, 0) = inner1 (i - 1) 0 + cost (a !! (i - 1))
                inner2 (0, j) = inner1 0 (j - 1) + cost (b !! (j - 1))
                inner2 (i, j) = minimum [
                        inner1 (i - 1) j + cost (a !! (i - 1)),
                        inner1 i (j - 1) + cost (b !! (j - 1)),
                        inner1 (i - 1) (j - 1) + compareTrees (a !! (i - 1)) (b !! (j - 1))
                    ]
        cost :: RhythmTree -> Int
        cost (Single _) = 1
        cost (Branch b) = sum (map cost b) + 1
