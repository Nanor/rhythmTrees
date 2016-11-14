module RhythmTree where

import Control.Monad
import Control.Monad.Random
import Data.List.Split
import Data.List

data RhythmElement = Note | Rest-- | Tie
    deriving (Show, Eq, Enum, Bounded)

data RhythmTree = Single RhythmElement | Branch [RhythmTree]
    deriving (Show, Eq)

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

simplifyOnce :: RhythmTree -> RhythmTree
simplifyOnce (Branch [a]) = a
simplifyOnce (Branch a) = Branch $ map simplifyOnce $ (colapseRests . concatBranches) a
    where
        colapseRests :: [RhythmTree] -> [RhythmTree]
        colapseRests b = maybe b (map removeRests) part
            where
                removeRests :: [RhythmTree] -> RhythmTree
                removeRests b | all (== Single Rest) b = Single Rest
                              | otherwise              = Branch b 
                part :: Maybe [[RhythmTree]]
                part = find (any $ all (== Single Rest)) $ split b
                factors :: Int -> [Int]
                factors n = [i | i <-[1..n], mod n i == 0]
                split :: [a] -> [[[a]]]
                split l = [chunksOf n l | n <- (tail . factors . length) l]
        concatBranches :: [RhythmTree] -> [RhythmTree]
        concatBranches b@(x:xs) | all (== (length . contents) x) (map (length . contents) xs) = concatMap contents b
                                | otherwise = b
simplifyOnce (Single a) = Single a

simplify :: RhythmTree -> RhythmTree
simplify tree | tree == simpleTree = tree
              | otherwise          = simplify simpleTree
              where simpleTree = simplifyOnce tree