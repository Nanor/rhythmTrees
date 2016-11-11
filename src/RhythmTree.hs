module RhythmTree where

import Control.Monad
import Control.Monad.Random

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
simplifyOnce (Branch a) | allEqualLength a = Branch $ concatMap contents a
                        | otherwise        = Branch $ map simplifyOnce a
                        where allEqualLength (x:xs) = all (== (length . contents) x) (map (length . contents) xs)
simplifyOnce (Single a) = Single a

simplify :: RhythmTree -> RhythmTree
simplify tree | tree == simpleTree = tree
              | otherwise          = simplify simpleTree
              where simpleTree = simplifyOnce tree