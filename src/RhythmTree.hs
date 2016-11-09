module RhythmTree where

import System.Random
import Control.Monad.Random
import Control.Monad

data RhythmElement = Note | Rest-- | Tie
    deriving (Show, Eq, Enum, Bounded)

data RhythmTree = Single RhythmElement | Branch [RhythmTree]
    deriving (Show, Eq)

instance Random RhythmElement where
    randomR (a, b) g =
        case randomR (fromEnum a, fromEnum b) g of
            (x, g') -> (toEnum x, g')
    random = randomR (minBound, maxBound)

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
simplifyOnce (Branch a) | length a == 1 = head a
                        | otherwise     = Branch $ map simplifyOnce a
simplifyOnce (Single a) = Single a

simplify :: RhythmTree -> RhythmTree
simplify tree | tree == simpleTree = tree
              | otherwise          = simplify simpleTree
              where simpleTree = simplifyOnce tree

test = do
    let tree = Branch [Single Rest,Single Note,Branch [Branch [Single Rest,Branch [Single Note]]],Branch [Branch [Branch [Single Rest]]]]
    print tree
    print $ simplify tree
    print $ tree == tree

testTree :: RhythmTree
testTree = Branch [
        Branch [
            Single Note,
            Single Rest,
            Single Note,
            Single Note,
            Single Rest,
            Single Note,
            Single Note,
            Single Rest
        ],
        Branch [
            Single Rest,
            Single Note,
            Single Rest,
            Single Rest,
            Single Note,
            Single Rest,
            Single Rest,
            Single Note
        ],
        Branch [
            Branch [
                Single Rest,
                Branch [
                    Single Note,
                    Single Rest
                ]
            ],
            Single Rest
        ]
    ]
