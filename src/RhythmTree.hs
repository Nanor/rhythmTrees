module RhythmTree where

import System.Random
import Control.Monad.Random
import Control.Monad
import Data.Functor.Identity

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
randomTree = evalRandIO (genSub 4)
        where 
            genSub :: Int -> RandT StdGen Identity RhythmTree
            genSub n = do 
                v <- getRandomR (0,n)
                if v == 0
                    then do
                        i <- getRandomR (0,1)
                        return $ Single $ [Note, Rest] !! i
                    else do
                        s <- replicateM v (genSub (n-1))
                        return $ Branch s

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
