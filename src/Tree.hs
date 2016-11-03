module Tree where

data RhythmElement = NoteElement | RestElement | TieElement
    deriving (Show, Eq)

data RhythmTree = Single RhythmElement | Branch [RhythmTree]
    deriving (Show, Eq)

testTree :: RhythmTree
testTree = Branch [
        Branch [
            Single NoteElement,
            Single RestElement,
            Single NoteElement,
            Single NoteElement,
            Single RestElement,
            Single NoteElement,
            Single NoteElement,
            Single RestElement
        ],
        Branch [
            Single RestElement,
            Single NoteElement,
            Single RestElement,
            Single RestElement,
            Single NoteElement,
            Single RestElement,
            Single RestElement,
            Single NoteElement
        ],
        Branch [
            Branch [
                Single RestElement,
                Branch [
                    Single NoteElement,
                    Single RestElement
                ]
            ],
            Single RestElement
        ]
    ]
