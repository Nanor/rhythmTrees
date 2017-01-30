module MarkovSpec (main, spec) where

import Test.Hspec
import RhythmTree
import Markov

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "characteriseTree" $
        it "should characterise a single tree" $
            characteriseTree (Branch [Single Note, Branch [Single Note, Single Rest]]) `shouldBe`
                [(0,0,0,2),(1,2,0,0),(1,2,1,2),(2,2,0,0),(2,2,1,-1)]
    describe "generateTransition" $
        it "should make a function" $ do
            generateTransition [Branch [Single Note, Branch [Single Note, Single Rest]], Branch [Single Note, Branch [Single Note, Single Note]]] (2, 2, 1) `shouldBe`
                [(-1, 0.5), (0, 0.5)]
            generateTransition [Branch [Single Note, Branch [Single Note, Single Rest]]] (0, 0, 0) `shouldBe`
                [(2, 1)]
    describe "selectNote" $
        it "should pick a note from a transition function" $ do
            selectNote [(1, 0.3), (2, 0.5), (3, 0.2)] 0.2 `shouldBe` 1
            selectNote [(1, 0.3), (2, 0.5), (3, 0.2)] 0.3 `shouldBe` 1
            selectNote [(1, 0.3), (2, 0.5), (3, 0.2)] 0.5 `shouldBe` 2
            selectNote [(1, 0.3), (2, 0.5), (3, 0.2)] 0.9 `shouldBe` 3
            selectNote [(1, 0.3), (2, 0.5), (3, 0.2)] 1 `shouldBe` 3
    describe "generateTree" $
        it "should generate the only tree it's seen" $ do
            let transition = generateTransition [Branch [Single Note, Branch [Single Note, Single Rest]]]
            let trees = generateTree transition
            tree1 <- trees
            tree1 `shouldBe` Branch [Single Note, Branch [Single Note, Single Rest]]