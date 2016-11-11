module RhythmTreeSpec (main, spec) where

import Test.Hspec
import RhythmTree

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "contents" $ do
        it "should return the contents of a branch" $
            contents (Branch [Single Note, Single Rest]) `shouldBe` [Single Note, Single Rest]
            
        it "should return a single inside a list" $
            contents (Single Note) `shouldBe` [Single Note]

    describe "simplify" $ do
        it "should simplify nested single length branches" $
            simplify (Branch [Branch [Single Note], Single Note]) `shouldBe` Branch [Single Note, Single Note]

        it "should simplify two branches of the same length into one" $
            simplify (Branch [Branch [Single Note, Single Rest], Branch [Single Rest, Single Note]]) `shouldBe`
                Branch [Single Note, Single Rest, Single Rest, Single Note]

        it "should simplify multiple times" $
            simplify (Branch [Branch [Branch [Branch [Single Note, Single Rest]]]]) `shouldBe`
                Branch [Single Note, Single Rest]