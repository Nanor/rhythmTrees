module ImporterSpec (main, spec) where

import Test.Hspec
import RhythmTree as RT
import Euterpea as E
import Data.Ratio
import Control.Exception.Base

import Importer

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "unpack" $ do
        it "should convert music into a list of notes" $
            unpack (line [note qn ((C, 3), []), note en ((C, 3), []), note en ((C, 3), []), rest hn]) `shouldBe`
                [(RT.Note, qn), (RT.Note, en), (RT.Note, en), (RT.Rest, hn)]
        it "should unpack from the music produced from midi" $
            unpack (Prim (E.Note 1 ((G,4),[Volume 80])) :=: ((Prim (E.Rest (1 % 16)) :+: Prim (E.Note (15 % 16) ((Ds,4),[Volume 80]))) :=: (Prim (E.Rest (3 % 16)) :+: Prim (E.Note (3 % 4) ((Gs,4),[Volume 80]))))) `shouldBe`
                [(RT.Note, 1 % 16),(RT.Note, 1 % 8),(RT.Note, 3 % 4),(RT.Rest, 1 % 16)]
    describe "toRhythmTree" $ do
        it "should convert back to the correct RhythmTree" $
            toRhythmTree (line [note qn ((C, 3), []), note en ((C, 3), []), note en ((C, 3), []), rest hn]) `shouldBe`
                Branch [Branch [Branch [Single RT.Note, Branch [Single RT.Note, Single RT.Note]], Single RT.Rest]]
        it "should enforce splitting into bars" $
            toRhythmTree (line [note (3 % 4) ((C, 3), []), rest qn, note qn ((C, 3), []), note qn ((C, 3), []), note qn ((C, 3), []), note qn ((C, 3), []), note qn ((C, 3), []), note qn ((C, 3), []), note qn ((C, 3), []), note qn ((C, 3), [])]) `shouldBe`
                Branch [
                    Branch [Single RT.Note,Branch [Single Tie,Single RT.Rest]],
                    Branch [Single RT.Note,Single RT.Note,Single RT.Note,Single RT.Note],
                    Branch [Single RT.Note,Single RT.Note, Single RT.Note,Single RT.Note]
                ]
        it "should enforce splitting into bars with tied bars" $
            toRhythmTree (line [note (3 % 4) ((C, 3), []), note hn ((C, 3), []), note qn ((C, 3), []), note qn ((C, 3), []), note qn ((C, 3), []), note qn ((C, 3), []), note qn ((C, 3), []), note qn ((C, 3), []), note qn ((C, 3), [])]) `shouldBe`
                Branch [
                    Branch [Single RT.Note,Branch [Single Tie,Single RT.Note]],
                    Branch [Single RT.Tie,Single RT.Note,Single RT.Note,Single RT.Note],
                    Branch [Single RT.Note,Single RT.Note,Single RT.Note,Single RT.Note]
                ]
        it "should pad the end with rests to split into bars" $
            toRhythmTree (line [note (3 % 4) ((C, 3), []), note qn ((C, 3), []), note qn ((C, 3), []), note qn ((C, 3), [])]) `shouldBe`
                Branch [
                    Branch [Single RT.Note, Branch [Single Tie, Single RT.Note]],
                    Branch [Branch [Single RT.Note, Single RT.Note], Single RT.Rest]
                ]
    describe "splitIntoN" $ do
        it "should split a list correctly" $
            splitIntoN [(RT.Note, 1), (RT.Note, 1), (RT.Note, 2)] 2 `shouldBe`
                Just [[(RT.Note, 1), (RT.Note, 1)], [(RT.Note, 2)]]
        it "should fail if it doesn't split equally" $
            splitIntoN [(RT.Note, 1), (RT.Note, 1), (RT.Note, 2)] 3 `shouldBe`
                Nothing
        it "should fail if it splits with some left over" $
            splitIntoN [(RT.Note, 1), (RT.Note, 2), (RT.Note, 2)] 3 `shouldBe`
                Nothing
    describe "splitEqually" $ do 
        it "should split a list correctly" $
            splitEqually [(RT.Note, 1), (RT.Note, 1), (RT.Note, 2)] `shouldBe`
                [[(RT.Note, 1), (RT.Note, 1)], [(RT.Note, 2)]]
        it "should fail if it doesn't split equally" $
            splitEqually [(RT.Note, 1), (RT.Note, 2), (RT.Note, 2)] `shouldBe`
                [[(RT.Note, 1)], [(RT.Note, 1)], [(RT.Tie, 1)], [(RT.Note, 1)], [(RT.Tie, 1)]]