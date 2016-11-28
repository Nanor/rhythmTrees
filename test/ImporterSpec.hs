module ImporterSpec (main, spec) where

import Test.Hspec
import RhythmTree as RT
import Euterpea as E
import Data.Ratio

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
    describe "fromEuterpea" $
        it "should convert back to the correct RhythmTree" $
            fromEuterpea (line [note qn ((C, 3), []), note en ((C, 3), []), note en ((C, 3), []), rest hn]) `shouldBe`
                Branch [Branch [Single RT.Note, Branch [Single RT.Note, Single RT.Note]], Single RT.Rest]