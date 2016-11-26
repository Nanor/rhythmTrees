module ImporterSpec (main, spec) where

import Test.Hspec
import RhythmTree as RT
import Euterpea as E
import Importer

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "unpack" $
        it "should convert music into a list of notes" $
            unpack (line [note qn ((C, 3), []), note en ((C, 3), []), note en ((C, 3), []), rest hn]) `shouldBe`
                [(RT.Note, qn), (RT.Note, en), (RT.Note, en), (RT.Rest, hn)]
    describe "fromEuterpea" $
        it "should convert back to the correct RhythmTree" $
            fromEuterpea (line [note qn ((C, 3), []), note en ((C, 3), []), note en ((C, 3), []), rest hn]) `shouldBe`
                Branch [Branch [Single RT.Note, Branch [Single RT.Note, Single RT.Note]], Single RT.Rest]