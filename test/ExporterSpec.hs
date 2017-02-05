module ExporterSpec (main, spec) where

import Test.Hspec
import RhythmTree as RT
import Euterpea as E
import Exporter

main :: IO ()
main = hspec spec

spec :: Spec
spec = 
    describe "toEuterpea" $
        it "should convert a tree to music" $
            toEuterpea 1 (Branch [Single RT.Note, Single RT.Note, Branch [Single RT.Note, Single RT.Note], Single RT.Note]) `shouldBe`
                line [note qn ((C, 3), []), note qn ((C, 3), []), note en ((C, 3), []), note en ((C, 3), []), note qn ((C, 3), [])]