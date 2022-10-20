module ConfigSpec(spec) where

import Test.Hspec
import Config
spec :: Spec
spec = do
    describe "Config" $ do
        describe "parser" $ do
            it "Parsing should work" $ do
                pending
            it "subparsers should be tested" $ do
                pending
        describe "Validation" $ do
            it "should reject invalid configs" $ do
                pending
            it "should accept valid configs" $ do
                pending
