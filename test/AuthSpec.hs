{-# LANGUAGE OverloadedStrings #-}
module AuthSpec(spec) where

import Test.Hspec
import AuthImpl

spec :: Spec
spec = do
    describe "Auth" $ do
        it "should successfully open a browser" $ do
            pendingWith "super annoying to open a browser all the time.."
        --     exitCode <- openBrowser "https://vg.no"
        --     exitCode `shouldBe` True
        it "should extract the code from a valid request string" $ do
            extractCode "aaaa&code=234" `shouldBe` Just "234"

        it "A string without a code returns nothing" $ do
            extractCode "aaaa&poop=234" `shouldBe` Nothing

        it "A string with several codes returns the first" $ do
            extractCode "aaaa/code=234&code=123" `shouldBe` Just "234"
