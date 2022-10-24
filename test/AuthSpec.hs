{-# LANGUAGE OverloadedStrings #-}
module AuthSpec(spec) where

import Test.Hspec
import AuthImpl

spec :: Spec
spec = do
    describe "Auth" $ do
        it "should successfully open a browser" $ do
            pendingWith "super annoying to open a browser all the time.."
        it "should be able to split url parameters" $ do
            getRequestParameters "aaaa&code=234&poop=shit" `shouldBe` ["aaaa", "code=234", "poop=shit"]
        it "should be able to find one parameter in particular" $ do
            getRequestParameterValue "code" ["aaaa", "code=234", "poop=shit"] `shouldBe` Just "234"
        it "should return nothing if the given parameter does not exist" $ do
            getRequestParameterValue "code" ["aaaa", "poop=shit"] `shouldBe` Nothing
        it "should return the first hit if there are several hits" $ do
            getRequestParameterValue "poop" ["aaaa", "poop=shit", "poop=turd"] `shouldBe` Just "shit"
