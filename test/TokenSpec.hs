module TokenSpec where

import SAD.Parser.Token
import SAD.Core.SourcePos

import Test.Hspec

shouldTokenize :: String -> [String] -> Expectation
s `shouldTokenize` ts = (showToken `fmap` tokenize noPos s) `shouldBe` ts

spec :: Spec
spec = do
  describe "tokenize" $ do
    it "recognizes words" $ do
      "a" `shouldTokenize` ["a","EOF"]

