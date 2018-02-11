{-# LANGUAGE QuasiQuotes #-}

module HereDocSpec(main, spec) where

import Test.Hspec
import HereDoc

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "heredoc" $ do
        it "should parse 4-indented heredocuments" $
            sampleWith4Indents `shouldBe` sample
        it "should parse 3-indented heredocuments" $
            sampleWith3Indents `shouldBe` sample
        it "should parse 0-indented heredocuments" $
            sampleWithoutIndents `shouldBe` sample

sample :: String
sample = "This is a\n    minimal\nExample.\n"

sampleWith4Indents :: String
sampleWith4Indents = [heredoc|
    This is a
        minimal
    Example.
|]

sampleWith3Indents :: String
sampleWith3Indents = [heredoc|
   This is a
       minimal
   Example.
|]

sampleWithoutIndents :: String
sampleWithoutIndents = [heredoc|
This is a
    minimal
Example.
|]
