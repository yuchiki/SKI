{-# LANGUAGE QuasiQuotes #-}


module ReplSpec(main, spec) where

import Test.Hspec
import HereDoc(heredoc)
import Core(empty)
import Repl

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "readLibrary" $
        it "good yay" $ 1 `shouldBe` 1
--            readLibrary empty sampleLibrary `shouldBe` "hay!"

sampleLibrary :: String
sampleLibrary = [heredoc|
    let threeI = i (i i)
|]
