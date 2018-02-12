{-# LANGUAGE QuasiQuotes #-}


module ReplSpec(main, spec) where

import Test.Hspec
import HereDoc(heredoc)
import Core
import Repl.Internal
import qualified Data.Map as Map

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "readLibrary" $
        it "should parse texts only with assignments." $
            readLibrary empty sampleLibrary `shouldBe`  Map.singleton "abc" (Atom "a" `App` (Atom "b" `App` Atom "c"))

sampleLibrary :: String
sampleLibrary = [heredoc|
    let abc = a (b c)
|]
