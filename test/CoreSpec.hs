{-# LANGUAGE QuasiQuotes #-}


module CoreSpec(main, spec) where

import Test.Hspec
import Core.Internal
import qualified Data.Map as Map

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "parse" $ do
        it "should parse var" $
            parse "x" `shouldBe` rr (Atom "x")
        it "should parse integer" $
            parse "2" `shouldBe` rr  (CInt 2)
        it "should parse application" $
            parse "a b c" `shouldBe` rr sampleAbc
        it "should parse parens" $
            parse "a (b c)" `shouldBe` rr sampleABc
        it "should parse strangely spaced expression" $
            parse "   a(   2  c    )    " `shouldBe` rr samplea2c

rr :: Term -> Either a Statement
rr = Right . RawTerm

sampleAbc :: Term
sampleAbc = Atom "a" `App` Atom "b" `App` Atom "c"

sampleABc :: Term
sampleABc = Atom "a" `App` (Atom "b" `App` Atom "c")

samplea2c :: Term
samplea2c = Atom "a" `App` (CInt 2 `App` Atom "c")