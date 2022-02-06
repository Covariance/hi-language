{-# LANGUAGE OverloadedStrings #-}

module T4 (
    group
  ) where

import Data.Text (pack)
import Test.Hspec (Spec, it)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (testSpec)

import HI.Base (HiError (..), HiValue (..))
import Util (evaluatesTo, failsWith)

spec :: Spec
spec = do
  it "new functions" $ do
    "length(\"Hello World\")" `evaluatesTo` HiValueNumber 11
    "to-upper(\"Hello World\")" `evaluatesTo` HiValueString "HELLO WORLD"
    "to-lower(\"Hello World\")" `evaluatesTo` HiValueString "hello world"
    "reverse(\"stressed\")" `evaluatesTo` HiValueString "desserts"
    "trim(\" Hello World \")" `evaluatesTo` HiValueString "Hello World"

  it "operator overloads" $ do
    "\"Hello\" + \"World\"" `evaluatesTo` HiValueString "HelloWorld"
    "\"Cat\" * 5" `evaluatesTo` HiValueString "CatCatCatCatCat"
    "\"/home/user\" / \"hi\"" `evaluatesTo` HiValueString "/home/user/hi"

  it "indexing" $ do
    "\"Hello World\"(0)" `evaluatesTo` HiValueString "H"
    "\"Hello World\"(7)" `evaluatesTo` HiValueString "o"
    "\"Hello World\"(-1)" `evaluatesTo` HiValueNull
    "\"Hello World\"(99)" `evaluatesTo` HiValueNull

  it "slicing" $ do
    "\"Hello World\"(0, 5)" `evaluatesTo` HiValueString "Hello"
    "\"Hello World\"(2, 4)" `evaluatesTo` HiValueString "ll"
    "\"Hello World\"(0, -4)" `evaluatesTo` HiValueString "Hello W"
    "\"Hello World\"(-4, -1)" `evaluatesTo` HiValueString "orl"
    "\"Hello World\"(2, null)" `evaluatesTo` HiValueString "llo World"
    "\"Hello World\"(null, 5)" `evaluatesTo` HiValueString "Hello"
  -- This test is made by Python gang
    "\"test\"(-6, null)" `evaluatesTo` HiValueString "test"

  it "complex examples" $ do
    "to-upper(\"what a nice language\")(7, 11)" `evaluatesTo` HiValueString "NICE"
    "\"Hello\" == \"World\"" `evaluatesTo` HiValueBool False
    "length(\"Hello\" + \"World\")" `evaluatesTo` HiValueNumber 10
    "length(\"hehe\" * 5) / 3" `evaluatesTo` HiValueNumber (6 + 2/3)

  it "errors" $ do
    "\"1000\" * (-7)" `failsWith` HiErrorInvalidArgument
    "\"Hello\" + 42 + \"World\"" `failsWith` HiErrorInvalidArgument
    "\"Hello\" / 42" `failsWith` HiErrorInvalidArgument

  it "Q&A" $ do
    "\"suicide\"(4,100)" `evaluatesTo` HiValueString "ide"

group :: IO TestTree
group = testSpec "T4" spec
