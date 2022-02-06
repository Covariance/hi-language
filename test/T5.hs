{-# LANGUAGE OverloadedStrings #-}

module T5 (
    group
  ) where


import Data.Sequence (fromList)
import Data.Text (pack)
import Test.Hspec (Spec, it)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (testSpec)

import HI.Base (HiError (..), HiValue (..))
import Util (evaluatesTo)

spec :: Spec
spec = do
  it "list literals" $ do
    "[\"A\", \"B\", \"C\"]" `evaluatesTo` HiValueList (fromList [
        HiValueString "A",
        HiValueString "B",
        HiValueString "C"
      ])
    "[\"A\", \"B\", 1]" `evaluatesTo` HiValueList (fromList [
        HiValueString "A",
        HiValueString "B",
        HiValueNumber 1
      ])
    "[1, 2, 3]" `evaluatesTo` HiValueList (fromList [
        HiValueNumber 1,
        HiValueNumber 2,
        HiValueNumber 3
      ])

  it "new operations" $ do
    "list(1, 2, 3)" `evaluatesTo` HiValueList (fromList [
        HiValueNumber 1,
        HiValueNumber 2,
        HiValueNumber 3
      ])
    "range(5, 10.3)" `evaluatesTo` HiValueList (fromList [
        HiValueNumber 5,
        HiValueNumber 6,
        HiValueNumber 7,
        HiValueNumber 8,
        HiValueNumber 9,
        HiValueNumber 10
      ])
    "range(0.96, 1.96)" `evaluatesTo` HiValueList (fromList [
        HiValueNumber 0.96,
        HiValueNumber 1.96
      ])
    "fold(add, [11, 22, 33])" `evaluatesTo` HiValueNumber 66
    "fold(mul, [11, 22, 33])" `evaluatesTo` HiValueNumber 7986
    "fold(div, [11, 22, 33])" `evaluatesTo` HiValueNumber (1 / 66)
  -- These two tests were made by Slack Readers Gang
  -- and average Vladislav 'int-index' Zavyalov enjoyers
    "fold(add, [])" `evaluatesTo` HiValueNull
    "fold(add, [11])" `evaluatesTo` HiValueNumber 11

  it "operator overloads" $ do
    "length([1, true, \"Hello\"])" `evaluatesTo` HiValueNumber 3
    "reverse([1, true, \"Hello\"])" `evaluatesTo` HiValueList (fromList [
        HiValueString "Hello",
        HiValueBool True,
        HiValueNumber 1
      ])
    "[1, 2] + [3, 4, 5]" `evaluatesTo` HiValueList (fromList [
        HiValueNumber 1,
        HiValueNumber 2,
        HiValueNumber 3,
        HiValueNumber 4,
        HiValueNumber 5
      ])
    "[0, \"x\"] * 3" `evaluatesTo` HiValueList (fromList [
        HiValueNumber 0,
        HiValueString "x",
        HiValueNumber 0,
        HiValueString "x",
        HiValueNumber 0,
        HiValueString "x"
      ])

  it "complex examples" $ do
    "fold(add, [2, 5] * 3)" `evaluatesTo` HiValueNumber 21
    "fold(mul, range(1, 10))" `evaluatesTo` HiValueNumber 3628800
    "[0, true, false, \"hello\", \"world\"](2, 4)" `evaluatesTo` HiValueList (fromList [
        HiValueBool False,
        HiValueString "hello"
      ])
    "reverse(range(0.5, 70/8))" `evaluatesTo` HiValueList (fromList [
        HiValueNumber 8.5,
        HiValueNumber 7.5,
        HiValueNumber 6.5,
        HiValueNumber 5.5,
        HiValueNumber 4.5,
        HiValueNumber 3.5,
        HiValueNumber 2.5,
        HiValueNumber 1.5,
        HiValueNumber 0.5
      ])


group :: IO TestTree
group = testSpec "T5" spec
