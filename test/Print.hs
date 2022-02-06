module Print (
    group
  ) where

import Test.Hspec (Spec, it)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)

import HI.Base (HiError (..), HiExpr (..), HiFun (..), HiValue (..))
import Util (prints)

spec :: Spec
spec = do
  it "numbers" $ do
    "0.05" `prints` "0.05"
    "1 / 2" `prints` "0.5"
    "10 / 3" `prints` "3 + 1/3"
    "3.0" `prints` "3"
    "-30" `prints` "-30"
    "1e3" `prints` "1000"
    "div(3, 5)" `prints` "0.6"
    "3.14" `prints` "3.14"
    "div(1, 3)" `prints` "1/3"
    "div(3, 3)" `prints` "1"
    "div(-3, 3)" `prints` "-1"
    "div(-1, 3)" `prints` "-1/3"
    "div(4, 3)" `prints` "1 + 1/3"
    "div(4, -3)" `prints` "-1 - 1/3"
    "div(1, 20)" `prints` "0.05"

  it "containers" $ do
    "[]" `prints` "[ ]"
    "[##]" `prints` "[# #]"
    "{}" `prints` "{ }"
    "[1]" `prints` "[ 1 ]"
    "[#00#]" `prints` "[# 00 #]"
    "{0:1}" `prints` "{ 0: 1 }"

group :: IO TestTree
group = testSpec "Print" spec
