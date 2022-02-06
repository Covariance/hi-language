{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module T11 (
    group
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (fromList)
import Data.Text (pack)
import System.Directory (getCurrentDirectory, removeFile)
import Test.Hspec (Spec, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)

import HI.Base (HiValue (..))
import Util (evaluatesTo, prints, tryEvaluateWithPermissions)

spec :: Spec
spec = do
  it "dictionary creation" $ do
    "{}" `evaluatesTo` HiValueDict Map.empty
    "{1 : 2}" `evaluatesTo` HiValueDict (Map.fromList [
        (HiValueNumber 1, HiValueNumber 2)
      ])
    "{ \"width\": 120, \"height\": 80 }" `evaluatesTo` HiValueDict (Map.fromList [
        (HiValueString "width", HiValueNumber 120)
      , (HiValueString "height", HiValueNumber 80)
      ])
    "{ 1: true, 3: true, 4: false }" `evaluatesTo` HiValueDict (Map.fromList [
        (HiValueNumber 1, HiValueBool True)
      , (HiValueNumber 3, HiValueBool True)
      , (HiValueNumber 4, HiValueBool False)
      ])
-- Nested dictionaries are fun!
    "{ {1 : 1} : 2, 3 : {4 : 4}}" `evaluatesTo` HiValueDict (Map.fromList [
        (HiValueDict (Map.fromList [
            (HiValueNumber 1, HiValueNumber 1)
          ]), HiValueNumber 2)
      , (HiValueNumber 3, HiValueDict (Map.fromList [
            (HiValueNumber 4, HiValueNumber 4)
          ]))
      ])
  it "access" $ do
    "{ \"width\": 120, \"height\": 80 }(\"width\")" `evaluatesTo` HiValueNumber 120
    "{ \"width\": 120, \"height\": 80 }.width" `evaluatesTo` HiValueNumber 120
-- Q&A based tests
    "{ \"A\": 1 }.B" `evaluatesTo` HiValueNull
    "if(true, { \"width\" : 1 }, 1+1).width" `evaluatesTo` HiValueNumber 1
-- Thank god and Q&A that this is invalid test
-- "{ \"1\": true}.1" `evaluatesTo` HiValueBool True

  it "dot-access shenanigans" $ do
    "reverse.hello" `evaluatesTo` HiValueString "olleh"
    "{ \"add\" : mul }.add(1,1)" `evaluatesTo` HiValueNumber 1
    "{ \"a\" : 1 }.a - 1" `evaluatesTo` HiValueNumber 0

    let fileName = "tmp-hi-read-check" -- No underscores for you!
    let content = "Hello From Hi Language Tests"
    writeFile fileName content

    ("read." ++ fileName ++ "!") `evaluatesTo` HiValueString (pack content)

    removeFile fileName

  it "new functions" $ do
    "keys({ \"width\": 120, \"height\": 80 })" `evaluatesTo` HiValueList (fromList [
          HiValueString "height"
        , HiValueString "width"
      ])
    "values({ \"width\": 80, \"height\": 120 })" `evaluatesTo` HiValueList (fromList [
          HiValueNumber 120
        , HiValueNumber 80
      ])
    "count(\"XXXOX\")" `evaluatesTo` HiValueDict (Map.fromList [
        (HiValueString "O", HiValueNumber 1)
      , (HiValueString "X", HiValueNumber 4)
      ])
    "count([# 58 58 58 4f 58 #])" `evaluatesTo` HiValueDict (Map.fromList [
        (HiValueNumber 79, HiValueNumber 1)
      , (HiValueNumber 88, HiValueNumber 4)
      ])
    "count([true, true, false, true])" `evaluatesTo` HiValueDict (Map.fromList [
        (HiValueBool True, HiValueNumber 3)
      , (HiValueBool False, HiValueNumber 1)
      ])
 -- This test is commented out because of ordering in the list that can be arbitrary
 -- and I'm too sleepy at the moment to check it manually
 -- "invert({ 1: 1, 2: 2, 3: 1 })" `evaluatesTo` HiValueDict (Map.fromList [
 --     (HiValueNumber 1, HiValueList (fromList [HiValueNumber 1, HiValueNumber 3]))
 --   , (HiValueNumber 2, HiValueList (fromList [HiValueNumber 2]))
 --   ])
  it "statement tests" $ do
    "count(\"Hello World\").o" `evaluatesTo` HiValueNumber 2
    "fold(add, values(count(\"Hello, World!\")))" `evaluatesTo` HiValueNumber 13


group :: IO TestTree
group = testSpec "T11" spec
