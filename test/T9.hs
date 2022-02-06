{-# LANGUAGE LambdaCase #-}
module T9 (
    group
  ) where

import Control.Monad (forM_)
import Data.Maybe (fromJust)
import Test.Hspec (Spec, it, shouldSatisfy)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (testSpec)

import HI.Base (HiValue (HiValueNumber))
import Util (permissions, prints, tryEvaluateWithPermissions)

unitSpec :: Spec
unitSpec = do
  it "action prints correctly" $ do
    "rand(0, 10)" `prints` "rand(0, 10)"
    "rand(-100, 100)" `prints` "rand(-100, 100)"
    "rand(10 - 10, 2 * 5)" `prints` "rand(0, 10)"

propertySpec :: Spec
propertySpec = do
  it "random is in bounds" $ forM_ [1..100] $ \_ -> do
-- It was late night, I was on the fumes of Red Bull and my mind was already
-- flowing away, so I decided to write a test that would check
-- whether the random is really uniform. I implemented
-- chi-square test (https://en.wikipedia.org/wiki/Chi-squared_test)
-- only to later understand that it would still flack in some cases
-- so I erased it and decided to leave this meaningless test here
--
-- Exam in math statistics is in 3 days, wish me luck
    res <- tryEvaluateWithPermissions (permissions "rwt") "rand(0, 10)!"
    res `shouldSatisfy` (\case
        Just (Right (HiValueNumber x)) -> 0 <= x && x <= 10
        _                              -> False
      )


group :: IO TestTree
group = do
    unit <- testSpec "unit" unitSpec
    property <- testSpec "property" propertySpec
    return $ testGroup "T9" [unit, property]
