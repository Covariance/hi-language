{-# LANGUAGE LambdaCase #-}
module T8 (
    group
  ) where

import Data.Set (empty)
import Data.Time (diffUTCTime, getCurrentTime, nominalDay)
import Test.Hspec (Spec, anyException, it, shouldSatisfy, shouldThrow)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)

import HI.Action (HiPermission (AllowTime))
import HI.Base (HiValue (HiValueTime))
import Util (isPermissionException, permissions, prints, tryEvaluateWithPermissions)

spec :: Spec
spec = do
  it "calculate time" $ do
    "parse-time(\"2021-12-15 00:00:00 UTC\") + 1000" `prints` "parse-time(\"2021-12-15 00:16:40 UTC\")"
    "parse-time(\"2021-12-15 00:37:51.000890793 UTC\") - parse-time(\"2021-12-15 00:37:47.649047038 UTC\")"
      `prints` "3.351843755"
    "parse-time(\"2021-01-01 00:00:00 UTC\") + 365 * 24 * 60 * 60" `prints`
      "parse-time(\"2022-01-01 00:00:00 UTC\")"

  it "time permissions" $ do
    shouldThrow (tryEvaluateWithPermissions empty "now!") (isPermissionException AllowTime)

  it "time precision" $ do
    now <- tryEvaluateWithPermissions (permissions "t") "now!"
    time <- getCurrentTime

 -- It checks whether difference between measured times is less than a second
 -- which is sort of bad precision, but I don't want this test to flack
    now `shouldSatisfy` (\case
        (Just (Right (HiValueTime nowTime))) ->
          diffUTCTime time nowTime < (nominalDay / 86400)
        _ -> False
      )

group :: IO TestTree
group = testSpec "T8" spec
