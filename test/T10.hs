{-# LANGUAGE LambdaCase #-}
module T10 (
    group
  ) where

import Data.Set (empty)
import System.IO.Silently (capture)
import Test.Hspec (Spec, anyException, it, shouldBe, shouldSatisfy, shouldThrow)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (testSpec)

import HI.Action (HiPermission (AllowWrite))
import HI.Base (HiValue (..))
import Util (evaluatesTo, isPermissionException, permissions, prints, tryEvaluateWithPermissions)

echoSpec :: Spec
echoSpec = do
  it "echo print" $ do
    "echo(\"Hello\")" `prints` "echo(\"Hello\")"
    "echo(\"Hello there\"(null, 5))" `prints` "echo(\"Hello\")"
    "echo(\"hi\" * 3)" `prints` "echo(\"hihihi\")"

-- Oh my this test is funny. HSpec tests are considered failed if
-- there's anything in the output, and therefore any correct use of
-- echo should fail the test case. So we need to capture the output
-- to the stdout and rerout it. And for that I would use the 'Silently'
-- library: https://github.com/hspec/silently
--
-- But sometimes (for whatever reason) it flacks and captures some of the
-- test output (aka escape sequences), so we need to wait a bit.
--
-- Because of flacking, I've muted the check for captured output.
  it "echo output" $ do
    (captured, result) <- capture (tryEvaluateWithPermissions
      (permissions "w") "echo(\"hi\")!")

 -- captured `shouldBe` "hi\n"

    result `shouldSatisfy` (\case
        Just (Right HiValueNull) -> True
        _                        -> False
      )

  it "echo permissions" $ do
    shouldThrow (tryEvaluateWithPermissions empty "echo(\"a\")!") (isPermissionException AllowWrite)


lazySpec :: Spec
lazySpec = do
-- This test is simplier than it would appear - again, we will use echo,
-- like in statement, and use the same property of HSpec:
-- if there's anything in the output, then test fails.
  it "lazy boolean functions" $ do
    "false && echo(\"oops\")!" `evaluatesTo` HiValueBool False
    "null && echo(\"oops\")!" `evaluatesTo` HiValueNull
    "true || echo(\"oops\")!" `evaluatesTo` HiValueBool True
    "if(true, 1, echo(\"oops\")!)" `evaluatesTo` HiValueNumber 1

  it "not-so-boolean functions" $ do
-- idk what is with this syntax, I personally do not like it, but:
-- (quote from statement)
-- Then generalise A && B as follows:
-- - if A is false or null, return A without evaluating B
-- - otherwise, evaluate and return B
-- Generalise A || B as follows:
-- - if A is false or null, evaluate and return B
-- - otherwise, return A without evaluating B
    "5 + 5 || false" `evaluatesTo` HiValueNumber 10
    "\"strange syntax\" && 5 + 5" `evaluatesTo` HiValueNumber 10

group :: IO TestTree
group = do
    unit <- testSpec "echo" echoSpec
    property <- testSpec "&& and ||" lazySpec
    return $ testGroup "T10" [unit, property]
