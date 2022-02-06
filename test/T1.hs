module T1 (
    group
  ) where


import Test.Hspec (Spec, it)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)

import HI.Base (HiError (..), HiValue (HiValueNumber))
import Util (evaluatesTo, failsWith)

spec :: Spec
spec = do
  it "statement" $ do
    "mul(2, 10)" `evaluatesTo` HiValueNumber 20
    "sub(1000, 7)" `evaluatesTo` HiValueNumber 993 -- Dead Inside Test, not my fault
    "div(3, 5)" `evaluatesTo` HiValueNumber 0.6
    "100" `evaluatesTo` HiValueNumber 100
    "-15" `evaluatesTo` HiValueNumber (-15)
    "add(100, -15)" `evaluatesTo` HiValueNumber 85
    "add(3, div(14, 100))" `evaluatesTo` HiValueNumber 3.14
    "div(10, 3)" `evaluatesTo` HiValueNumber (3 + 1/3)
    "sub(mul(201, 11), 0.33)" `evaluatesTo` HiValueNumber 2210.67

  it "arity errors" $ do
    "div(57)" `failsWith` HiErrorArityMismatch
    "sub(1, 2, 3)" `failsWith` HiErrorArityMismatch
    "add(1, 2, 3)" `failsWith` HiErrorArityMismatch
    "mul()" `failsWith` HiErrorArityMismatch

  it "dbz errors" $ do
    "div(1, 0)" `failsWith` HiErrorDivideByZero
    "div(1, sub(5, 5))" `failsWith` HiErrorDivideByZero

  it "type mismatch errors" $ do
    "15(2)" `failsWith` HiErrorInvalidFunction
    "sub(10, add)" `failsWith` HiErrorInvalidArgument

group :: IO TestTree
group = testSpec "T1" spec
