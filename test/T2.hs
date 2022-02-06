module T2 (
    group
  ) where


import Test.Hspec (Spec, it)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (testSpec)

import HI.Base (HiFun (HiFunAdd), HiValue (..))
import Util (evaluatesTo)

unitSpec :: Spec
unitSpec = do
  it "boolean logic" $ do
    "not(true)" `evaluatesTo` HiValueBool False
    "and(true, false)" `evaluatesTo` HiValueBool False
    "or(true, false)" `evaluatesTo` HiValueBool True

  it "equals" $ do
    "equals(10, 10)" `evaluatesTo` HiValueBool True
    "equals(false, false)" `evaluatesTo` HiValueBool True
    "equals(3, 10)" `evaluatesTo` HiValueBool False
    "equals(1, true)" `evaluatesTo` HiValueBool False

  it "comparisons" $ do
    "less-than(3, 10)" `evaluatesTo` HiValueBool True
    "less-than(false, true)" `evaluatesTo` HiValueBool True
    "less-than(false, 0)" `evaluatesTo` HiValueBool True

  it "bare values" $ do
    "true" `evaluatesTo` HiValueBool True
    "false" `evaluatesTo` HiValueBool False

  it "functions as values (in terms of equality)" $ do
    "equals(mul, mul)" `evaluatesTo` HiValueBool True
    "not-equals(sub, mul)" `evaluatesTo` HiValueBool True

  it "functions as values (in terms of application)" $ do
    "if(true, add, mul)" `evaluatesTo` HiValueFunction HiFunAdd
    "if(true, add, mul)(10, 10)" `evaluatesTo` HiValueNumber 20
    "if(false, add, mul)(10, 10)" `evaluatesTo` HiValueNumber 100

  it "complex operations" $ do
    "less-than(mul(999, 99), 10000)" `evaluatesTo` HiValueBool False
    "if(greater-than(div(2, 5), div(3, 7)), 1, -1)" `evaluatesTo` HiValueNumber (-1)
    "and(less-than(0, 1), less-than(1, 0))" `evaluatesTo` HiValueBool False


-- | We need to check the following properties:
-- - for all A B, greater-than(A, B) ≡ less-than(B, A) holds
-- - for all A B, not-equals(A, B) ≡ not(equals(A, B)) holds
-- - for all A, B, not-less-than(A, B) ≡ not(less-than(A, B)) holds
-- - for all A, B, not-greater-than(A, B) ≡ not(greater-than(A, B)) holds
--
-- I was too tired to write all the generators for property tests. So this
-- is just a placeholder.
propertyGroup :: Spec
propertyGroup = do
  it "compliments" $ do
    "greater-than(10, 3)" `evaluatesTo` HiValueBool True
    "greater-than(true, false)" `evaluatesTo` HiValueBool True
    "greater-than(0, false)" `evaluatesTo` HiValueBool True
    "not-equals(10, 10)" `evaluatesTo` HiValueBool False
    "not-equals(false, false)" `evaluatesTo` HiValueBool False
    "not-equals(3, 10)" `evaluatesTo` HiValueBool True
    "not-equals(1, true)" `evaluatesTo` HiValueBool True

group :: IO TestTree
group = do
  unit <- testSpec "unit tests" unitSpec
  property <- testSpec "property tests" propertyGroup
  return $ testGroup "T2" [unit, property]
