module T3 (
    group
  ) where


import Control.Monad (forM_)
import Test.Hspec (Spec, it)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)

import HI.Base (HiError (..), HiValue (..))
import Util (evaluatesTo, parseFails)

spec :: Spec
spec = do
  it "statement" $ do
    "2 + 2" `evaluatesTo` HiValueNumber 4
    "2 + 2 * 3" `evaluatesTo` HiValueNumber 8
    "(2 + 2) * 3" `evaluatesTo` HiValueNumber 12
    "2 + 2 * 3 == (2 + 2) * 3" `evaluatesTo` HiValueBool False
    "10 == 2*5 && 143 == 11*13" `evaluatesTo` HiValueBool True

  it "additional" $ do
    "1 - 2 < 3 + 4 && 5 * 6 >= 7 / 8" `evaluatesTo` HiValueBool True
    "1 + 2 > 3 * 4 || 5 - 6 <= 7 / 8" `evaluatesTo` HiValueBool True

  it "boolean operators are unchainable" $ do
    let operators = ["<", ">", "<=", ">=", "==", "/="]
    forM_ operators $ \first -> forM_ operators $ \second -> do
      parseFails ("1" ++ first ++ "2" ++ second ++ "3")

group :: IO TestTree
group = testSpec "T3" spec
