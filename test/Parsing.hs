module Parsing (
    group
  ) where

import qualified Data.ByteString as BS (pack)
import Data.Sequence (fromList)
import Test.Hspec (Spec, it)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)

import HI.Base (HiError (..), HiExpr (..), HiFun (..), HiValue (..))
import Util (evaluatesTo, parseFails, parsesTo)

spec :: Spec
spec = do
  it "simple infix operators" $ do
    "1 / 3" `parsesTo` HiExprApply (HiExprValue $ HiValueFunction HiFunDiv)
      [HiExprValue $ HiValueNumber 1, HiExprValue $ HiValueNumber 3]
    "1 /= 3" `parsesTo` HiExprApply (HiExprValue $ HiValueFunction HiFunNotEquals)
      [HiExprValue $ HiValueNumber 1, HiExprValue $ HiValueNumber 3]
    "1 - -3" `parsesTo` HiExprApply (HiExprValue $ HiValueFunction HiFunSub)
      [HiExprValue $ HiValueNumber 1, HiExprValue $ HiValueNumber (-3)]

  it "brackets everywhere" $ do
    "(add)(((1)), ((3)))" `parsesTo` HiExprApply (HiExprValue $ HiValueFunction HiFunAdd)
      [HiExprValue $ HiValueNumber 1, HiExprValue $ HiValueNumber 3]
    "(((add)))" `parsesTo` HiExprValue (HiValueFunction HiFunAdd)
    parseFails "-(10)" -- No unary minus for you!
    parseFails "add((1, 1))" -- So strange...

  it "spaces everywhere" $ do -- Well, almost everywhere
    "[  \n \t 0  \t\t\n\t ]" `parsesTo` HiExprApply (HiExprValue $ HiValueFunction HiFunList)
      [HiExprValue $ HiValueNumber 0]
    "   ( \n \t add )(\t   0, \n 0  ) " `parsesTo` HiExprApply (HiExprValue $ HiValueFunction HiFunAdd)
      [HiExprValue $ HiValueNumber 0, HiExprValue $ HiValueNumber 0]

  it "it is hard to parse lists, isn't it?" $ do
    "[#\t\n 00 \t\n#]" `evaluatesTo` HiValueBytes (BS.pack [0x00])
    "[#00 \t\n#]" `evaluatesTo` HiValueBytes (BS.pack [0x00])
    "[# \t\n 00#]" `evaluatesTo` HiValueBytes (BS.pack [0x00])
    "[#00#]" `evaluatesTo` HiValueBytes (BS.pack [0x00])
    "[#00 ff#]" `evaluatesTo` HiValueBytes (BS.pack [0x00, 0xff])

    parseFails "[ # 00 # ]"

    "[\t\n 0 \t\n]" `evaluatesTo` HiValueList (fromList [HiValueNumber 0])
    "[0 \t\n]" `evaluatesTo` HiValueList (fromList [HiValueNumber 0])
    "[ \t\n 0]" `evaluatesTo` HiValueList (fromList [HiValueNumber 0])
    "[0]" `evaluatesTo` HiValueList (fromList [HiValueNumber 0])
    "[0 \t, \t \n1]" `evaluatesTo` HiValueList
      (fromList [HiValueNumber 0, HiValueNumber 1])

  it "do you know how to parse numbers? because i don't" $ do
    "+42" `parsesTo` HiExprValue (HiValueNumber 42)
    "-42" `parsesTo` HiExprValue (HiValueNumber (-42))
    "-  42" `parsesTo` HiExprValue (HiValueNumber (-42))

    parseFails "- 4 2"
    parseFails "4 2"

group :: IO TestTree
group = testSpec "Parsing" spec
