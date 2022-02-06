{-# LANGUAGE OverloadedStrings #-}

module T6 (
    group
  ) where

import qualified Data.ByteString as BS (pack)
import Data.Sequence (fromList)
import qualified Data.Text as T (pack)
import Test.Hspec (Spec, it)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (testSpec)

import Control.Monad (forM_)
import HI.Base (HiError (HiErrorInvalidArgument), HiFun (HiFunAdd), HiValue (..))
import Text.Printf (printf)
import Util (evaluatesTo, failsWith, parseFails, prints)

unitSpec :: Spec
unitSpec = do
  it "new operations" $ do
    "pack-bytes([ 3, 255, 158, 32 ])" `evaluatesTo` HiValueBytes (BS.pack
      [0x03, 0xff, 0x9e, 0x20])
    "unpack-bytes([# 10 20 30 #])" `evaluatesTo` HiValueList (fromList [
        HiValueNumber 16,
        HiValueNumber 32,
        HiValueNumber 48
      ])
    "encode-utf8(\"Hello!\")" `evaluatesTo` HiValueBytes (BS.pack
      [0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x21])
    "decode-utf8([# 48 65 6c 6c 6f #])" `evaluatesTo` HiValueString "Hello"
    "decode-utf8([# c3 28 #])" `evaluatesTo` HiValueNull

  it "operator overloads" $ do
    "[# 00 ff #] + [# 01 e3 #]" `evaluatesTo` HiValueBytes (BS.pack
      [0x00, 0xff, 0x01, 0xe3])
    "[# 00 ff #] * 3" `evaluatesTo` HiValueBytes (BS.pack
      [0x00, 0xff, 0x00, 0xff, 0x00, 0xff])

  it "complex examples" $ do
    "pack-bytes(range(30, 40))" `evaluatesTo` HiValueBytes (BS.pack
      [0x1e, 0x1f, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28])
    "decode-utf8([# 68 69 #] * 5)" `evaluatesTo` HiValueString "hihihihihi"
    "unzip([# 78 da 63 64 62 06 00 00 0d 00 07 #])" `evaluatesTo` HiValueBytes (BS.pack
      [0x01, 0x02, 0x03])

  it "Q&A" $ do
    "[# 00 ff #](1)" `evaluatesTo` HiValueNumber 255
    parseFails "[# 0 1 2 3 #]"
    parseFails "[# (1 + 2) #]"
    "pack-bytes([ 256 ])" `failsWith` HiErrorInvalidArgument
    "pack-bytes([ -1 ])" `failsWith` HiErrorInvalidArgument
    "[# #]" `evaluatesTo` HiValueBytes (BS.pack [])
    "pack-bytes([])" `evaluatesTo` HiValueBytes (BS.pack [])

-- | We need to check the following properties:
-- - for all A, unzip(zip(A)) ≡ A holds
-- - for all A, deserialise(serialise(A)) ≡ A holds
--
-- I was too tired to write all the generators for property tests. So this
-- is just a placeholder. Kinda.
propertyGroup :: Spec
propertyGroup = do
  it "unzip-zip - statement" $ do
    "unzip(zip([# 01 02 03 #]))" `prints` "[# 01 02 03 #]"

  it "unzip-zip - cycled" $ forM_ [0..255] $ \byte -> do
    let repr = printf "%02x" (byte :: Int) :: String
    ("unzip(zip([#" ++ repr ++ "#]))") `prints` ("[# " ++ repr ++ " #]")

  it "serialize-deserialize" $ do
    "deserialise(serialise([# 01 02 03 #]))" `prints` "[# 01 02 03 #]"

  it "serialize-deserialize - cycled" $ forM_ [0..255] $ \byte -> do
    let repr = printf "%02x" (byte :: Int) :: String
    ("deserialise(serialise([#" ++ repr ++ "#]))") `prints` ("[# " ++ repr ++ " #]")



group :: IO TestTree
group = do
  unit <- testSpec "unit tests" unitSpec
  property <- testSpec "property tests" propertyGroup
  return $ testGroup "T6" [unit, property]
