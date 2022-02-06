module T7 (
    group
  ) where

import Control.Monad (forM_)
import Data.Foldable (Foldable (toList))
import Data.List (null, (\\))
import Data.Maybe (fromJust, isNothing)
import Data.Sequence (fromList)
import Data.Set (empty)
import Data.Text (pack)
import Data.Traversable (forM)
import System.Directory (createDirectory, doesDirectoryExist, getCurrentDirectory,
                         removeDirectoryRecursive, removeFile, setCurrentDirectory)
import Test.Hspec (Spec, anyException, describe, expectationFailure, it, pending, pendingWith,
                   shouldBe, shouldReturn, shouldThrow)
import Test.Hspec.Core.Hooks (after_, before)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (testSpec)

import HI.Action (HiPermission (..))
import HI.Base (HiError (..), HiValue (..))
import Util (evaluatesTo, failsWith, isPermissionException, permissions, prints,
             tryEvaluateWithPermissions)

-- Fun fact about this tests - because of it I removed '-threaded' flag
-- because for some reason (completely unknown reason) it flacked otherwise
spec :: Spec
spec = do
  it "cwd" $ do
    let dirName = "tmp_hi_cwd_check"

    dir <- getCurrentDirectory
    "cwd!" `evaluatesTo` HiValueString (pack dir)

    createDirectory dirName
    setCurrentDirectory dirName
    newDir <- getCurrentDirectory
    "cwd!" `evaluatesTo` HiValueString (pack newDir)

    setCurrentDirectory dir
    "cwd!" `evaluatesTo` HiValueString (pack dir)

    removeDirectoryRecursive dirName

  it "cd" $ do
    let dirName = "tmp_hi_cd_check"

    dir <- getCurrentDirectory
    createDirectory dirName

    ("cd(\"" ++ dirName ++ "\")!") `evaluatesTo` HiValueNull
    getCurrentDirectory `shouldReturn` (dir ++ "/" ++ dirName)

    "cd(\"..\")!" `evaluatesTo` HiValueNull
    getCurrentDirectory `shouldReturn` dir

    removeDirectoryRecursive dirName

  it "read (file)" $ do
    let fileName = "tmp_hi_read_check"
    let content = "Hello From Hi Language Tests"
    writeFile fileName content

    ("read(\"" ++ fileName ++ "\")!") `evaluatesTo` HiValueString (pack content)

    removeFile fileName

  it "read (dir) - one file" $ do
    let dirName = "tmp_hi_read_check"

    let fileName = "hi_check_file"
    let content = "Hello Hi Dir Lister"

    createDirectory dirName
    writeFile (dirName ++ "/" ++ fileName) content

    ("read(\"" ++ dirName ++ "\")!") `evaluatesTo`
      HiValueList (fromList [HiValueString (pack fileName)])

    removeDirectoryRecursive dirName

  it "read (dir) - many files" $ do
    let dirName = "tmp_hi_read_check"

    let fileNames = map (\ind -> "file_" ++ show ind) [1..100]
    let content = "Hello Hi Dir Lister"

    createDirectory dirName
    forM_ fileNames (\fileName -> writeFile (dirName ++ "/" ++ fileName) content)

    result <- tryEvaluateWithPermissions
      (permissions "r")
      ("read(\"" ++ dirName ++ "\")!")

    let unpacked = unpackList result

    shouldBe (isNothing unpacked) False
    shouldBe (listsEqual
      (map (HiValueString . pack) fileNames)
      (fromJust unpacked))
      True

    removeDirectoryRecursive dirName

  -- This test checks case when neither file, nor folder exist. In such case
  -- we should return null.
  it "read (error)" $ do
    let dirName = "tmp_hi_read_check"

    ("read(\"" ++ dirName ++ "\")!") `evaluatesTo` HiValueNull

  it "write - string" $ do
    let fileName = "tmp_hi_write_check"
    let content = "Hello Hi File Writer"

    ("write(\"" ++ fileName ++ "\", \"" ++ content ++ "\")!") `evaluatesTo`
      HiValueNull

    readFile fileName `shouldReturn` content

    removeFile fileName

-- Apparently, we MUST NOT accept anything except string in
-- write second argument so this test is commented out
--
--  it "write - bytes" $ do
--    let fileName = "tmp_hi_write_check"
--    let content = "hihihi"
--
--    ("write(\"" ++ fileName ++ "\", [# 68 69 68 69 68 69 #])!") `evaluatesTo`
--      HiValueNull

--    readFile fileName `shouldReturn` content

--    removeFile fileName

  it "mkdir" $ do
    let dirName = "tmp_hi_mkdir_check"

    ("mkdir(\"" ++ dirName ++ "\")!") `evaluatesTo` HiValueNull

    doesDirectoryExist dirName `shouldReturn` True

    removeDirectoryRecursive dirName

  it "complex test from statement" $ do
    let dirName = "tmp_hi_complex_test"

    createDirectory dirName
    setCurrentDirectory dirName

    "mkdir(\"tmp\")!" `evaluatesTo` HiValueNull

    "read(\"tmp\")!" `evaluatesTo` HiValueList (fromList [])

    "mkdir(\"tmp/a\")!" `evaluatesTo` HiValueNull

    "mkdir(\"tmp/b\")!" `evaluatesTo` HiValueNull

    result <- tryEvaluateWithPermissions
      (permissions "r")
      "read(\"tmp\")!"

    let unpacked = unpackList result

    shouldBe (isNothing unpacked) False
    shouldBe (listsEqual
      [HiValueString (pack "a"), HiValueString (pack "b")]
      (fromJust unpacked))
      True


    "write(\"tmp/hi.txt\", \"Hello\")!" `evaluatesTo` HiValueNull

    "cd(\"tmp\")!" `evaluatesTo` HiValueNull

    "read(\"hi.txt\")!" `evaluatesTo` HiValueString (pack "Hello")

    setCurrentDirectory "../.."
    removeDirectoryRecursive dirName

  it "correct prints of unrunned actions" $ do
    "cwd" `prints` "cwd"
    "cd(\"to\" * 2)" `prints` "cd(\"toto\")"
    "read(\"to\" * 2)" `prints` "read(\"toto\")"
    "write(\"to\" + \".txt\", \"i\" * 2)" `prints` "write(\"to.txt\", [# 69 69 #])"
    "mkdir(\"to\" * 2)" `prints` "mkdir(\"toto\")"

  it "unauthorized actions" $ do
    shouldThrow (tryEvaluateWithPermissions empty "read(\"a\")!") (isPermissionException AllowRead)
    shouldThrow (tryEvaluateWithPermissions empty "cwd!") (isPermissionException AllowRead)
    shouldThrow (tryEvaluateWithPermissions empty "cd(\"a\")!") (isPermissionException AllowRead)
    shouldThrow (tryEvaluateWithPermissions empty "write(\"a\", \"b\")!") (isPermissionException AllowWrite)
    shouldThrow (tryEvaluateWithPermissions empty "mkdir(\"a\")!") (isPermissionException AllowWrite)

    where
        unpackList :: Maybe (Either HiError HiValue) -> Maybe [HiValue]
        unpackList (Just (Right (HiValueList values))) = Just $ toList values
        unpackList _                                   = Nothing

        listsEqual :: (Eq a) => [a] -> [a] -> Bool
        listsEqual x y = null (x \\ y) && null (y \\ x)


group :: IO TestTree
group = testSpec "T7" spec
