module Main (
    main
  ) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)

import qualified Parsing (group)
import qualified Print (group)
import qualified T1 (group)
import qualified T10 (group)
import qualified T11 (group)
import qualified T2 (group)
import qualified T3 (group)
import qualified T4 (group)
import qualified T5 (group)
import qualified T6 (group)
import qualified T7 (group)
import qualified T8 (group)
import qualified T9 (group)

main :: IO ()
main = do
  t1 <- T1.group
  t2 <- T2.group
  t3 <- T3.group
  t4 <- T4.group
  t5 <- T5.group
  t6 <- T6.group
  t7 <- T7.group
  t8 <- T8.group
  t9 <- T9.group
  t10 <- T10.group
  t11 <- T11.group
  parsing <- Parsing.group
  print <- Print.group
  defaultMain $ testGroup "Tests"
    [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, parsing, print]
