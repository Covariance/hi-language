{-# LANGUAGE LambdaCase #-}
module Util (
    permissions
  , isPermissionException
  , tryParse
  , tryEvaluateWithPermissions
  , tryPrettyPrintWithPermsissions
  , parsesTo
  , parseFails
  , evaluatesToP
  , evaluatesTo
  , failsWithP
  , failsWith
  , printsP
  , prints
  ) where

import Data.List (isInfixOf)
import Data.Set (Set)
import qualified Data.Set as Set
import HI.Action (HIO (runHIO), HiPermission (..), PermissionException (..))
import HI.Base (HiError, HiExpr, HiValue)
import HI.Evaluator (eval)
import HI.Parser (parse)
import HI.Pretty (prettyValue)
import System.Directory.Internal.Prelude (isPermissionError)
import Test.Hspec (Expectation, shouldBe, shouldReturn)

-- Helpers section

-- | Gives permission set based on string representation
-- 'r' for Read, 'w' for Write and 't' for Time
permissions :: String -> Set HiPermission
permissions perms = do
    let r = givePermIfContains "r" AllowRead
    let w = givePermIfContains "w" AllowWrite
    let t = givePermIfContains "t" AllowTime
    Set.unions [r, w, t]
      where
        givePermIfContains :: String -> HiPermission -> Set HiPermission
        givePermIfContains name perm =
          if name `isInfixOf` perms
            then Set.singleton perm
            else Set.empty


-- | Checks whether the PermissionException is of the given type
isPermissionException :: HiPermission -> PermissionException -> Bool
isPermissionException expected (PermissionRequired actual) = expected == actual


-- | Tries to parse input
--
-- As errors are not produced by our code, but by Megaparsec,
-- we do not actually care about the error message, we only check
-- whether there was an error.
tryParse :: String -> Maybe HiExpr
tryParse input = case parse input of
  Left err   -> Nothing
  Right expr -> Just expr


-- | Tries to evaluate given expression with permissions (impure)
tryEvaluateWithPermissions :: Set HiPermission -> String -> IO (Maybe (Either HiError HiValue))
tryEvaluateWithPermissions permissions input = case tryParse input of
  Nothing   -> return Nothing
  Just expr -> do
    result <- runHIO (eval expr) permissions
    return $ Just result


-- | Tries to evaluate given expression with permissions and pretty print it (impure)
tryPrettyPrintWithPermsissions :: Set HiPermission -> String -> IO (Maybe String)
tryPrettyPrintWithPermsissions permissions input = do
  evaluated <- tryEvaluateWithPermissions permissions input
  return (case evaluated of
    Nothing            -> Nothing
    Just (Left error)  -> Just $ show error
    Just (Right value) -> Just $ show $ prettyValue value
    )


-- Test functions section

-- | Expects the parsing result to match expression:
-- Ex:
-- "1" `parsesTo` HiExprNumber (HiValueNumber 1)
parsesTo :: String -> HiExpr -> Expectation
parsesTo input output = do
  tryParse input `shouldBe` Just output


-- | Expects that parser fails:
-- Ex:
-- parseFails "(1"
parseFails :: String -> Expectation
parseFails input = do
  tryParse input `shouldBe` Nothing


-- | Expects the evaluation result with permissions
evaluatesToP :: String -> String -> HiValue -> Expectation
evaluatesToP perms input output = do
  tryEvaluateWithPermissions (permissions perms) input `shouldReturn` Just (Right output)

-- | Default value of evaluatesToP with all permissions
-- Ex:
-- "1" `evaluatesTo` HiValueNumber 1
evaluatesTo :: String -> HiValue -> Expectation
evaluatesTo = evaluatesToP "rwt"


-- | Expects the evaluation with permissions to fail with certain error
failsWithP :: String -> String -> HiError -> Expectation
failsWithP perms input error = do
  tryEvaluateWithPermissions (permissions perms) input `shouldReturn` Just (Left error)


-- | Default value of failsWithP with all permissions
-- Ex:
-- "add(1, 2, 3)" `failsWith` HiErrorArityMismatch
failsWith :: String -> HiError -> Expectation
failsWith = failsWithP "rwt"


-- | Expects the pretty print of evaluation result with permissions
printsP :: String -> String -> String -> Expectation
printsP perms input output = do
  tryPrettyPrintWithPermsissions (permissions perms) input `shouldReturn` Just output


-- | Default value of printsP with all permissions
-- Ex:
-- "div(10, 3)" `prints` "3 + 1/3"
prints :: String -> String -> Expectation
prints = printsP "rwt"
