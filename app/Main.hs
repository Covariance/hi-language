{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Exception (catch)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Monoid (Any)
import Data.Set (Set, fromList)
import Data.Void (Void)
import HI.Action (HIO (runHIO), HiPermission (..), PermissionException)
import HI.Base (HiError, HiExpr, HiValue)
import HI.Evaluator (eval)
import HI.Parser (parse)
import HI.Pretty (prettyValue)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import System.Console.Haskeline (InputT, defaultSettings, getExternalPrint, getInputLine,
                                 outputStrLn, runInputT)
import Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)

-- | All the permissions for REPL
fullPermissions :: Set HiPermission
fullPermissions = fromList [
     AllowRead
  ,  AllowWrite
  ,  AllowTime
  ]


-- | Parses and possibly evaluates given string with given set of permissions
processLine ::
     Set HiPermission
  -> String
  -> IO (Either (ParseErrorBundle String Void) (Either HiError HiValue))
processLine permissions str = do
  let parsed = parse str
  mapM (\expr -> runHIO (eval expr) permissions) parsed


-- | Pretty-prints result of line processing
showResult ::
     IO (Either (ParseErrorBundle String Void) (Either HiError HiValue))
  -> IO String
showResult parsed = do
  either <- parsed
  case either of
    Left errorBundle -> return $ errorBundlePretty errorBundle
    Right expression -> case expression of
      Left error  -> return $ show error
      Right value -> return $ renderString $
        layoutPretty defaultLayoutOptions (prettyValue value)


-- | Starts repl with specified prompt and input handler
repl :: String -> ((String -> IO ()) -> String -> IO ()) -> InputT IO()
repl prompt handler = replLoop
  where
    replLoop = do
      input <- getInputLine prompt
      case input of
        Nothing -> return ()
        Just input -> do
          printer <- getExternalPrint
          lift $ handler printer input
          replLoop


main :: IO ()
main = runInputT defaultSettings $
  repl "hi>" handler
    where
      handler :: (String -> IO ()) -> String -> IO ()
      handler printer input =
        catch @PermissionException
          ((showResult . processLine fullPermissions) input >>= printer)
          print
