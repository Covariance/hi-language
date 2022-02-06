{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module HI.Pretty where

import Data.ByteString (unpack)
import Data.Foldable (Foldable (toList))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ratio (denominator, numerator)
import Data.Scientific (FPFormat (Fixed), formatScientific, fromRationalRepetendUnlimited)
import Data.Word (Word8)
import GHC.Exts (IsList)
import HI.Base (HiAction (..), HiValue (..))
import Prettyprinter (Doc, Pretty (pretty), encloseSep, slash, viaShow, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle)
import Text.Printf (printf)

-- | Pretty prints an array of values that are stored in list-like structure
prettyManyValues :: IsList l =>
     (l -> [a])
  -> (a -> Doc ann)
  -> Doc ann
  -> Doc ann
  -> Doc ann
  -> l
  -> Doc ann
prettyManyValues unpacker formatter left right sep values = case values of
  [] -> left <> " " <> right
  seq ->
    let unpacked = unpacker seq
        formatted = map formatter unpacked
    in encloseSep (left <> " ") (" " <> right) sep formatted


-- | Pretty prints arbitrary HiValue
prettyValue :: HiValue -> Doc AnsiStyle
prettyValue = \case
  -- Functions
  (HiValueFunction fun) -> viaShow fun
  -- Booleans
  (HiValueBool True) -> "true"
  (HiValueBool False) -> "false"
  -- Null
  HiValueNull -> "null"
  -- Strings
  (HiValueString text) -> viaShow text
  -- Containers
  (HiValueList a) -> prettyManyValues toList prettyValue "[" "]" ", " a
  (HiValueBytes a) -> prettyManyValues unpack
    (\byte -> pretty (printf "%02x" (byte :: Word8) :: String)) "[#" "#]" " " a
  (HiValueDict a) -> prettyManyValues Map.toList
    (\(a, b) -> prettyValue a <> ":" <+> prettyValue b) "{" "}" ", " a
  -- Actions
  (HiValueAction a) -> case a of
    HiActionRead s       -> "read(" <> viaShow s <> ")"
    HiActionWrite s bs   -> "write(" <> viaShow s <> "," <+> prettyValue (HiValueBytes bs) <> ")"
    HiActionMkDir s      -> "mkdir(" <> viaShow s <> ")"
    HiActionChDir s      -> "cd(" <> viaShow s <> ")"
    HiActionCwd          -> "cwd"
    HiActionNow          -> "now"
    HiActionRand from to -> "rand(" <> pretty from <> "," <+> pretty to <> ")"
    HiActionEcho str     -> "echo(" <> viaShow str <> ")"
  -- Time
  (HiValueTime time) -> "parse-time(\"" <> viaShow time <> "\")"
  -- Numbers
  (HiValueNumber x) -> let
    num = numerator x
    den = denominator x in case fromRationalRepetendUnlimited x of
      (y, Nothing) -> pretty $
        if den == 1
          then show num
          else formatScientific Fixed Nothing y
      (_, Just _)  ->
        let frac = pretty (rem (abs num) den) <> slash <> pretty den in
        if quot num den /= 0
          then pretty (quot num den) <+> (if x > 0 then "+" else "-") <+> frac
          else (if x > 0 then "" else "-") <> frac
