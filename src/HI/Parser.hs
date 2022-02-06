module HI.Parser (
    parse
  ) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import qualified Data.ByteString (pack)
import Data.Char (isAlpha, isAlphaNum)
import Data.List (intercalate)
import Data.Scientific (toRealFloat)
import Data.Sequence (fromList)
import Data.Text (pack)
import Data.Void (Void)
import Data.Word (Word8)
import HI.Base (HiAction (..), HiExpr (..), HiFun (..), HiValue (..))
import Text.Megaparsec (MonadParsec (eof, label, notFollowedBy, try), ParseErrorBundle, Parsec,
                        between, choice, empty, many, manyTill, runParser, satisfy, sepBy, sepBy1,
                        sepEndBy, some, (<|>))
import Text.Megaparsec.Char (char, hexDigitChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

-- My oh my, this did help a lot: https://serokell.io/blog/parser-combinators-in-haskell

-- General helpers section

type Parser = Parsec Void String

-- | Stolen from article, but we don't have any comments
skipSpaces :: Parser ()
skipSpaces = L.space space1 empty empty

-- | Parses lexeme and skips spaces afterwards
lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpaces

-- | Parses symbol and skips spaces afterwards
symbol :: String -> Parser String
symbol = L.symbol skipSpaces

-- | General bracket parser: transform parser to parse something between brackets
brackets :: String -> String -> Parser a -> Parser a
brackets open close = between (symbol open) (symbol close)

-- | Transform parser to parse something between round brackets
parenthesis :: Parser a -> Parser a
parenthesis = brackets "(" ")"

-- | Parser of something that is prefix of another something with lower
-- priority (oh my this is worded so well)
op :: String -> String -> Parser String
op n next = (lexeme . try) (string n <* notFollowedBy (symbol next))

-- | Ultimate parser of infix binary expressions
binary' ::
     (Parser (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr)
  -> Parser String
  -> HiFun
  -> Operator Parser HiExpr
binary' ctor p f = ctor (wrapper <$ p)
  where
    wrapper :: HiExpr -> HiExpr -> HiExpr
    wrapper a b = HiExprApply (HiExprValue $ HiValueFunction f) [a, b]

-- | Constructs operator that parses left-associative infix expressions
binaryL :: String -> HiFun -> Operator Parser HiExpr
binaryL name = binary' InfixL (symbol name)

-- | Constructs operator that parses non-associative infix expressions
binaryN :: String -> HiFun -> Operator Parser HiExpr
binaryN name = binary' InfixN (symbol name)

-- | Constructs operator that parses right-associative infix expressions
binaryR ::String -> HiFun -> Operator Parser HiExpr
binaryR name = binary' InfixR (symbol name)


-- HiValue section

-- | Parses boolean value into HiValueBool
pHiValueBool :: Parser HiValue
pHiValueBool = label "boolean" $
  HiValueBool <$> choice [
    True <$ string "true"
  , False <$ string "false"
  ]

-- | Parses Hi function into its representation (aka HiValueFunction)
pHiValueFunction :: Parser HiValue
pHiValueFunction = label "Hi function" $
  HiValueFunction <$> choice [
      HiFunNotLessThan    <$ string "not-less-than"
    , HiFunNotGreaterThan <$ string "not-greater-than"
    , HiFunNotEquals      <$ string "not-equals"
    , HiFunDiv            <$ string "div"
    , HiFunMul            <$ string "mul"
    , HiFunAdd            <$ string "add"
    , HiFunSub            <$ string "sub"
    , HiFunNot            <$ string "not"
    , HiFunAnd            <$ string "and"
    , HiFunOr             <$ string "or"
    , HiFunLessThan       <$ string "less-than"
    , HiFunGreaterThan    <$ string "greater-than"
    , HiFunEquals         <$ string "equals"
    , HiFunIf             <$ string "if"
    , HiFunLength         <$ string "length"
    , HiFunToUpper        <$ string "to-upper"
    , HiFunToLower        <$ string "to-lower"
    , HiFunReverse        <$ string "reverse"
    , HiFunTrim           <$ string "trim"
    , HiFunList           <$ string "list"
    , HiFunRange          <$ string "range"
    , HiFunFold           <$ string "fold"
    , HiFunPackBytes      <$ string "pack-bytes"
    , HiFunUnpackBytes    <$ string "unpack-bytes"
    , HiFunEncodeUtf8     <$ string "encode-utf8"
    , HiFunDecodeUtf8     <$ string "decode-utf8"
    , HiFunZip            <$ string "zip"
    , HiFunUnzip          <$ string "unzip"
    , HiFunSerialise      <$ string "serialise"
    , HiFunDeserialise    <$ string "deserialise"
    , HiFunRead           <$ string "read"
    , HiFunWrite          <$ string "write"
    , HiFunMkDir          <$ string "mkdir"
    , HiFunChDir          <$ string "cd"
    , HiFunParseTime      <$ string "parse-time"
    , HiFunRand           <$ string "rand"
    , HiFunEcho           <$ string "echo"
    , HiFunCount          <$ string "count"
    , HiFunKeys           <$ string "keys"
    , HiFunValues         <$ string "values"
    , HiFunInvert         <$ string "invert"
    ]

-- | Parses number into HiValueNumber
pHiValueNumber :: Parser HiValue
pHiValueNumber = label "number" $
  HiValueNumber . toRational <$> L.signed skipSpaces L.scientific

-- | Parses null value into HiValueNull
pHiValueNull :: Parser HiValue
pHiValueNull = label "null" $
  HiValueNull <$ string "null"

-- | Parses string literal into HiValueString
-- note: cannot use 'between' here because it's greedy (I shot my leg here)
pHiValueString :: Parser HiValue
pHiValueString = label "string literal" $
  HiValueString . pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

-- | Parses byte array into HiValueBytes
pHiValueBytes :: Parser HiValue
pHiValueBytes = label "bytes" $ do
  parsed <- brackets "[#" "#]" (pWord8 `sepEndBy` space1)
  return $ HiValueBytes (Data.ByteString.pack parsed)
    where
      pWord8 :: Parser Word8
      pWord8 = label "byte" $ do
        f <- hexDigitChar
        s <- hexDigitChar
        return $ read $ "0x" ++ [f, s]

-- | Parses pure (0-arity) actions into HiValueAction
pHiValueAction :: Parser HiValue
pHiValueAction = label "action" $
  HiValueAction <$> choice [
    HiActionCwd <$ string "cwd"
  , HiActionNow <$ string "now"
  ]

-- | Parses arbitrary HiValue
pHiValue :: Parser HiValue
pHiValue = label "hi value" $
  choice [
    pHiValueBool
  , pHiValueFunction
  , pHiValueNumber
  , pHiValueNull
  , pHiValueString
  , pHiValueBytes
  , pHiValueAction
  ]

-- | Parses list of values into list (aka application of HiFunList to all)
pList :: Parser HiExpr
pList = label "list" $ do
  parsed <- brackets "[" "]" (pInfix `sepBy` symbol ",")
  return $ HiExprApply (HiExprValue (HiValueFunction HiFunList)) parsed

-- | Parses hi value as expression
pHiExprValue :: Parser HiExpr
pHiExprValue = label "hi expr value" $
  parenthesis pHiExprValue <|> (HiExprValue <$> lexeme pHiValue <|> pList <|> pDictionary)

-- | Parses dictionary item (aka pair)
pDictionaryItem :: Parser (HiExpr, HiExpr)
pDictionaryItem = label "dictionary item" $
  (,) <$> (pInfix <* symbol ":") <*> pInfix

-- | Parses dictionary
pDictionary :: Parser HiExpr
pDictionary = label "dictionary" $
  HiExprDict <$> brackets "{" "}" (pDictionaryItem `sepBy` symbol ",")

-- Here the fun part begins
-- Some explanation - I consider '!', '.dot-access' and '("arg list")' to have
-- the same priority and therefore parse them as 'many' after expression.
-- To build an expression after that I parse every argument-like postfix
-- as (HiExpr -> HiExpt) which allows me to construct resulting HiExpr
-- with fold.

-- | Parses run symbol, aka '!'
pAction :: Parser (HiExpr -> HiExpr)
pAction = label "hi expr run" $
  HiExprRun <$ symbol "!"

-- | Parses dot access (.dot-access -> ("dot-access"))
pDotAccess :: Parser (HiExpr -> HiExpr)
pDotAccess = lexeme $ do
  _ <- lexeme $ char '.'
  -- Thanks for this expression
  text <- ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'
  return $ \expr ->
    HiExprApply expr [HiExprValue $ HiValueString (pack $ intercalate "-" text)]

-- | Parses usual arglist
pArguments :: Parser (HiExpr -> HiExpr)
pArguments = flip HiExprApply <$> parenthesis (pInfix `sepBy` symbol ",")

-- | Parses Hi expression with all of its arguments
pHiExpr :: Parser HiExpr
pHiExpr = label "hi expr" $
  pHiExprInner <|> parenthesis pHiExpr
    where
      pHiExprInner :: Parser HiExpr
      pHiExprInner = do
        main <- pHiExprValue
        rest <- many (pArguments <|> pAction <|> pDotAccess)
        return $ foldl (\acc cur -> cur acc) main rest


-- | Operator table for infix expressions, groupped by priority
operatorTable :: [[Operator Parser HiExpr]]
operatorTable = [
    -- mul and div
    [ binaryL "*" HiFunMul
      -- this is needed becuase of existence of '/='
    , binary' InfixL (op "/" "=") HiFunDiv ]

    -- add and sub
  , [ binaryL "+" HiFunAdd
    , binaryL "-" HiFunSub ]

    -- comparisons
  , [ binaryN "<=" HiFunNotGreaterThan
    , binaryN ">=" HiFunNotLessThan
    , binaryN "<" HiFunLessThan
    , binaryN ">" HiFunGreaterThan
    , binaryN "==" HiFunEquals
    , binaryN "/=" HiFunNotEquals ]

    -- boolean ops (and is higher than or)
  , [ binaryR "&&" HiFunAnd ]
  , [ binaryR "||" HiFunOr ]
  ]

-- | Parses infix expression
pInfix :: Parser HiExpr
pInfix = makeExprParser (try pHiExpr <|> parenthesis pInfix) operatorTable

-- | Parse arbitrary expression in Hi language or return ParseErrorBundle
parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (between skipSpaces eof pInfix) ""
