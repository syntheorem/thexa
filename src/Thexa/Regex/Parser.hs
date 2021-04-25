{-# LANGUAGE NoOverloadedStrings #-}

-- | See @SYNTAX.md@ for more information about the regex syntax that we're parsing here. Most of
-- the subparsers in this file have a corresponding production in the syntax description.
module Thexa.Regex.Parser
( ParseErrors
, parseRegex
, parseCharSet
, parseErrorsPretty
) where

import PreludePrime

import Data.Char qualified as Char
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Numeric (showHex)

import Text.Megaparsec ((<?>))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

import Thexa.Regex.AST (RegexAST)
import Thexa.Regex.AST qualified as RE
import Thexa.Regex.CharSet.AST (CharSetAST)
import Thexa.Regex.CharSet.AST qualified as CS
import Thexa.Regex.Unicode.Properties qualified as UC

type Parser = P.Parsec Void String

type ParseErrors = P.ParseErrorBundle String Void

parseRegex :: String -> Either ParseErrors RegexAST
parseRegex = P.parse (regex <* P.hidden P.eof) ""

parseCharSet :: String -> Either ParseErrors CharSetAST
parseCharSet = P.parse (charSet <* P.hidden P.eof) ""

-- | Pretty-print the parse errors.
parseErrorsPretty :: ParseErrors -> String
parseErrorsPretty = P.errorBundlePretty

------------------
-- Regex Parser --
------------------

-- | Top-level parser used by the regex quasiquoter.
regex :: Parser RegexAST
regex = do
  whitespace
  re <- RE.concat <$> P.many regexQuantifiedAtom
  P.choice
    [ symbol "|" >> (RE.alt re <$> regex)
    , failIfOneOf "*+?{" "quantifier must follow a non-empty regex"
    , pure re
    ]

-- | Parses a regex atom possibly followed by a quantifier.
regexQuantifiedAtom :: Parser RegexAST
regexQuantifiedAtom = do
  re <- regexAtom
  P.choice
    [ symbol "?" $> RE.opt re
    , symbol "+" $> RE.plus re
    , symbol "*" $> RE.star re
    , regexRepeater re
    , pure re
    ]

-- | Parses a repeat expression of the form @{n}@, @{n,}@, @{,m}@ or @{n,m}@, where @n@ and @m@ are
-- decimal numbers. Then applies the correct repeat to the provided regex.
regexRepeater :: RegexAST -> Parser RegexAST
regexRepeater re = P.between openBrace (symbol "}") $ P.choice
  [ upperBounded
  , P.try exactBounded
  , lowerBounded
  ]
  where
    number :: Parser Natural
    number = lexeme L.decimal

    -- We need to ensure that we fail the parser without consuming input if we see an opening double
    -- brace, since that begins a regex splice rather than repeat braces.
    openBrace = lexeme $ P.try do
      _ <- P.char '{'
      P.notFollowedBy (P.char '{')

    upperBounded = do
      _ <- symbol ","
      m <- number
      pure (RE.repeatBounded 0 m re)

    exactBounded = do
      n <- number
      P.notFollowedBy (P.char ',')
      pure (RE.repeat n re)

    lowerBounded = do
      n <- number
      _ <- symbol ","

      mOffset <- P.getOffset
      P.optional number >>= \case
        Nothing            -> pure (RE.repeatUnbounded n re)
        Just m | m >= n    -> pure (RE.repeatBounded n m re)
        Just m | otherwise -> failOffset mOffset $
          "repeat quantifier upper bound ("<>show m<>") is smaller than lower bound ("<>show n<>")"

regexAtom :: Parser RegexAST
regexAtom = P.choice
  [ P.between (symbol "(") (symbol ")") regex
  , RE.Chars <$> charSetSplice
  , RE.Chars <$> unicodePropEscape
  , regexCharSet
  , regexSplice
  , regexString
  , regexChar
  ]

regexCharSet :: Parser RegexAST
regexCharSet = do
  csOffset <- P.getOffset
  csAST <- P.between (symbol "[") (symbol "]") charSet
  case csAST of
    CS.Chars cs -> case RE.tryChars cs of
      Just re -> pure re
      Nothing -> failOffset csOffset "empty character set in regex will match nothing"
    _ -> pure (RE.Chars csAST)

regexString :: Parser RegexAST
regexString = lexeme do
  _ <- P.char '"'
  s <- P.many stringChar
  _ <- P.char '"'
  pure (RE.string s)
  where
    stringChar = P.choice
      [ charEscape "\""
      , P.anySingleBut '"'
      ]

regexChar :: Parser RegexAST
regexChar = lexeme $
  RE.char <$> P.choice
    [ charEscape "(){}[]*+?|\" "
    , P.noneOf "(){}[]*+?|\"" <?> "non-special character"
    ]

regexSplice :: Parser RegexAST
regexSplice = P.between (symbol "{{") (symbol "}}") do
  RE.Splice <$> haskellVar

--------------------
-- CharSet Parser --
--------------------

-- | Top-level parser used by the charset quasiquoter.
--
-- Essentially, we parse a charset as though we just saw a @[@, so we handle the possible @^@ which
-- inverts it and then parse a non-empty list of charset atoms.
charSet :: Parser CharSetAST
charSet = whitespace >> P.choice
  [ symbol "^" >> (CS.complement <$> charSetAtoms)
  , do
      cs <- charSetAtoms
      P.choice
        [ symbol "^" >> (CS.difference cs <$> charSetAtoms)
        , pure cs
        ]
  ]

-- | Parse zero or more charset atoms.
charSetAtoms :: Parser CharSetAST
charSetAtoms = CS.unions <$> P.many charSetAtom

charSetAtom :: Parser CharSetAST
charSetAtom = P.choice
  [ charSetSplice
  , P.between (symbol "[") (symbol "]") charSet
  , unicodePropEscape
  , charSetCharOrRange
  , failIfOneOf "-" "expected character before range operator"
  ]

-- | Parse a single character or character range in a charset.
charSetCharOrRange :: Parser CharSetAST
charSetCharOrRange = do
  c <- charSetChar
  charSetRange c <|> pure (CS.char c)

charSetRange :: Char -> Parser CharSetAST
charSetRange c1 = do
  _ <- symbol "-"
  c2Offset <- P.getOffset
  c2 <- charSetChar <?> "upper bound of character range"
  case compare c1 c2 of
    EQ -> pure (CS.char c1)
    LT -> pure (CS.range c1 c2)
    GT -> failOffset c2Offset $ "upper bound of range (code point 0x"<>showHex (Char.ord c2) ") "
            <>"is smaller than the lower bound (code point 0x"<>showHex (Char.ord c1) ")"

charSetChar :: Parser Char
charSetChar = lexeme $ P.choice
  [ charEscape "[]^- "
  , P.noneOf "[]^-" <?> "non-special character"
  ]

charSetSplice :: Parser CharSetAST
charSetSplice = P.between (symbol "[:") (symbol ":]") do
  CS.Splice <$> haskellVar

--------------------
-- Escape Parsers --
--------------------

-- | Parse an escaped character.
--
-- Does NOT consume whitespace following the escape.
charEscape :: String -> Parser Char
charEscape otherEscapes = P.label "escaped character" do
  _ <- P.char '\\'
  P.label "valid escape char" $ P.choice
    [ P.char 'x' >> xEscape
    , P.char 'u' >> uEscape
    , P.char 'n' $> '\n'
    , P.char 't' $> '\t'
    , P.char 'r' $> '\r'
    , P.char 'f' $> '\f'
    , P.char 'v' $> '\v'
    , P.char '0' $> '\0'
    , P.oneOf ('\\':otherEscapes)
    ]

-- | Parse an ASCII escape following a @\x@.
--
-- This takes two forms. One form is two hexadecimal digits representing the code point of the ASCII
-- character. The other is an ASCII control code abbreviation enclosed in braces, i.e. @{ESC}@.
xEscape :: Parser Char
xEscape = hexCode <|> controlCode
  where
    hexCode = do
      digits <- P.count 2 P.hexDigitChar
      let code = digitsToInt 16 digits
      pure (Char.chr (fromIntegral code))

    -- Note that we don't consume whitespace after the '}'
    controlCode = P.between (symbol "{") (P.char '}') $
      P.label "ASCII control code abbreviation" $ P.choice
        [ symbol "NUL" $> '\NUL'
        , symbol "SOH" $> '\SOH'
        , symbol "STX" $> '\STX'
        , symbol "ETX" $> '\ETX'
        , symbol "EOT" $> '\EOT'
        , symbol "ENQ" $> '\ENQ'
        , symbol "ACK" $> '\ACK'
        , symbol "BEL" $> '\BEL'
        , symbol "BS"  $> '\BS'
        , symbol "HT"  $> '\HT'
        , symbol "LF"  $> '\LF'
        , symbol "VT"  $> '\VT'
        , symbol "FF"  $> '\FF'
        , symbol "CR"  $> '\CR'
        , symbol "SO"  $> '\SO'
        , symbol "SI"  $> '\SI'
        , symbol "DLE" $> '\DLE'
        , symbol "DC1" $> '\DC1'
        , symbol "DC2" $> '\DC2'
        , symbol "DC3" $> '\DC3'
        , symbol "DC4" $> '\DC4'
        , symbol "NAK" $> '\NAK'
        , symbol "SYN" $> '\SYN'
        , symbol "ETB" $> '\ETB'
        , symbol "CAN" $> '\CAN'
        , symbol "EM"  $> '\EM'
        , symbol "SUB" $> '\SUB'
        , symbol "ESC" $> '\ESC'
        , symbol "FS"  $> '\FS'
        , symbol "GS"  $> '\GS'
        , symbol "RS"  $> '\RS'
        , symbol "US"  $> '\US'
        , symbol "DEL" $> '\DEL'
        ]

-- | Parse a Unicode escape following a @\u@.
uEscape :: Parser Char
uEscape = P.between (symbol "{") (P.char '}') do
  hexNumOffset <- P.getOffset
  _ <- P.optional (P.string "0x")
  code <- lexeme L.hexadecimal

  unless (code <= 0x10FFFF) do
    failOffset hexNumOffset "invalid Unicode code point (maximum is 0x10FFFF)"

  pure (Char.chr (fromInteger code))

-- | Parse a Unicode property escape.
unicodePropEscape :: Parser CharSetAST
unicodePropEscape = P.label "Unicode property escape" do
  _ <- P.string "\\p"
  P.between (symbol "{") (symbol "}") do
    propNameOffset <- P.getOffset
    propName <- lexeme $ P.some $ (P.alphaNumChar <|> P.char '-' <|> P.char '_')

    case Map.lookup propName propMap of
      Just cs -> pure (CS.Chars cs)
      Nothing -> failOffset propNameOffset $
        "unknown Unicode category, script, or block: "<>propName
  where
    propMap :: UC.PropertyMap
    propMap = blocks
      `unionProps` UC.scripts
      `unionProps` UC.generalCategories

    -- As a sanity check, we want to make sure that none of the properties we're combining have the
    -- same name. Since we use the strict map functions, these errors will be evaluated as long as
    -- we evaluate the combined map, so any such errors will be caught by any test that attempts to
    -- parse a '\p' escape.
    unionProps = Map.unionWithKey
      (\k _ _ -> error ("duplicate Unicode property name: "<>k))

    -- The names of the Unicode blocks need to be modified to match the syntax. Specifically, they
    -- must be prefixed with "In" and have space characters replaced with underscores.
    blocks = Map.mapKeysWith
      (\_ _ -> error "duplicate block names")
      (\blkName -> "In"<>replaceWS blkName)
      UC.blocks

    replaceWS = map (\c -> if c == ' ' then '_' else c)

--------------------
-- Helper Parsers --
--------------------

-- | Parse a (possibly qualified) Haskell identifier.
haskellVar :: Parser String
haskellVar = lexeme $ P.label "Haskell variable identifier" $ do
  modid <- fold <$> P.many do
    c0 <- P.upperChar
    cs <- P.many identChar
    _  <- P.char '.'
    pure ((c0 : cs) <> ".")

  c0 <- P.lowerChar
  cs <- P.many identChar
  pure (modid <> (c0 : cs))
  where
    identChar = P.alphaNumChar <|> P.char '_' <|> P.char '\''

-- | Convert a list of digits in the given base to an Integer.
digitsToInt :: Integer -> [Char] -> Integer
digitsToInt base = foldl' go 0 . map (toInteger . Char.digitToInt)
  where go acc digit = acc * base + digit

-- | Parse a value and consume all following whitespace.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitespace

-- | Parse a literal string and consume all following whitespace
symbol :: String -> Parser String
symbol = L.symbol whitespace

-- | Consume all whitespace.
whitespace :: Parser ()
whitespace = L.space P.space1 empty empty

-- | Fail the parse with a custom offset for the error message.
failOffset :: Int -> String -> Parser a
failOffset offset msg = P.parseError (P.FancyError offset (Set.singleton (P.ErrorFail msg)))

-- | If the next char is one of the chars in the first argument, fail after consuming it and using
-- the second argument as the error message. Useful for providing better errors.
failIfOneOf :: String -> String -> Parser a
failIfOneOf chars msg = P.hidden $ do
  offset <- P.getOffset
  _ <- P.oneOf chars
  failOffset offset msg
