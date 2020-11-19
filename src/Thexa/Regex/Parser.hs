{-# LANGUAGE NoOverloadedLists, NoOverloadedStrings #-}

module Thexa.Regex.Parser
( ParseErrors
, parseRegex
, parseCharSet
, parseErrorsPretty
) where

import PreludePrime

import Data.Char qualified as Char
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

import Thexa.Regex.AST

type Parser = P.Parsec Void String

type ParseErrors = P.ParseErrorBundle String Void

parseRegex :: String -> Either ParseErrors RegexQ
parseRegex = P.parse (regex <* P.eof) ""

parseCharSet :: String -> Either ParseErrors CharSetQ
parseCharSet = P.parse (charSet <* P.eof) ""

-- | Pretty-print the parse errors.
parseErrorsPretty :: ParseErrors -> String
parseErrorsPretty = P.errorBundlePretty

------------------
-- Regex Parser --
------------------

-- | Top-level parser used by the regex quasiquoter.
regex :: Parser RegexQ
regex = whitespace >> P.choice
  [ symbol "|" >> (reAlt REEmpty <$> regex)
  , regexAtomRepeat >>= regexRec
  , pure REEmpty
  ]

-- | Recursive case of the regex parser, where we pass in the prefix of the regex that we've already
-- parsed, and then either form a sequence or alternative with the rest of the regex.
regexRec :: RegexQ -> Parser RegexQ
regexRec re = P.choice
  [ symbol "|" >> (reAlt re <$> regex)
  , (reSeq re <$> regexAtomRepeat) >>= regexRec
  , pure re
  ]

-- | Parses a regex atom possibly followed by a repeater.
regexAtomRepeat :: Parser RegexQ
regexAtomRepeat = do
  re <- regexAtom
  P.choice
    [ symbol "?" $> reOpt re
    , symbol "+" $> rePlus re
    , symbol "*" $> reStar re
    , regexRepeatBraces re
    , pure re
    ]

-- | Parses a repeat expression of the form @{n}@, @{n,}@, or @{n,m}@, where @n@ and @m@ are decimal
-- numbers. Then applies the correct repeat to the provided regex.
regexRepeatBraces :: RegexQ -> Parser RegexQ
regexRepeatBraces re =
  P.between (symbol "{") (symbol "}") do
    n <- bound
    P.choice
      [ symbol "," >> P.choice
          [ bound <&> \m -> reRepeatBounded n m re
          , pure (reRepeatUnbounded n re)
          ]
      , pure (reRepeat n re)
      ]
  where
    bound :: Parser Natural
    bound = lexeme L.decimal

regexAtom :: Parser RegexQ
regexAtom = P.choice
  [ RECharSet <$> (charSetBrackets <|> charSetSplice)
  , P.between (symbol "(") (symbol ")") regex
  , regexSplice
  , regexString
  , regexChar
  ]

regexString :: Parser RegexQ
regexString = do
  q <- P.char '\'' <|> P.char '"'
  s <- P.many (escapedChar <|> P.anySingleBut q)
  _ <- symbol [q]
  pure (reString s)

regexChar :: Parser RegexQ
regexChar = lexeme $
  reChar <$> P.choice
    [ escapedChar
    , P.noneOf "?+*|{}()"
    ]

regexSplice :: Parser RegexQ
regexSplice = lexeme do
  _ <- P.char '%'
  RESplice <$> spliceName

--------------------
-- CharSet Parser --
--------------------

-- | Top-level parser used by the charset quasiquoter.
--
-- Essentially, we parse a charset as though we just saw a @[@, so we handle the possible @^@ which
-- inverts it and then parse a non-empty list of charset atoms.
charSet :: Parser CharSetQ
charSet = whitespace >> P.choice
  [ symbol "^" >> (csInverse <$> charSetInner)
  , charSetInner
  ]

-- | Parse a charset after we've already checked for @^@.
charSetInner :: Parser CharSetQ
charSetInner = do
  cs <- charSetAtom
  charSetRec cs

-- | Recursive case of the charset parser, where we pass in the prefix of the charset parsed so far
-- and then either union or difference it with the rest of the charset.
charSetRec :: CharSetQ -> Parser CharSetQ
charSetRec cs = P.choice
  [ CSDiff cs <$> (symbol "^" >> charSetInner)
  , (csUnion cs <$> charSetAtom) >>= charSetRec
  , pure cs
  ]

charSetAtom :: Parser CharSetQ
charSetAtom = P.choice
  [ charSetBrackets
  , charSetSplice
  , singleCharOrRange
  ]

charSetBrackets :: Parser CharSetQ
charSetBrackets = P.between (symbol "[") (symbol "]") charSet

-- | Parse a single character or character range in a charset.
singleCharOrRange :: Parser CharSetQ
singleCharOrRange = do
  c <- singleChar
  charSetRange c <|> pure (CSSingle c)

charSetRange :: Char -> Parser CharSetQ
charSetRange c1 = do
  _ <- symbol "-"
  c2 <- singleChar
  case compare c1 c2 of
    EQ -> pure (CSSingle c1)
    LT -> pure (CSRange c1 c2)
    GT -> fail "empty character range"

-- | Parse a single character (standalone, quoted, or escaped) in a charset.
singleChar :: Parser Char
singleChar = lexeme $ P.choice
  [ quotedChar
  , escapedChar
  , P.noneOf "^-[]$"
  ]

-- | Parse a character in single or double quotes.
quotedChar :: Parser Char
quotedChar = do
  q <- P.char '\'' <|> P.char '"'
  c <- escapedChar <|> P.anySingleBut q
  _ <- P.char q
  pure c

charSetSplice :: Parser CharSetQ
charSetSplice = lexeme do
  _ <- P.char '$'
  CSSplice <$> spliceName

--------------------
-- Helper Parsers --
--------------------

-- | Parse a (possibly qualified) Haskell identifier.
spliceName :: Parser String
spliceName = do
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

-- | Parse an escaped character.
--
-- Does NOT consume whitespace following the escape.
escapedChar :: Parser Char
escapedChar = P.char '\\' >> P.choice
  [ P.char 'x' >> xEscape
  , P.char 'u' >> uEscape
  , P.char 'n' $> '\n'
  , P.char 't' $> '\t'
  , P.char 'r' $> '\r'
  , P.char 'f' $> '\f'
  , P.char 'v' $> '\v'
  , P.char '0' $> '\0'
  , P.anySingle -- any other escaped character maps to itself
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
      unless (code <= 127) $
        fail "ASCII escape code is out of bounds (maximum is \\x7F)"
      pure (Char.chr (fromIntegral code))

    -- Note that we don't consume whitespace after the '}'
    controlCode = P.between (symbol "{") (P.char '}') $ P.choice
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
      , symbol "SP"  $> '\SP'
      , symbol "DEL" $> '\DEL'
      ]

-- | Parse a Unicode escape following a @\u@.
uEscape :: Parser Char
uEscape = P.between (symbol "{") (P.char '}') $ do
  digits <- lexeme (P.many P.hexDigitChar)
  let code = digitsToInt 16 digits
  unless (code <= 0x10FFFF) $
    fail "Unicode escape code is out of bounds (maximum is \\u{10FFFF})"
  pure (Char.chr (fromIntegral code))

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
