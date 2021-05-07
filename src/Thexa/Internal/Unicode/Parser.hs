-- | Functions to parse the Unicode data files in the @unicode@ directory.
module Thexa.Internal.Unicode.Parser
( readUnicodeDataFile
, readGraphemeBreakTest
) where

import PreludePrime

import Data.Char qualified as Char
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

import Data.FileEmbed (makeRelativeToProject)
import Language.Haskell.TH (Q, TExp)
import System.IO (FilePath, readFile)

import Thexa.CharSet (CharSet)
import Thexa.CharSet qualified as CS

readUnicodeDataFile :: FilePath -> Q (TExp (Map String CharSet))
readUnicodeDataFile file = do
  file' <- makeRelativeToProject file
  contents <- liftIO $ readFile file'

  propMap <- case P.parse ucdFile file contents of
    Left errs -> fail ("unable to parse Unicode data file\n"<>P.errorBundlePretty errs)
    Right charProps -> pure (Map.fromListWith CS.union charProps)

  let propMapList = Map.toAscList propMap
  [|| Map.fromDistinctAscList propMapList ||]

readGraphemeBreakTest :: Q (TExp [[String]])
readGraphemeBreakTest = do
  let file = "unicode/GraphemeBreakTest.txt"
  file' <- makeRelativeToProject file
  contents <- liftIO $ readFile file'

  result <- case P.parse gbtFile file contents of
    Left errs -> fail ("unable to parse Unicode data file\n"<>P.errorBundlePretty errs)
    Right res -> pure res

  [|| result ||]

type Parser = P.Parsec Void String

ucdFile :: Parser [(String, CharSet)]
ucdFile = do
  skipEmptyLines
  P.many (ucdEntry <* skipEmptyLines) <* P.eof

gbtFile :: Parser [[String]]
gbtFile = do
  skipEmptyLines
  P.many (gbtEntry <* skipEmptyLines) <* P.eof

gbtEntry :: Parser [String]
gbtEntry = do
  break -- every entry starts with a break
  P.manyTill grapheme lineEnd
  where
    grapheme = do
      c <- codePoint
      P.choice
        [ break >> pure [c]
        , noBreak >> (c:) <$> grapheme
        ]

    break = P.char '÷' >> P.hspace
    noBreak = P.char '×' >> P.hspace
    codePoint = ucdCodePoint <* P.hspace
    lineEnd = L.skipLineComment "#" <|> void P.eol

skipEmptyLines :: Parser ()
skipEmptyLines = P.skipMany do
  L.space P.hspace1 (L.skipLineComment "#") empty
  P.eol

ucdEntry :: Parser (String, CharSet)
ucdEntry = do
  c1 <- ucdCodePoint
  cs <- P.choice
    [ P.string ".." >> (CS.range c1 <$> ucdCodePoint)
    , pure (CS.singleton c1)
    ]

  P.hspace
  _ <- P.char ';'
  P.hspace

  propName <- P.some (P.noneOf ("#\n" :: String))
  pure (trimEnd propName, cs)

ucdCodePoint :: Parser Char
ucdCodePoint = do
  code <- L.hexadecimal
  unless (code <= 0x10FFFF) $
    fail "code point out of bounds"
  pure (Char.chr (fromInteger code))

trimEnd :: String -> String
trimEnd = List.dropWhileEnd Char.isSpace
