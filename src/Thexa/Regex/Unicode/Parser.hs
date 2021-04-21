module Thexa.Regex.Unicode.Parser where

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

import Thexa.Regex.CharSet (CharSet)
import Thexa.Regex.CharSet qualified as CS

readUnicodeDataFile :: FilePath -> Q (TExp (Map String CharSet))
readUnicodeDataFile file = do
  file' <- makeRelativeToProject file
  contents <- liftIO $ readFile file'

  propMap <- case P.parse ucdFile file contents of
    Left errs -> fail ("unable to parse Unicode data file\n"<>P.errorBundlePretty errs)
    Right charProps -> pure (Map.fromListWith CS.union charProps)

  let propMapList = Map.toAscList propMap
  [|| Map.fromDistinctAscList propMapList ||]

type Parser = P.Parsec Void String

ucdFile :: Parser [(String, CharSet)]
ucdFile = do
  skipEmptyLines
  P.many (ucdEntry <* skipEmptyLines) <* P.eof
  where
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
