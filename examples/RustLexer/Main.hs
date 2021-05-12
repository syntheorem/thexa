module Main where

import PreludePrime

import Data.List qualified as List
import Data.ByteString qualified as BS
import System.IO qualified as IO

import RustLexer.Rules
import Thexa (makeLexer)

lexer :: Lexer
lexer = $$(makeLexer lexerRules)

main :: IO ()
main = do
  input <- BS.getContents
  case runLexer lexer input of
    Left err -> IO.putStrLn (displayLexerError err)
    Right tokens -> IO.putStrLn $ List.unlines $
      map (\(Token tok span) -> displaySpan span<>"  \t"<>show tok) tokens
