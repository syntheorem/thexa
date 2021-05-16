-- This is a fully worked example for using Thexa to create a lexer for a real programming language,
-- Rust. I chose Rust because (a) I wanted to use a real language rather than something contrived,
-- (b) I had recently been working on parsing Rust code so the precise syntax was fresh in my mind,
-- and (c) the lexical structure isn't too complex but has opportunities to show most of the
-- features of Thexa.
--
-- It shouldn't be necessary to be familiar with Rust syntax to understand this example, but just in
-- case, the reference for Rust's lexical structure can be found here:
-- https://doc.rust-lang.org/stable/reference/lexical-structure.html
module Main where

import Data.List qualified as List
import Data.ByteString qualified as BS
import System.IO qualified as IO

-- All of the actual lexer definition is done in RustLexer.Rules, but due to Template Haskell
-- restrictions, the actual splice which calls makeLexer must be in a separate module.
import RustLexer.Rules
import Thexa (makeLexer)

-- This is where the lexer is constructed. The $$() syntax is a typed splice, which executes the
-- contained code at compile time and then replaces the splice with the resulting Haskell code.
-- makeLexer takes a list of lexer rules and turns them into an efficient state machine.
lexer :: Lexer
lexer = $$(makeLexer lexerRules)

-- Our main function simply reads input from stdin, runs it through the lexer, and prints the
-- resulting list of tokens to stdout along with their locations in the input.
main :: IO ()
main = do
  input <- BS.getContents
  case runLexer lexer input of
    Left err -> IO.putStrLn (displayLexerError err)
    Right tokens -> IO.putStrLn $ List.unlines $
      map (\(Token tok span) -> displaySpan span<>"  \t"<>show tok) tokens
