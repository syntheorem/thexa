module Thexa.RustLexerSpec where

import Test.Hspec

import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as UTF8
import Data.Sequence (Seq, (|>))

import RustLexer.Rules qualified as RL
import Thexa (makeLexer, Position(..), Span(..))

lexer :: RL.Lexer
lexer = $$(makeLexer RL.lexerRules)

-- The point of this test isn't to test if the Rust lexer specifically is correct, but rather to
-- test the specific features of the Thexa that are used.
spec :: Spec
spec = do
  specify "multiple tokens" $ testTokens
    -- various newline types are counted correctly
    [ blankLines "\n \r \r\n" 3

    -- keywords aren't parsed as idents, showing we're respecting ordering
    , token "match" RL.KW_match

    -- keyword with suffix is parsed as ident, showing we use longest match
    , token "match_" (RL.TOK_Ident "match_")

    -- nested block comments, showing the mode stack works
    , blankLines "/* /* */ /* /* */ */ */" 0

    -- string with multi code point grapheme cluster to make sure columns are counted correctly
    , tokenWithCol "\"\x1F3F3\xFE0F\x200D\x1F308\"" (RL.LIT_String "\x1F3F3\xFE0F\x200D\x1F308") 3

    -- raw string to test rule conditions
    , token "r##\" \" \"# \"##" (RL.LIT_String " \" \"# ")

    -- test when notFollowedBy is satisfied
    , token "0." (RL.LIT_Float "0.")
    ]

  -- test when notFollowedBy is not satisfied
  specify "notFollowedBy" $
    "0.x" `shouldLex` [RL.LIT_Int "0", RL.SYM_Dot, RL.TOK_Ident "x"]

shouldLex :: String -> [RL.TokenData] -> Expectation
shouldLex inputStr tds =
  case RL.runLexer lexer inputBS of
    Right tokens -> map (\(RL.Token td _) -> td) tokens `shouldBe` tds
    Left lexErr  -> expectationFailure (RL.displayLexerError lexErr)
  where
    inputBS = UTF8.fromString inputStr

token :: String -> RL.TokenData -> (String, Maybe RL.TokenData, Int, Int)
token s t = (s, Just t, 0, length s)

tokenWithCol :: String -> RL.TokenData -> Int -> (String, Maybe RL.TokenData, Int, Int)
tokenWithCol s t c = (s, Just t, 0, c)

blankLines :: String -> Int -> (String, Maybe RL.TokenData, Int, Int)
blankLines s l = (s, Nothing, l, 0)

testTokens :: [(String, Maybe RL.TokenData, Int, Int)] -> Expectation
testTokens tokInfo =
  case RL.runLexer lexer inputBS of
    Right tokens -> checkTokens tokens (toList expected)
    Left lexErr  -> expectationFailure (RL.displayLexerError lexErr)
  where
    inputBS  = UTF8.fromString inputStr
    inputStr = concatMap (\(s,_,_,_) -> s<>"\n") tokInfo

    expected :: Seq RL.Token
    expected = snd $ foldl' foldGo (Position 0 0 0 0, mempty) tokInfo

    foldGo :: (Position, Seq RL.Token) -> (String, Maybe RL.TokenData, Int, Int) -> (Position, Seq RL.Token)
    foldGo (pos, tokens) (s, Just td, _, cols) = (nextPos, tokens |> RL.Token td (Span pos endPos))
      where
        strLen  = BS.length (UTF8.fromString s)
        endPos  = Position (posLine pos) cols strLen (posFileOffset pos + strLen)
        nextPos = Position (posLine endPos + 1) 0 0 (posFileOffset endPos + 1)

    foldGo (pos, tokens) (s, Nothing, lines, _) = (nextPos, tokens)
      where
        strLen  = BS.length (UTF8.fromString s)
        nextPos = Position (posLine pos + lines + 1) 0 0 (posFileOffset pos + strLen + 1)

    checkTokens :: [RL.Token] -> [RL.Token] -> Expectation
    checkTokens (t0:ts0) (t1:ts1) = do
      t0 `shouldBe` t1
      checkTokens ts0 ts1

    checkTokens [] [] = pure ()
    checkTokens [] ts = expectationFailure $ "expected tokens remaining: "<>show ts
    checkTokens ts [] = expectationFailure $ "unexpected tokens remaining: "<>show ts
