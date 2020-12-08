module Thexa.Regex.ParserSpec where

import PreludePrime

import Test.Hspec
import Test.Hspec.Megaparsec

import Thexa.Regex.AST (RegexAST)
import Thexa.Regex.AST qualified as RE
import Thexa.Regex.CharSet.AST (CharSetAST)
import Thexa.Regex.CharSet.AST qualified as CS
import Thexa.Regex.Parser

shouldParseCS :: String -> CharSetAST -> Expectation
shouldParseCS = shouldParse . parseCharSet

shouldFailCS :: String -> Expectation
shouldFailCS = shouldFailOn parseCharSet

shouldParseRE :: String -> RegexAST -> Expectation
shouldParseRE = shouldParse . parseRegex

shouldFailRE :: String -> Expectation
shouldFailRE = shouldFailOn parseRegex

spec :: Spec
spec = do
  describe "parseCharSet" do
    specify "1 char" $
      "a" `shouldParseCS` "a"

    specify "2 char" $
      "ab" `shouldParseCS` "ab"

    specify "2 char with space" $
      " \t a \nb  " `shouldParseCS` "ab"

    specify "inverted" $
      "^ab" `shouldParseCS` CS.complement "ab"

    specify "difference" $
      "ab ^ c" `shouldParseCS` CS.difference "ab" "c"

    specify "chained difference" $
      "ab ^ c ^ def" `shouldParseCS` CS.difference "ab" (CS.difference "c" "def")

    specify "bad invert"     $ shouldFailCS "^"
    specify "bad difference" $ shouldFailCS "a^"

    specify "nested" $
      "[ab[c-z][^x[A-Z]]]" `shouldParseCS`
        CS.unions ["ab", CS.range 'c' 'z', CS.complement (CS.union "x" (CS.range 'A' 'Z'))]

    describe "escaped chars" do
      specify "basic" $
        "\\n\\t\\r\\f\\v\\0" `shouldParseCS` "\n\t\r\f\v\0"

      specify "special" $
        "\\-\\^\\[\\]\\\\\\\"\\'\\ " `shouldParseCS` "-^[]\\\"' "

      specify "ASCII numeric" $
        "\\x21 \\x30" `shouldParseCS` "!0"

      specify "ASCII numeric out-of-bounds" $
        shouldFailCS "\\x80"

      specify "ASCII control codes" $
        "\\x{SOH}\\x{DEL}\\x{SO}\\x{NUL}" `shouldParseCS` "\SOH\DEL\SO\0"

      specify "Unicode" $
        "\\u{0}\\u{394}\\u{0003B5}\\u{03bc}" `shouldParseCS` "\0Δεμ"

      specify "Unicode out-of-bounds" $
        shouldFailCS "\\u{110000}"

      specify "Unicode empty" $
        shouldFailCS "\\u{}"

    describe "ranges" do
      specify "basic" $
        "a-z" `shouldParseCS` CS.range 'a' 'z'

      specify "escaped" $
        "\\x00-\\x20" `shouldParseCS` CS.range '\0' ' '

      specify "quoted" $
        "'a'-'z'" `shouldParseCS` CS.range 'a' 'z'

      specify "multiple" $
        "a-zA-Z" `shouldParseCS` CS.union (CS.range 'a' 'z') (CS.range 'A' 'Z')

      specify "with other chars" $
        "abcA-Zxyz" `shouldParseCS` CS.unions ["abc", CS.range 'A' 'Z', "xyz"]

      specify "invalid bounds" $ shouldFailCS "z-a"
      specify "missing bounds" $ shouldFailCS "-"
      specify "missing left"   $ shouldFailCS "-z"
      specify "missing right"  $ shouldFailCS "a-"

    describe "quoted char" do
      specify "single" $
        "'a''b'  'c' '\x21' ' '" `shouldParseCS` "abc! "

      specify "double" $
        "\"a\"\"b\"  \"c\" \"\x21\" \" \"" `shouldParseCS` "abc! "

    describe "splice" do
      specify "basic" $
        "$foo" `shouldParseCS` CS.Splice "foo"

      specify "qualified" $
        "$Bar.Foo.foo" `shouldParseCS` CS.Splice "Bar.Foo.foo"

      specify "with other chars" do
        "ab$foo.y$bar$x\\$" `shouldParseCS`
          CS.unions ["ab", CS.Splice "foo", ".", "y", CS.Splice "bar", CS.Splice "x", "$"]

      specify "only module" $ shouldFailCS "$Bar.Foo"
      specify "bad name"    $ shouldFailCS "$0"
      specify "only $"      $ shouldFailCS "$"
      specify "with space"  $ shouldFailCS "$ foo"

  describe "parseRegex" do
    specify "string" $
      "ab c \\x20z" `shouldParseRE` "abc z"

    specify "single quoted" $
      "'ab c \\x20+?'" `shouldParseRE` "ab c  +?"

    specify "double quoted" $
      "\"ab c \\x20+?\"" `shouldParseRE` "ab c  +?"

    specify "regex splice" $
      "%foo" `shouldParseRE` RE.Splice "foo"

    specify "charset splice" $
      "$foo" `shouldParseRE` RE.Chars (CS.Splice "foo")

    describe "empty" do
      specify "basic" $
        "" `shouldParseRE` RE.Empty

      specify "parens" $
        "()" `shouldParseRE` RE.Empty

      specify "string" $
        "''" `shouldParseRE` RE.Empty

    describe "repeat" do
      specify "exact" $
        "(abc){5}" `shouldParseRE` RE.repeat 5 "abc"

      specify "unbounded" $
        "a 'bc'{2,}" `shouldParseRE` RE.append "a" (RE.repeatUnbounded 2 "bc")

      specify "bounded" $
        "[a] { 7 , 100 } " `shouldParseRE` RE.repeatBounded 7 100 "a"

    describe "?" do
      specify "with other chars" $
        "abc?de" `shouldParseRE` RE.concat ["ab", RE.opt "c", "de"]

      specify "with parens" $
        "a(bc)?de" `shouldParseRE` RE.concat ["a", RE.opt "bc", "de"]

      specify "bad" $
        shouldFailRE "?abc"

    describe "*" do
      specify "with other chars" $
        "abc*de" `shouldParseRE` RE.concat ["ab", RE.star "c", "de"]

      specify "with parens" $
        "a(bc)*de" `shouldParseRE` RE.concat ["a", RE.star "bc", "de"]

      specify "bad" $
        shouldFailRE "*abc"

    describe "+" do
      specify "with other chars" $
        "abc+de" `shouldParseRE` RE.concat ["ab", RE.plus "c", "de"]

      specify "with parens" $
        "a(bc)+de" `shouldParseRE` RE.concat ["a", RE.plus "bc", "de"]

      specify "bad" $
        shouldFailRE "+abc"

    describe "|" do
      specify "basic" $
        "abc|def" `shouldParseRE` RE.alt "abc" "def"

      specify "chained" $
        "ab|cd|ef|gh" `shouldParseRE` RE.alts ["ab", "cd", "ef", "gh"]

      specify "in parens" $
        "a(bc|de)f" `shouldParseRE` RE.concat ["a", RE.alt "bc" "de", "f"]

      specify "empty" $
        "|" `shouldParseRE` RE.alt RE.Empty RE.Empty

      specify "left empty" $
        "|ab" `shouldParseRE` RE.alt RE.Empty "ab"

      specify "right empty" $
        "ab|" `shouldParseRE` RE.alt "ab" RE.Empty

