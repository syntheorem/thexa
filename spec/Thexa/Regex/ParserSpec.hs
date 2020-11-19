module Thexa.Regex.ParserSpec where

import PreludePrime

import Data.List.NonEmpty (NonEmpty((:|)))

import Test.Hspec
import Test.Hspec.Megaparsec

import Thexa.Regex.AST
import Thexa.Regex.Parser

shouldParseCS :: String -> CharSetQ -> Expectation
shouldParseCS = shouldParse . parseCharSet

shouldFailCS :: String -> Expectation
shouldFailCS = shouldFailOn parseCharSet

shouldParseRE :: String -> RegexQ -> Expectation
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
      "^ab" `shouldParseCS` csInverse "ab"

    specify "difference" $
      "ab ^ c" `shouldParseCS` CSDiff "ab" "c"

    specify "chained difference" $
      "ab ^ c ^ def" `shouldParseCS` CSDiff "ab" (CSDiff "c" "def")

    specify "bad invert"     $ shouldFailCS "^"
    specify "bad difference" $ shouldFailCS "a^"

    specify "nested" $
      "[ab[c-z][^x[A-Z]]]" `shouldParseCS`
        csUnionList ("ab":|[CSRange 'c' 'z', csInverse (csUnion "x" (CSRange 'A' 'Z'))])

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

    describe "ranges" do
      specify "basic" $
        "a-z" `shouldParseCS` CSRange 'a' 'z'

      specify "escaped" $
        "\\x00-\\x20" `shouldParseCS` CSRange '\0' ' '

      specify "quoted" $
        "'a'-'z'" `shouldParseCS` CSRange 'a' 'z'

      specify "multiple" $
        "a-zA-Z" `shouldParseCS` csUnion (CSRange 'a' 'z') (CSRange 'A' 'Z')

      specify "with other chars" $
        "abcA-Zxyz" `shouldParseCS` csUnionList ("abc":|[CSRange 'A' 'Z', "xyz"])

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
        "$foo" `shouldParseCS` CSSplice "foo"

      specify "qualified" $
        "$Bar.Foo.foo" `shouldParseCS` CSSplice "Bar.Foo.foo"

      specify "with other chars" do
        "ab$foo.y$bar$x\\$" `shouldParseCS`
          csUnionList ("ab":|[CSSplice "foo", ".y", CSSplice "bar", CSSplice "x", "$"])

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
      "%foo" `shouldParseRE` RESplice "foo"

    specify "charset splice" $
      "$foo" `shouldParseRE` RECharSet (CSSplice "foo")

    describe "empty" do
      specify "basic" $
        "" `shouldParseRE` REEmpty

      specify "parens" $
        "()" `shouldParseRE` REEmpty

      specify "string" $
        "''" `shouldParseRE` REEmpty

    describe "repeat" do
      specify "exact" $
        "(abc){5}" `shouldParseRE` reRepeat 5 "abc"

      specify "unbounded" $
        "a 'bc'{2,}" `shouldParseRE` reSeq "a" (reRepeatUnbounded 2 "bc")

      specify "bounded" $
        "[a] { 7 , 100 } " `shouldParseRE` reRepeatBounded 7 100 "a"

    describe "?" do
      specify "with other chars" $
        "abc?de" `shouldParseRE` reSeqList ["ab", reOpt "c", "de"]

      specify "with parens" $
        "a(bc)?de" `shouldParseRE` reSeqList ["a", reOpt "bc", "de"]

      specify "bad" $
        shouldFailRE "?abc"

    describe "*" do
      specify "with other chars" $
        "abc*de" `shouldParseRE` reSeqList ["ab", reStar "c", "de"]

      specify "with parens" $
        "a(bc)*de" `shouldParseRE` reSeqList ["a", reStar "bc", "de"]

      specify "bad" $
        shouldFailRE "*abc"

    describe "+" do
      specify "with other chars" $
        "abc+de" `shouldParseRE` reSeqList ["ab", rePlus "c", "de"]

      specify "with parens" $
        "a(bc)+de" `shouldParseRE` reSeqList ["a", rePlus "bc", "de"]

      specify "bad" $
        shouldFailRE "+abc"

    describe "|" do
      specify "basic" $
        "abc|def" `shouldParseRE` reAlt "abc" "def"

      specify "chained" $
        "ab|cd|ef|gh" `shouldParseRE` reAltList ["ab", "cd", "ef", "gh"]

      specify "in parens" $
        "a(bc|de)f" `shouldParseRE` reSeqList ["a", reAlt "bc" "de", "f"]

      specify "empty" $
        "|" `shouldParseRE` reAlt REEmpty REEmpty

      specify "left empty" $
        "|ab" `shouldParseRE` reAlt REEmpty "ab"

      specify "right empty" $
        "ab|" `shouldParseRE` reAlt "ab" REEmpty

