module Thexa.Regex.ParserSpec where

import PreludePrime

import Data.Map ((!))
import Test.Hspec
import Test.Hspec.Megaparsec

import Thexa.Regex.AST (RegexAST)
import Thexa.Regex.AST qualified as RE
import Thexa.Regex.CharSet.AST (CharSetAST)
import Thexa.Regex.CharSet.AST qualified as CS
import Thexa.Regex.Parser
import Thexa.Regex.Unicode qualified as UC

shouldParseCS :: String -> CharSetAST -> Expectation
shouldParseCS s = shouldParse (CS.normalize <$> parseCharSet s) . CS.normalize

shouldFailCS :: String -> Expectation
shouldFailCS = shouldFailOn parseCharSet

shouldParseRE :: String -> RegexAST -> Expectation
shouldParseRE s = shouldParse (RE.normalize <$> parseRegex s) . RE.normalize

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

    specify "empty difference" $
      "ab^" `shouldParseCS` CS.difference "ab" ""

    specify "bad invert"     $ shouldFailCS "^a^b"
    specify "bad difference" $ shouldFailCS "a^b^c"

    specify "nested" $
      "[ab[c-z][^x[A-Z]]]" `shouldParseCS`
        CS.unions ["ab", CS.range 'c' 'z', CS.complement (CS.union "x" (CS.range 'A' 'Z'))]

    specify "unclosed nested" $
      shouldFailCS "[ab"

    specify "Unicode general category" $
      "\\p{ Math_Symbol }" `shouldParseCS` CS.Chars (UC.generalCategoriesLong ! "Math_Symbol")

    specify "Unicode general category abbr" $
      "\\p{Sm}" `shouldParseCS` CS.Chars (UC.generalCategoriesAbbr ! "Sm")

    specify "Unicode script" $
      "\\p{Greek}" `shouldParseCS` CS.Chars (UC.scripts ! "Greek")

    specify "Unicode block" $
      "\\p{InLatin-1_Supplement}" `shouldParseCS` CS.Chars (UC.blocks ! "Latin-1 Supplement")

    describe "escaped chars" do
      specify "basic" $
        "\\n\\t\\r\\f\\v\\0\\\\" `shouldParseCS` "\n\t\r\f\v\0\\"

      specify "special" $
        "\\-\\^\\[\\]\\ " `shouldParseCS` "-^[] "

      specify "ASCII numeric" $
        "\\x21 \\x30" `shouldParseCS` "!0"

      specify "ASCII control codes" $
        "\\x{SOH}\\x{DEL}\\x{SO}\\x{NUL}" `shouldParseCS` "\SOH\DEL\SO\0"

      specify "Unicode" $
        "\\u{0}\\u{0x394}\\u{0003B5}\\u{03bc}" `shouldParseCS` "\0Δεμ"

      specify "Unicode out-of-bounds" $
        shouldFailCS "\\u{110000}"

      specify "Unicode empty" $
        shouldFailCS "\\u{}"

    describe "unescaped special chars" do
      specify "]" $ shouldFailCS "]"
      specify "\\" $ shouldFailCS "\\"

    describe "ranges" do
      specify "basic" $
        "a-z" `shouldParseCS` CS.range 'a' 'z'

      specify "escaped" $
        "\\x00-\\x20" `shouldParseCS` CS.range '\0' ' '

      specify "multiple" $
        "a-zA-Z" `shouldParseCS` CS.union (CS.range 'a' 'z') (CS.range 'A' 'Z')

      specify "with other chars" $
        "abcA-Zxyz" `shouldParseCS` CS.unions ["abc", CS.range 'A' 'Z', "xyz"]

      specify "invalid bounds" $ shouldFailCS "z-a"
      specify "missing bounds" $ shouldFailCS "-"
      specify "missing left"   $ shouldFailCS "-z"
      specify "missing right"  $ shouldFailCS "a-"

    describe "splice" do
      specify "basic" $
        "[:foo:]" `shouldParseCS` CS.Splice "foo"

      specify "qualified" $
        "[:  Bar.Foo.foo  :]" `shouldParseCS` CS.Splice "Bar.Foo.foo"

      specify "with other chars" do
        "ab[:foo:].y[:bar:][:x:]\\[:" `shouldParseCS`
          CS.unions ["ab", CS.Splice "foo", ".", "y", CS.Splice "bar", CS.Splice "x", "[:"]

      specify "only module" $ shouldFailCS "[:Bar.Foo:]"
      specify "bad name"    $ shouldFailCS "[:0:]"
      specify "empty"       $ shouldFailCS "[::]"

  describe "parseRegex" do
    specify "string" $
      "ab c \\x20z" `shouldParseRE` "abc z"

    specify "double quoted" $
      "\"ab c\\\"  \\x20+?\"" `shouldParseRE` "ab c\"   +?"

    specify "regex splice" $
      "{{foo}}" `shouldParseRE` RE.Splice "foo"

    specify "regex splice after char" $
      "a{{foo}}" `shouldParseRE` RE.concat ["a", RE.Splice "foo"]

    specify "embedded charset" $
      "a[bc]d" `shouldParseRE` RE.concat ["a", RE.chars "bc", "d"]

    specify "charset splice" $
      "[:foo:]" `shouldParseRE` RE.Chars (CS.Splice "foo")

    specify "Unicode property" $
      "\\p{Greek}" `shouldParseRE` RE.Chars (CS.Chars (UC.scripts ! "Greek"))

    describe "empty" do
      specify "basic" $
        "" `shouldParseRE` RE.Empty

      specify "parens" $
        "()" `shouldParseRE` RE.Empty

      specify "string" $
        "\"\"" `shouldParseRE` RE.Empty

      specify "charset" $
        shouldFailRE "[]"

    describe "escaped chars" do
      specify "basic" $
        "\\n\\t\\r\\f\\v\\0\\\\" `shouldParseRE` "\n\t\r\f\v\0\\"

      specify "special" $
        "\\(\\)\\{\\}\\[\\]\\*\\+\\?\\|\\\"\\ " `shouldParseRE` "(){}[]*+?|\" "

      specify "ASCII numeric" $
        "\\x21 \\x30" `shouldParseRE` "!0"

      specify "ASCII control codes" $
        "\\x{SOH}\\x{DEL}\\x{SO}\\x{NUL}" `shouldParseRE` "\SOH\DEL\SO\0"

      specify "Unicode" $
        "\\u{0}\\u{0x394}\\u{0003B5}\\u{03bc}" `shouldParseRE` "\0Δεμ"

      specify "Unicode out-of-bounds" $
        shouldFailRE "\\u{110000}"

      specify "Unicode empty" $
        shouldFailRE "\\u{}"

    describe "repeat" do
      specify "exact" $
        "(abc){5}" `shouldParseRE` RE.repeat 5 "abc"

      specify "unbounded" $
        "a \"bc\"{2,}" `shouldParseRE` RE.append "a" (RE.repeatUnbounded 2 "bc")

      specify "bounded" $
        "[a] { 7 , 100 } " `shouldParseRE` RE.repeatBounded 7 100 "a"

      specify "upper bound" $
        "{{foo}}{ ,5}" `shouldParseRE` RE.repeatBounded 0 5 (RE.Splice "foo")

      specify "bad" $ shouldFailRE "{5}abc"
      specify "unclosed" $ shouldFailRE "a{2"
      specify "bad inner" $ shouldFailRE "a{bad}"

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

