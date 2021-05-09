module Thexa.Regex.CompileSpec where

import PreludePrime

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Codec.Binary.UTF8.String qualified as UTF8
import Thexa.Internal.IntLike.Set qualified as ILSet

import Thexa.Internal.DFA (DFA)
import Thexa.Internal.DFA qualified as DFA
import Thexa.Internal.NFA (NFA, MatchKey, MatchSet)
import Thexa.Internal.NFA qualified as NFA
import Thexa.Internal.Regex.AST qualified as RE
import Thexa.Internal.Regex.Compiler (compileRegexes)
import Thexa.Internal.Regex.Parser qualified as RE

import Thexa.CharSet qualified as CS
import Thexa.Regex (Regex, CharSet, re)

-- We want to run every test against all possible representations of the DFA. Although this would be
-- sufficient for establishing correctness, we also test against the NFA directly to aid in
-- debugging, since if the NFA fails then we can rule out the NFA->DFA conversion as being the
-- source of the bug.
--
-- To do this, we use this type to hold all forms of the compiled regex.
data CompiledRegex = CR
  { nfa :: NFA
  , dense16  :: DFA DFA.Dense16
  , dense32  :: DFA DFA.Dense32
  , sparse16 :: DFA DFA.Sparse16
  , sparse32 :: DFA DFA.Sparse32
  }

-- Compile a single regex.
compile1 :: Regex -> CompiledRegex
compile1 regex = compile [(regex, 0)]

compile :: [(Regex, MatchKey)] -> CompiledRegex
compile regexes = CR {..}
  where
    nfa = compileRegexes regexes
    dense16 = DFA.fromNFA nfa
    dense32 = DFA.fromNFA nfa
    sparse16 = DFA.fromNFA nfa
    sparse32 = DFA.fromNFA nfa

-- Parse a regex. Throw an exception on failure or if the regex contains splices.
parse :: String -> Regex
parse r = case RE.parseRegex r of
  Left errs  -> error (RE.parseErrorsPretty errs)
  Right rAST -> case RE.fromAST rAST of
    Just r' -> r'
    Nothing -> error "regex contains splices"

-- Find the longest match on the given list of bytes for the NFA, returning the set of match keys
-- and the remaining unmatched bytes.
matchBytesNFA :: NFA -> [Word8] -> Maybe (MatchSet, [Word8])
matchBytesNFA nfa = go Nothing (NFA.startNodes nfa)
  where
    go lastMatch nodes bs = case bs of
      (b:bs')
        | let nodes' = NFA.step nfa nodes b
        , not (ILSet.null nodes')           -> go lastMatch' nodes' bs'
      _ | otherwise                         -> lastMatch'
      where
        lastMatch'
          | ILSet.null ms = lastMatch
          | otherwise     = Just (ms, bs)
        ms = NFA.matches nfa nodes

-- Basically the same as matchBytesNFA, but for DFAs. I could create a typeclass to abstract over
-- NFAs and DFAs to avoid this duplication, but this is the only place that would be useful.
matchBytesDFA :: DFA.Transitions t => DFA t -> [Word8] -> Maybe (MatchSet, [Word8])
matchBytesDFA dfa = go Nothing DFA.startNode
  where
    go lastMatch node bs = case bs of
      (b:bs')
        | Just node' <- DFA.step dfa node b -> go lastMatch' node' bs'
      _ | otherwise                         -> lastMatch'
      where
        lastMatch'
          | ILSet.null ms = lastMatch
          | otherwise     = Just (ms, bs)
        ms = DFA.matches dfa node

------------------
-- Actual Tests --
------------------

spec :: Spec
spec = modifyMaxSuccess (max 1000) do
  describe "char" do
    -- 1 byte char
    specifyChar 'a'
    -- 2 byte char
    specifyChar '\x80'
    -- 3 byte char
    specifyChar '\x800'
    -- 4 byte char
    specifyChar '\x10000'
    -- max char
    specifyChar '\x10FFFF'

  describe "charset" do
    propCharSet "any 1B char" $ CS.range '\x000000' '\x00007F'
    propCharSet "any 2B char" $ CS.range '\x000080' '\x0007FF'
    propCharSet "any 3B char" $ CS.range '\x000800' '\x00FFFF'
    propCharSet "any 4B char" $ CS.range '\x010000' '\x10FFFF'
    propCharSet "any char"    $ CS.range '\x000000' '\x10FFFF'

    -- Idea is to test disjoint ranges that contain chars with differing numbers of bytes
    propCharSet "disjointed" $
      CS.fromList [('a', '\xFF'), ('\x7FF', '\xFFF0'), ('\xFFFF', '\x1FFFF'), ('\xFFFFF', '\x10FFFF')]

    -- Test a set containing every multiple of 16. Pathological because it's very contrived but will
    -- force us to generate a very large number of nodes.
    propCharSet "pathological" $
      CS.unsafeFromList [(c, c) | c <- ['\0', '\16' ..]]

    -- This is testing a specific bug I fixed. Basically the UTF-8 encoding scheme can work for
    -- numbers up to 0x1FFFFF, but Unicode code points max out at 0x10FFFF. But because of an
    -- optimization we do for large ranges, we were accepting anything less than 0x13FFFF.
    specify "any char doesn't match out of range" $
      [re|[\u{0}-\u{10FFFF}]|] `shouldNotMatch` [0b11110100, 0b10010000, 0x80, 0x80]

  describeRegex ""
    [""]
    ["a", "abc"]

  describeRegex "a|b"
    ["a", "b"]
    ["", "c", "ab"]

  describeRegex "a?"
    ["", "a"]
    ["aa", "b", "ab"]

  describeRegex "a*"
    ["", "a", "aa", "aaaaaa"]
    ["b", "aab"]

  describeRegex "a+"
    ["a", "aa", "aaaaaa"]
    ["", "b", "aab"]

  describeRegex "a{3}"
    ["aaa"]
    ["", "aa", "aaaa", "bbb", "aaab"]

  describeRegex "a{3,}"
    ["aaa", "aaaa", "aaaaaaa"]
    ["", "aa", "bbb", "aaab"]

  describeRegex "a{2,5}"
    ["aa", "aaa", "aaaa", "aaaaa"]
    ["aaaaaaa", "", "a", "bb", "aab"]

  describeRegex "\\u{FF}a\\u{FFFF}b"
    ["\xFF\&a\xFFFF\&b"]
    ["", "\xFF", "\xFF\&a", "a", "\xFFFF", "ba\xFFFF", "a\xFFFF\&b"]

  describeRegex "\\p{Sm}"
    ["÷", "×"]
    ["", "a", " "]

  describeRegex "\\p{Greek}"
    ["δ", "Φ"]
    ["", "a", " "]

  describeRegex "\\p{InBasic_Latin}"
    ["a", "\n", " ", "1"]
    ["", "÷", "Φ"]

  describeRegex "a|b*c"
    ["a", "c", "bbbc"]
    ["", "abc", "ba"]

  describeRegexes "multiple regexes"
    [ ("a+"   , ["a", "aa"])
    , ("a*b+" , ["b", "bbb", "ab", "aaabb"])
    , ("ab?c" , ["abc", "ac"])
    , (""     , [""])
    ]

-- Test a regex matching only the given character.
specifyChar :: HasCallStack => Char -> Spec
specifyChar c = describe (show c) do
  let cr = compile1 (RE.char c)
  let cBytes = UTF8.encodeChar c

  specify ("matches "<>show c) $
    cr `shouldMatchC` cBytes

  prop "doesn't match other chars" $
    forAll charGen \c' -> (c /= c') ==>
      propNoMatch cr (UTF8.encodeChar c')

  prop "doesn't match other bytes" $
    \bytes -> (cBytes /= bytes) ==> propNoMatch cr bytes

-- Test a regex matching the given set of characters.
propCharSet :: String -> CharSet -> Spec
propCharSet str cs = prop str $
  forAll charGen \c ->
    let cBytes = UTF8.encodeChar c in
    if CS.member c cs
      then label "in set" $ propMatch cr cBytes
      else propNoMatch cr cBytes
  where
    cr = compile1 (RE.chars cs)

-- Test the regex against a list of strings it should match and a list of strings it shouldn't.
describeRegex :: HasCallStack => String -> [String] -> [String] -> Spec
describeRegex r matchStrs noMatchStrs = describe desc do
  for_ matchStrs \str ->
    it ("matches "<>show str) $
      cr `shouldMatchC` UTF8.encode str
  for_ noMatchStrs \str ->
    it ("doesn't match "<>show str) $
      cr `shouldNotMatchC` UTF8.encode str
  where
    desc = if r == "" then "\"\"" else r
    cr = compile1 (parse r)

-- Test multiple regexes compiled together against a list of strings that each one should match.
describeRegexes :: HasCallStack => String -> [(String, [String])] -> Spec
describeRegexes name rs = describe name do
  for_ irs \(desc, matchStrs, key) -> do
    describe desc do
      for_ matchStrs \str -> do
        it ("matches "<>show str) $
          cr `shouldMatchKeyC` (key, UTF8.encode str)
  where
    cr = compile [(parse r, i) | (r, _) <- rs | i <- [0..]]
    irs = [(if r == "" then "\"\"" else r, matchStrs, i) | (r, matchStrs) <- rs | i <- [0..]]

-- The Arbitrary instance for Char is... less arbitrary than I would like. Basically it only
-- generates a subset of code points and is heavily biased towards ASCII characters. For my use
-- case, I want it to generate anything. But using the Random instance makes it so that about 95% of
-- the chars we generate are 4 bytes when UTF-8 encoded, so I'm using this generator to get a better
-- spread of characters based on the number of bytes in their encoding.
charGen :: Gen Char
charGen = frequency
  [ (1, chooseEnum ('\x000000', '\x00007F'))
  , (2, chooseEnum ('\x000080', '\x0007FF'))
  , (3, chooseEnum ('\x000800', '\x00FFFF'))
  , (4, chooseEnum ('\x010000', '\x10FFFF'))
  ]

----------------------------------
-- Hspec and QuickCheck helpers --
----------------------------------

shouldMatch :: HasCallStack => Regex -> [Word8] -> Expectation
shouldMatch = shouldMatchC . compile1

shouldNotMatch :: HasCallStack => Regex -> [Word8] -> Expectation
shouldNotMatch = shouldNotMatchC . compile1

shouldMatchC :: HasCallStack => CompiledRegex -> [Word8] -> Expectation
shouldMatchC = shouldMatchWith expectExactMatch

shouldNotMatchC :: HasCallStack => CompiledRegex -> [Word8] -> Expectation
shouldNotMatchC = shouldMatchWith expectNoExactMatch

shouldMatchKeyC :: HasCallStack => CompiledRegex -> (MatchKey, [Word8]) -> Expectation
shouldMatchKeyC cr (key, bs) = shouldMatchWith (expectMatchKey key) cr bs

-- Tests all forms of the compiled regex at once. The provided function receives the result of
-- matching the given bytes, and should return an error message or Nothing on success.
shouldMatchWith :: HasCallStack
  => (Maybe (MatchSet, [Word8]) -> Maybe String)
  -> CompiledRegex -> [Word8] -> Expectation
shouldMatchWith f cr bs
  | Just err <- f (matchBytesNFA (nfa      cr) bs) = expectationFailure ("NFA failed: "<>err)
  | Just err <- f (matchBytesDFA (dense16  cr) bs) = expectationFailure ("DFA Dense16 failed: "<>err)
  | Just err <- f (matchBytesDFA (dense32  cr) bs) = expectationFailure ("DFA Dense32 failed: "<>err)
  | Just err <- f (matchBytesDFA (sparse16 cr) bs) = expectationFailure ("DFA Sparse16 failed: "<>err)
  | Just err <- f (matchBytesDFA (sparse32 cr) bs) = expectationFailure ("DFA Sparse32 failed: "<>err)
  | otherwise = pure ()

propMatch :: CompiledRegex -> [Word8] -> Property
propMatch = propMatchWith expectExactMatch

propNoMatch :: CompiledRegex -> [Word8] -> Property
propNoMatch = propMatchWith expectNoExactMatch

-- Like shouldMatchWith, but in a QuickCheck context.
propMatchWith :: ()
  => (Maybe (MatchSet, [Word8]) -> Maybe String)
  -> CompiledRegex -> [Word8] -> Property
propMatchWith f cr bs = conjoin
  [ counterexample "NFA"          $ go (matchBytesNFA (nfa      cr) bs)
  , counterexample "DFA Dense16"  $ go (matchBytesDFA (dense16  cr) bs)
  , counterexample "DFA Dense32"  $ go (matchBytesDFA (dense32  cr) bs)
  , counterexample "DFA Sparse16" $ go (matchBytesDFA (sparse16 cr) bs)
  , counterexample "DFA Sparse32" $ go (matchBytesDFA (sparse32 cr) bs)
  ]
  where
    go result = case f result of
      Just err -> counterexample err False
      Nothing  -> property True

expectExactMatch :: Maybe (MatchSet, [Word8]) -> Maybe String
expectExactMatch = \case
  Just (_, []) -> Nothing
  Just (_, bs) -> Just ("matched with remaining bytes: "<>show bs)
  Nothing      -> Just "no matches found"

expectNoExactMatch :: Maybe (MatchSet, [Word8]) -> Maybe String
expectNoExactMatch = \case
  Just (_, []) -> Just "matched entire string"
  _            -> Nothing

expectMatchKey :: MatchKey -> Maybe (MatchSet, [Word8]) -> Maybe String
expectMatchKey key = \case
  Just (ms, [])
    | ms == keySing -> Nothing
    | otherwise     -> Just ("expected match keys "<>show keySing<>", got "<>show ms)
  Just (_, bs)      -> Just ("matched with remaining bytes: "<>show bs)
  Nothing           -> Just "no matches found"
  where
    keySing = ILSet.singleton key
