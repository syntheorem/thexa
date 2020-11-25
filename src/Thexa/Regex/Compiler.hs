module Thexa.Regex.Compiler
( compile
) where

import PreludePrime

import Data.Char (chr, ord)
import Data.Bits ((.&.), (.|.), shiftR, shiftL, complement, countTrailingZeros)
import Data.Maybe (fromJust)

import Thexa.IntLike.Map qualified as ILMap
import Thexa.Regex.AST (Regex)
import Thexa.Regex.AST qualified as RE
import Thexa.Regex.CharSet (CharSet)
import Thexa.Regex.CharSet qualified as CS
import Thexa.NFA (NFA, ByteMap)
import Thexa.NFA qualified as NFA

compile :: [(Regex, NFA.MatchKey)] -> NFA
compile = NFA.execBuild . traverse_ (uncurry buildRegexMatch)

-- | Return type of a function that recursively builds an NFA from a regex. Takes the starting node
-- as input and returns the ending node after building the regex into the NFA.
type BuildRegex = NFA.Node -> NFA.Build NFA.Node

buildRegexMatch :: Regex -> NFA.MatchKey -> NFA.Build ()
buildRegexMatch re k = do
  end <- buildRegex re NFA.startNode
  matchNode <- NFA.newMatchNode k
  NFA.addEpsilonTransition end matchNode

buildRegex :: Regex -> BuildRegex
buildRegex = \case
  RE.Chars cs      -> buildCharSetUtf8 cs
  RE.Alt re1 re2   -> buildRegexAlt re1 re2
  RE.Seq re1 re2   -> buildRegexSeq re1 re2
  RE.Repeat re n m -> buildRegexRepeat re n m
  RE.Empty         -> \start -> pure start

buildRegexAlt :: Regex -> Regex -> BuildRegex
buildRegexAlt re1 re2 start = do
  end  <- NFA.newNode
  end1 <- buildRegex re1 start
  end2 <- buildRegex re2 start
  NFA.addEpsilonTransition end1 end
  NFA.addEpsilonTransition end2 end
  pure end

buildRegexSeq :: Regex -> Regex -> BuildRegex
buildRegexSeq re1 re2 start = do
  start' <- buildRegex re1 start
  buildRegex re2 start'

buildRegexRepeat :: Regex -> Natural -> Maybe Natural -> BuildRegex
buildRegexRepeat re n = \case
  Just m  -> buildRegexRepeatBounded re n m
  Nothing -> buildRegexRepeatUnbounded re n

buildRegexRepeatUnbounded :: Regex -> Natural -> BuildRegex
buildRegexRepeatUnbounded re n start
  | n == 0    = pure start
  | otherwise = do
      end1 <- buildRegexRepeatExact re (n - 1) start
      end2 <- buildRegex re end1
      NFA.addEpsilonTransition end2 end1
      pure end2

buildRegexRepeatBounded :: Regex -> Natural -> Natural -> BuildRegex
buildRegexRepeatBounded re n m start = do
  start' <- buildRegexRepeatExact re n start
  buildRegexRepeatUpTo re m start'

buildRegexRepeatExact :: Regex -> Natural -> BuildRegex
buildRegexRepeatExact re n start
  | n == 0    = pure start
  | otherwise = do
      start' <- buildRegex re start
      buildRegexRepeatExact re (n - 1) start'

buildRegexRepeatUpTo :: Regex -> Natural -> BuildRegex
buildRegexRepeatUpTo re n start
  | n == 0    = pure start
  | otherwise = do
      final <- NFA.newNode

      let go 0 _ = pure final
          go n' start' = do
            end <- buildRegex re start'
            NFA.addEpsilonTransition end final
            go (n' - 1) end

      go n start

{- UTF-8 encoding table
+-------------------+------------------+-----------+-----------+-----------+----------+
| First code point  | Last code point  |  Byte 1   |  Byte 2   |  Byte 3   |  Byte 4  |
+-------------------+------------------+-----------+-----------+-----------+----------+
| U+0000            | U+007F           | 0xxxxxxx  |           |           |          |
| U+0080            | U+07FF           | 110xxxxx  | 10xxxxxx  |           |          |
| U+0800            | U+FFFF           | 1110xxxx  | 10xxxxxx  | 10xxxxxx  |          |
| U+10000           | U+10FFFF         | 11110xxx  | 10xxxxxx  | 10xxxxxx  | 10xxxxxx |
+-------------------+------------------+-----------+-----------+-----------+----------+
-}

-- | Build the NFA that matches the characters in the given set when UTF-8 encoded.
--
-- The naive (and simpler) way to implement this is to enumerate all the characters in the set and
-- construct separate nodes to match each character, all of which end at the returned end node. But
-- there are over one million Unicode code points, so such an implementation would be very slow and
-- potentially create millions of nodes for large character ranges.
--
-- Instead, we want to match one byte, then have a single node to match the second byte of any
-- characters starting from that first byte, then a node to match the third byte given those first
-- two bytes, etc. This is better, but to match the full range of Unicode characters, we still end
-- up with @1 + (2^5) + (2^4)(2^6) + (2^3)(2^6)(2^6) = 33,825@ nodes, even though it could be done
-- with just a few nodes.
--
-- To optimize cases like this with large contiguous ranges of characters, we preconstruct nodes to
-- match all possible remaining bytes for 2, 3, and 4 byte characters, then these nodes are shared
-- among all ranges of characters that we identify they can be used for. This allows us to match the
-- full range of Unicode characters with the optimal 5 nodes.
--
-- It is still possible to construct cases that use a suboptimal number of states, but they would
-- either take a large number of states anyway (e.g., a large set of non-contiguous characters) or
-- are very contrived (e.g., all characters with even code points).
--
-- We could guarantee the optimal number of nodes by deduplicating nodes with the exact same
-- transitions (using a data structure like @Map (ByteMap Node) Node@), but this could also
-- potentially be very expensive. And note that these extra states won't necessarily make the final
-- DFA slower, it will just use more memory.
buildCharSetUtf8 :: CharSet -> BuildRegex
buildCharSetUtf8 cs start = do
  end <- NFA.newNode

  -- 1 byte chars
  let (cs1b, csGT1b) = CS.splitLE '\x7F' cs
  let bm = ILMap.fromDistinctAscList [(fromIntegral (ord c), end) | c <- CS.toString cs1b]
  NFA.addTransitions start bm

  -- 2 byte chars
  unless (CS.null csGT1b) do
    let (cs2b, csGT2b) = CS.splitLE '\x07FF' csGT1b
    anyNext1 <- anyNextByte end
    buildNByteChars 2 end start [anyNext1] cs2b

    -- 3 byte chars
    unless (CS.null csGT2b) do
      let (cs3b, cs4b) = CS.splitLE '\xFFFF' csGT2b
      anyNext2 <- anyNextByte anyNext1
      buildNByteChars 3 end start [anyNext2, anyNext1] cs3b

      -- 4 byte chars
      unless (CS.null cs4b) do
        anyNext3 <- anyNextByte anyNext2
        buildNByteChars 4 end start [anyNext3, anyNext2, anyNext1] cs4b

  pure end

-- | Build a node that transitions to the given node when matching any byte that could be the 2nd,
-- 3rd, or 4th byte in a UTF-8 encoded character (i.e., any byte begining with 10).
anyNextByte :: NFA.Node -> NFA.Build NFA.Node
anyNextByte end = do
  n <- NFA.newNode
  NFA.addTransitions n bm
  pure n
  where
    bm = ILMap.fromDistinctAscList [(b, end) | b <- [0b10000000..0b10111111]]

-- | Build nodes that accept the characters from the given set, all of which must use @n@ bytes.
buildNByteChars
  :: Int        -- ^ @n@: Number of bytes in the chars (between 2 and 4)
  -> NFA.Node   -- ^ End node
  -> NFA.Node   -- ^ Start node
  -> [NFA.Node] -- ^ anyNextByte nodes (exactly @n - 1@ of them)
  -> CharSet   -- ^ Set of chars to accept
  -> NFA.Build ()
buildNByteChars n end = go 1
  where
    -- We never call this with an empty list.
    go _ _ [] _ = undefined

    -- Build nodes for the @i@th byte and (recursively) all subsequent bytes.
    go i start (anyNext:anyNexts) cs = do
      let grouped = groupByByte i cs

      -- Traverse the sets grouped by the @i@th byte, recursively mapping each to the node that will
      -- match the @i+1@th byte of that set.
      byteTrans <- for grouped \cs' -> do

        -- Get a char from the set so we can check the range of all chars with the same @i@th byte.
        -- We know the set is non-empty because groupByByte doesn't add entries for empty sets.
        let c = fromJust (CS.findMin cs')

        -- This is a special case because the second byte of the largest Unicode character
        -- (@\x10FFFF@) is actually @0b10001111@ rather than @0b10111111@, so we can't use the
        -- normal anyNext node.
        let canDoAnyNext = c < '\x100000' || i > 1

        if cs' == uncurry CS.range (utf8Range i c) && canDoAnyNext
          then do
            -- In this case, we know that we can match any character with the bytes that we've
            -- matched so far. As an optimization, we use a preconstructed state which matches the
            -- next @n - i@ bytes, which is shared by all ranges that reach this case.
            pure anyNext

          else do
            -- Otherwise, we need a new node to match the next byte.
            node <- NFA.newNode

            if i + 1 >= n
              then do
                -- The base case: the next byte is the last byte we need to match, so we can
                -- directly add transitions from the new node to the end node.
                let byteEndPairs = [(utf8Byte (i+1) x, end) | x <- CS.toString cs']
                NFA.addTransitions node (ILMap.fromDistinctAscList byteEndPairs)

              else do
                -- The recursive case: we have more than one byte left to match.
                go (i+1) node anyNexts cs'

            pure node

      -- Add transitions from the start node to our newly constructed nodes.
      NFA.addTransitions start byteTrans

-- | Split up the set into sets of characters which have the same @n@th byte when UTF-8 encoded.
-- Assumes that the first @n - 1@ bytes of all characters in the set are the same.
groupByByte :: Int -> CharSet -> ByteMap CharSet
groupByByte n = go ILMap.empty
  where
    go bm cs = case CS.findMin cs of
      Nothing -> bm
      Just c  -> go (ILMap.insert (utf8Byte n c) l bm) r
        where (l, r) = CS.splitLE u cs
              (_, u) = utf8Range n c

-- Range of characters with the same first @n@ bytes as @c@ when UTF-8 encoded.
utf8Range :: Int -> Char -> (Char, Char)
utf8Range n c = (chr l, chr u)
  where
    i    = ord c
    l    = i .&. complement mask
    u    = min 0x10FFFF (i .|. mask)
    mask = bitMask 0 bits
    bits = countTrailingZeros (utf8Mask n c)

-- | The @n@th byte of a UTF-8 encoded character with at least @n@ bytes.
utf8Byte :: Int -> Char -> Word8
utf8Byte n c = fromIntegral i
  where
    i      = prefix .|. ((ord c .&. mask) `shiftR` shift)
    prefix = utf8Prefix n c
    mask   = utf8Mask n c
    shift  = countTrailingZeros mask

-- | Prefix used when encoding the @n@th byte of @c@.
utf8Prefix :: Int -> Char -> Int
utf8Prefix 1 c
  | c <= '\x007F' = 0b00000000
  | c <= '\x07FF' = 0b11000000
  | c <= '\xFFFF' = 0b11100000
  | otherwise     = 0b11110000
utf8Prefix _ _
  | otherwise     = 0b10000000

-- | Mask for the bits of @c@ used when encoding the @n@th byte of @c@.
utf8Mask :: Int -> Char -> Int
utf8Mask 1 c
  | c <= '\x007F' = bitMask  0 7
  | c <= '\x07FF' = bitMask  6 5
  | c <= '\xFFFF' = bitMask 12 4
  | otherwise     = bitMask 18 3
utf8Mask 2 c
  | c <= '\x07FF' = bitMask  0 6
  | c <= '\xFFFF' = bitMask  6 6
  | otherwise     = bitMask 12 6
utf8Mask 3 c
  | c <= '\xFFFF' = bitMask  0 6
  | otherwise     = bitMask  6 6
utf8Mask _ _
  | otherwise     = bitMask  0 6

bitMask :: Int -> Int -> Int
bitMask i n = ((1 `shiftL` n) - 1) `shiftL` i
