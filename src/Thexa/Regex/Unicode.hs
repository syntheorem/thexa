module Thexa.Regex.Unicode
( module Thexa.Regex.Unicode.Properties

-- * Grapheme clusters
, grapheme
, graphemeDFA
, findGraphemeBoundary
) where

import PreludePrime

import Thexa.DFA qualified as DFA
import Thexa.IntLike.Set qualified as ILSet
import Thexa.Regex.Compiler (compileRegex)

import Thexa.Regex.Unicode.Grapheme (grapheme)
import Thexa.Regex.Unicode.Properties

-- | A precompiled 'DFA' which matches the 'grapheme' Regex for UTF-8 encoded text.
graphemeDFA :: DFA.DFA (DFA.Dense Word16)
graphemeDFA = $$(let dfa = DFA.fromNFA (compileRegex grapheme) in [|| dfa ||])

-- | Given a UTF-8 encoded string, find the offset to the next grapheme boundary, in bytes.
--
-- This works by finding the longest match of the 'grapheme' regex. The regex will always match at
-- least one code point, so the returned offset will always be positive for valid UTF-8. However, if
-- the string is empty or not UTF-8, the regex may not match at all, resulting in an offset of zero.
--
-- The first argument is a 'ByteString.uncons'-like function which tells us how to split the first
-- character off from the string. Passing this as an argument lets us be generic over the particular
-- string type.
findGraphemeBoundary :: (str -> Maybe (Word8, str)) -> str -> Int
-- TODO: benchmark this function
findGraphemeBoundary getNextByte initStr
  | Just offset <- simpleBoundary = offset
  | otherwise = go 0 DFA.startNode initStr 0
  where
    -- In the common case of purely ASCII text, we want to avoid the whole DFA machinery. So to
    -- optimize this, we look at the next two bytes of the input, and if they're both ASCII, then we
    -- know there is a grapheme boundary between them. This works because none of the code points
    -- that can result in a longer grapheme cluster are ASCII. The exception is the sequence \r\n,
    -- which counts as a single grapheme, so we specifically check for that case.
    simpleBoundary = case getNextByte initStr of
      Just (b0, initStr') ->
        case getNextByte initStr' of
          Just (b1, _)
            | b0 == 0xD && b1 == 0xA -> Just 2
            | b0 <= 127 && b1 <= 127 -> Just 1
            | otherwise -> Nothing
          Nothing -> Just 1
      Nothing -> Just 0

    -- In all other cases, we step the DFA as far as we can and then take the length of the longest
    -- sequence of bytes that the DFA matched.
    dfa = graphemeDFA
    go i node str lastOffset
      | Just (b, str') <- getNextByte str
      , Just node' <- DFA.step dfa node b = go (i + 1) node' str'
          (if ILSet.null (DFA.matches dfa node') then lastOffset else i + 1)
      | otherwise = lastOffset
{-# INLINE findGraphemeBoundary #-}
