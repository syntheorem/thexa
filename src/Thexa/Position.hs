module Thexa.Position
( Position(..)
, Span(..)

-- * Stateful position tracking
, PosState
, GetNextByte
, posStatePosition
, initPosState
, updatePosState
, findGraphemeBoundary
) where

import PreludePrime

import Thexa.Internal.DFA qualified as DFA
import Thexa.Internal.IntLike.Set qualified as ILSet
import Thexa.Internal.Regex.Compiler (compileRegex)
import Thexa.Internal.Unicode.Grapheme (grapheme)

-- | The position of a character relative to the start of an input stream.
--
-- In this case, "character" means an extended grapheme cluster, as defined by the Unicode standard.
-- This means that it is possible for multiple code points (i.e. 'Char' values) to exist in the same
-- column.
data Position = Position
  { posLine   :: {-# UNPACK #-} !Int
  -- ^ Number of newline characters (@\n@) seen before this position.
  , posColumn :: {-# UNPACK #-} !Int
  -- ^ Number of characters seen on the current line before this position.
  , posOffset :: {-# UNPACK #-} !Int
  -- ^ Number of bytes in the UTF-8 encoded input stream seen before this position.
  }

-- | The start and end positions of a span of characters.
data Span = Span !Position !Position

-- | State to track the current position when iterating over a byte stream.
--
-- This is necessary because code points can consist of multiple bytes, and characters can consist
-- of multiple code points, so we need to track how many bytes are remaining in the current
-- character so we know when to update the column count.
data PosState = PosState
  {-# UNPACK #-} !Int -- ^ Bytes remaining in current character (grapheme cluster).
  {-# UNPACK #-} !Position -- ^ Current position.

-- | Type of the function used to get the next byte of the input stream.
--
-- Such a function should return the next byte and the remaining input, or 'Nothing' if the input
-- stream is empty. For example, the 'Data.ByteString.ByteString' version of this function is
-- 'Data.ByteString.uncons'.
type GetNextByte str = str -> Maybe (Word8, str)

-- | Extract the current position from a 'PosState'.
posStatePosition :: PosState -> Position
posStatePosition (PosState _ pos) = pos

-- | Construct an initial 'PosState' given the input string.
initPosState :: GetNextByte str -> str -> PosState
initPosState nextByte str = PosState (findGraphemeBoundary nextByte str) (Position 0 0 0)
{-# INLINE initPosState #-}

-- | Update a 'PosState' for the next byte of the input stream.
updatePosState
  :: GetNextByte str
  -> Int      -- ^ Tab size: the number of columns to advance when encountering @\t@.
  -> Word8    -- ^ @b@, the next byte of the input stream.
  -> str      -- ^ The rest of the input stream.
  -> PosState -- ^ The state before @b@.
  -> PosState -- ^ The state after @b@.

updatePosState nextByte tabSize b str (PosState remBytes Position{..})
  -- If we see a newline, just increment the line number. We can ignore the remaining bytes in the
  -- current character because a newline can't appear in the middle of a grapheme cluster.
  | b == 0xA      = PosState remBytes' (Position (posLine + 1) 0 (posOffset + 1))

  -- If we see a tab, increase the column count by the tab size. We can ignore the remaining bytes
  -- in the current character because a tab can't appear in the middle of a grapheme cluster.
  | b == 0x9      = PosState remBytes' (Position posLine (posColumn + tabSize) (posOffset + 1))

  -- If this is the last byte in the current character, then increment the column number and
  -- determine the length of the next character.
  | remBytes <= 1 = PosState remBytes' (Position posLine (posColumn + 1) (posOffset + 1))

  -- Otherwise we're still inside a single character, so simply decrement the remaining bytes.
  | otherwise     = PosState (remBytes - 1) (Position posLine posColumn (posOffset + 1))
  where
    remBytes' = findGraphemeBoundary nextByte str
{-# INLINE updatePosState #-}

-- | A precompiled 'DFA' which matches the 'grapheme' Regex for UTF-8 encoded text.
graphemeDFA :: DFA.DFA DFA.Dense16
graphemeDFA = $$(let dfa = DFA.fromNFA (compileRegex grapheme) in [|| dfa ||])

-- | Given a UTF-8 encoded string, find the offset to the next grapheme boundary, in bytes.
--
-- This works by finding the longest match of the 'grapheme' regex. The regex will always match at
-- least one code point, so the returned offset will always be positive for valid UTF-8. However, if
-- the string is empty or not UTF-8, the regex may not match at all, resulting in an offset of zero.
findGraphemeBoundary :: GetNextByte str -> str -> Int
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
    go !i node str !lastOffset
      | Just (b, str') <- getNextByte str
      , Just node' <- DFA.step dfa node b = go (i + 1) node' str'
          (if ILSet.null (DFA.matches dfa node') then lastOffset else i + 1)
      | otherwise = lastOffset
{-# INLINE findGraphemeBoundary #-}
