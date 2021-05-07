module Thexa.Lexer.Position
( Position(..)
, Span(..)

, PosState
, GetNextByte
, posStatePosition
, initPosState
, updatePosState
) where

import PreludePrime

import Thexa.Regex.Unicode (findGraphemeBoundary)

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
