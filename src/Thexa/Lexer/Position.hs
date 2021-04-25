module Thexa.Lexer.Position
( Position(..)
, Span(..)

, PosState
, getPosition
, initPosState
, updatePosState
) where

import PreludePrime

import Thexa.Lexer.Core (GetNextByte)
import Thexa.Regex.Unicode (findGraphemeBoundary)

-- | The position of a character relative to the start of an input stream.
--
-- In this case, "character" means an extended grapheme cluster, as defined by the Unicode standard.
-- This means that it is possible for multiple code points (i.e. 'Char' values) to exist in the same
-- column.
data Position = Position
  { posLine :: {-# UNPACK #-} !Int
  -- ^ Number of newline characters (@\n@) seen before this position.
  , posColumn :: {-# UNPACK #-} !Int
  -- ^ Number of characters seen on the current line before this position.
  , posByteOffset :: {-# UNPACK #-} !Int
  -- ^ Number of bytes in the UTF-8 encoded input stream seen before this position.
  }

data Span = Span
  { spanStartPos :: Position
  , spanEndPos :: Position
  }

data PosState = PosState
  {-# UNPACK #-} !Int -- ^ Bytes remaining in current character (grapheme cluster).
  {-# UNPACK #-} !Position -- ^ Current position.

getPosition :: PosState -> Position
getPosition (PosState _ pos) = pos

initPosState :: GetNextByte str -> str -> PosState
initPosState nextByte str = PosState (findGraphemeBoundary nextByte str) (Position 0 0 0)

updatePosState
  :: GetNextByte str
  -> Int      -- ^ Tab size: the number of columns to advance when encountering @\t@.
  -> Word8    -- ^ @b@, the next byte of the input stream.
  -> str      -- ^ The rest of the input stream.
  -> PosState -- ^ The state before @b@.
  -> PosState -- ^ The state after @b@.

updatePosState nextByte tabSize b str (PosState break Position{..})
  -- If we see a newline, just increment the line number. We can ignore the remaining bytes in the
  -- current character because a newline can't appear in the middle of a grapheme cluster.
  | b == 0xA   = PosState break' (Position (posLine + 1) 0 (posByteOffset + 1))

  -- If we see a tab, increase the column count by the tab size. We can ignore the remaining bytes
  -- in the current character because a tab can't appear in the middle of a grapheme cluster.
  | b == 0x9   = PosState break' (Position posLine (posColumn + tabSize) (posByteOffset + 1))

  -- If this is the last byte in the current character, then increment the column number and
  -- determine the length of the next character.
  | break <= 1 = PosState break' (Position posLine (posColumn + 1) (posByteOffset + 1))

  -- Otherwise we're still inside a single character, so simply decrement the remaining bytes.
  | otherwise  = PosState (break - 1) (Position posLine posColumn (posByteOffset + 1))
  where
    break' = findGraphemeBoundary nextByte str
