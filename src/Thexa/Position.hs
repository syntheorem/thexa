module Thexa.Position
( Position(..)
, posLineOffset
, Span(..)

-- * Stateful position tracking
, PosState
, GetNextByte
, posStatePosition
, initPosState
, updatePosState
, findGraphemeBoundary
) where

import Thexa.Internal.DFA qualified as DFA
import Thexa.Internal.Regex.Compiler (compileRegex)
import Thexa.Internal.Unicode.Grapheme (grapheme)

-- | The position of a character relative to the start of an input stream.
--
-- In this case, "character" means an extended grapheme cluster, as defined by the Unicode standard.
-- This means that it is possible for multiple code points (i.e. 'Char' values) to exist in the same
-- column.
data Position = Position
  { posLine       :: {-# UNPACK #-} !Int
  -- ^ Number of newline characters (@\\n@, @\\r\\n@, or @\\r@) seen before this position.
  , posColumn     :: {-# UNPACK #-} !Int
  -- ^ Number of characters seen on the current line before this position.
  , posColOffset  :: {-# UNPACK #-} !Int
  -- ^ Number of bytes in the current line seen before this position.
  , posFileOffset :: {-# UNPACK #-} !Int
  -- ^ Number of bytes in the input stream seen before this position.
  }
  deriving (Eq, Ord, Show)

instance NFData Position where
  rnf pos = pos `seq` ()

-- | Number of bytes in the input stream seen before the beginning of the line this position is on.
posLineOffset :: Position -> Int
posLineOffset (Position{..}) = posFileOffset - posColOffset

-- | The start and end positions of a span of characters.
data Span = Span
  { spanStart :: !Position
  -- ^ The position of the first character in the span.
  , spanEnd   :: !Position
  -- ^ The position /after/ the last character in the span.
  }
  deriving (Eq, Ord, Show)

instance NFData Span where
  rnf span = span `seq` ()

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

-- | Construct an initial 'PosState' given the UTF-8 encoded input string.
initPosState :: GetNextByte str -> str -> PosState
initPosState nextByte str = PosState (findGraphemeBoundary nextByte str) (Position 0 0 0 0)
{-# INLINE initPosState #-}

-- | Update a 'PosState' for the next byte of the UTF-8 encoded input stream.
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
  | b == nl       = PosState remBytes' (Position (posLine + 1) 0 0 (posFileOffset + 1))

  -- If we see a carriage return, increment the line number only if it's the last byte in the
  -- current character. This is because a \r\n sequence is considered a single character, so we'll
  -- increment the line upon encountering the \n. But if we encounter an isolated \r, then we still
  -- want to count it as a newline.
  | b == cr
  , remBytes <= 1 = PosState remBytes' (Position (posLine + 1) 0 0 (posFileOffset + 1))

  -- If we see a tab, increase the column count by the tab size. We can ignore the remaining bytes
  -- in the current character because a tab can't appear in the middle of a grapheme cluster.
  | b == tab      = PosState remBytes' (Position posLine (posColumn + tabSize) (posColOffset + 1) (posFileOffset + 1))

  -- If this is the last byte in the current character, then increment the column number and
  -- determine the length of the next character.
  | remBytes <= 1 = PosState remBytes' (Position posLine (posColumn + 1) (posColOffset + 1) (posFileOffset + 1))

  -- Otherwise we're still inside a single character, so simply decrement the remaining bytes.
  | otherwise     = PosState (remBytes - 1) (Position posLine posColumn (posColOffset + 1) (posFileOffset + 1))
  where
    remBytes' = findGraphemeBoundary nextByte str

    -- byte values for \t, \n, and \r
    tab = 0x9
    nl  = 0xA
    cr  = 0xD
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
          (if DFA.isMatchNode dfa node' then i + 1 else lastOffset)
      | otherwise = lastOffset
{-# INLINE findGraphemeBoundary #-}
