module Thexa.Regex.CharRangeSet
( CharRangeSet
, empty
, singleton
, range
, insert
, insertRange
) where

import PreludePrime hiding (empty, toList, fromList)

import Data.Char qualified as Char
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap

newtype CharRangeSet = CRS (IntMap Int)
  deriving (Eq, Ord)

-- Invariants:
--
-- 1. Each entry @(k, v)@ in the map represents a non-empty range, so @k <= v@.
--
-- 2. No pair of entries @(l1, u1)@ and @(l2, u2)@ are mergeable. Ranges are mergeable if they can
-- be combined into a single range that represents exactly the union of the two ranges. So if we
-- have @l1 <= l2@, then we must have @l2 > u1 + 1@.

-- | Helper pattern to convert between Char and Int
pattern IChar :: Int -> Char
pattern IChar i <- (Char.ord -> i) where
  IChar i = Char.chr i
{-# COMPLETE IChar #-}

-- | The empty set.
empty :: CharRangeSet
empty = CRS IntMap.empty

-- | Construct a set from a single character.
singleton :: Char -> CharRangeSet
singleton (IChar c) = CRS $ IntMap.singleton c c

-- | Construct a set from a range of characters.
--
-- If the second argument is smaller than the first, results in the empty set.
range :: Char -> Char -> CharRangeSet
range (IChar lower) (IChar upper)
  | upper < lower = empty
  | otherwise     = CRS $ IntMap.singleton lower upper

-- | Insert a single character.
insert :: Char -> CharRangeSet -> CharRangeSet
insert c = insertRange c c

-- | Insert a range of characters.
--
-- If the second argument is smaller than the first, does nothing.
insertRange :: Char -> Char -> CharRangeSet -> CharRangeSet
insertRange (IChar lower) (IChar upper) (CRS im)
  | upper < lower = CRS im
  | otherwise     =
      case IntMap.lookupLE lower im of
        Just (l, u)
          -- The looked up range already contains the inserted range
          | upper <= u -> CRS im
          -- The looked up range can be expanded to contain the inserted range
          | lower <= u + 1 -> mergeRangesGT l upper (IntMap.delete l im)
        -- Can't merge with the lesser range, try greater ranges
        _ -> mergeRangesGT lower upper im

-- | Helper function to insert a range after merging it with any overlapping ranges with a higher
-- lower bound.
mergeRangesGT :: Int -> Int -> IntMap Int -> CharRangeSet
mergeRangesGT lower upper im =
  case IntMap.lookupGT lower im of
    Just (l, u)
      -- No overlap, just insert the range
      | l > upper + 1 -> CRS $ IntMap.insert lower upper im
      -- Partial overlap, just merge these two ranges
      | u >= upper -> CRS $ IntMap.insert lower u (IntMap.delete l im)
      -- Looked up range is fully contained, delete it and merge recursively
      | otherwise -> mergeRangesGT lower upper (IntMap.delete l im)
    -- No range starting after this one, just insert it
    Nothing -> CRS $ IntMap.insert lower upper im

union :: CharRangeSet -> CharRangeSet -> CharRangeSet
union (CRS im1) (CRS im2)
  | IntMap.size im1 >= IntMap.size im2 = undefined

toList :: CharRangeSet -> [(Char, Char)]
toList (CRS im) = map (\(l, u) -> (IChar l, IChar u)) (IntMap.toAscList im)
