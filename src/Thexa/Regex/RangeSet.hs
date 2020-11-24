module Thexa.Regex.RangeSet
( Range(..)
, RangeSet

-- * Construction
, empty
, full
, singleton
, range
, range'

-- * Queries
, null
, findMin

-- * Insertion
, insert
, insertRange
, insertRange'

-- * Deletion
, delete
, deleteRange
, deleteRange'

-- * Union and difference
, union
, difference
, complement

-- * Splits
, splitLE

-- * List conversion
, toString
, toList
, fromList
, unsafeFromList
) where

import PreludePrime hiding (empty, null, toList)
import Control.Arrow (first)

-- | A range of characters. @Range l u@ represents all characters @c@ where @l <= c <= u@.
data Range = Range
  {-# UNPACK #-} !Char
  {-# UNPACK #-} !Char
  deriving (Eq, Ord, Show)

-- | A set of characters represented using a list of ranges.
newtype RangeSet = RS [Range]
  deriving newtype (Eq, Ord, Show)

instance Semigroup RangeSet where
  (<>) = union

instance Monoid RangeSet where
  mempty = empty

-- Invariants:
--
-- 1. Each entry @Range l u@ in the list represents a non-empty range, so @l <= u@.
--
-- 2. The list is sorted, so for each adjacent pair @Range l1 u1@ and @Range l2 u2@, @l1 <= l2@.
--
-- 3. Each adjacent pair @Range l1 u1@ and @Range l2 u2@ is not mergeable into a single range
-- containing exactly the union of both ranges, so @l2 > u1 + 1@.

-- | The empty set.
empty :: RangeSet
empty = RS []

-- | The full set, containing all valid characters.
full :: RangeSet
full = range minBound maxBound

-- | Construct a set from a single character.
singleton :: Char -> RangeSet
singleton c = RS [Range c c]

-- | Construct a set from a range of characters.
--
-- If the second argument is smaller than the first, results in the empty set.
range :: Char -> Char -> RangeSet
range lower upper
  | upper < lower = empty
  | otherwise     = RS [Range lower upper]

-- | Like 'range', but takes a single 'Range' as an argument.
range' :: Range -> RangeSet
range' r@(Range lower upper)
  | upper < lower = empty
  | otherwise     = RS [r]

-- | Is this the empty set?
null :: RangeSet -> Bool
null (RS []) = True
null _       = False

-- | Lookup the smallest character in the set.
findMin :: RangeSet -> Maybe Char
findMin (RS [])            = Nothing
findMin (RS (Range c _:_)) = Just c

-- | Insert a single character.
insert :: Char -> RangeSet -> RangeSet
insert c = insertRange c c

-- | Insert a range of characters.
--
-- If the second argument is smaller than the first, does nothing.
insertRange :: Char -> Char -> RangeSet -> RangeSet
insertRange lower upper = union (range lower upper)

-- | Like 'insertRange', but takes a single 'Range' as an argument.
insertRange' :: Range -> RangeSet -> RangeSet
insertRange' = union . range'

-- | Delete a single character.
delete :: Char -> RangeSet -> RangeSet
delete = flip difference . singleton

-- | Delete a range of characters.
deleteRange :: Char -> Char -> RangeSet -> RangeSet
deleteRange lower upper = flip difference (range lower upper)

-- | Like 'deleteRange', but takes a single 'Range' as an argument.
deleteRange' :: Range -> RangeSet -> RangeSet
deleteRange' = flip difference . range'

-- | Union of two 'RangeSet's.
union :: RangeSet -> RangeSet -> RangeSet
union (RS rs1) (RS rs2) = RS $ unionLists rs1 rs2

unionLists :: [Range] -> [Range] -> [Range]
unionLists [] rs = rs
unionLists rs [] = rs
unionLists rs1@(r1@(Range l1 u1):tail1) rs2@(r2@(Range l2 u2):tail2)
  -- No overlap, just order the ranges
  | u1 < maxBound && succ u1 < l2 = r1 : unionLists tail1 rs2
  | u2 < maxBound && succ u2 < l1 = r2 : unionLists rs1 tail2
  -- Overlap, so merge the ranges into one. But we have to recurse on the new, larger range since it
  -- may merge with subsequent ranges. So we can prepend to the list where the range we're replacing
  -- had the larger upper bound, since it can't overlap with the next range in that list.
  | u1 <= u2 = unionLists tail1 (Range (min l1 l2) u2 : tail2)
  | u2 <= u1 = unionLists (Range (min l1 l2) u1 : tail1) tail2
  -- The above cases are exhaustive if the RangeSet invariants hold.
  | otherwise = error "broken RangeSet invariants"

-- | Set difference between two 'RangeSet's.
difference :: RangeSet -> RangeSet -> RangeSet
difference (RS rs1) (RS rs2) = RS $ diffLists rs1 rs2

diffLists :: [Range] -> [Range] -> [Range]
diffLists rs [] = rs
diffLists [] _  = []
diffLists rs1@(r1@(Range l1 u1):tail1) rs2@(Range l2 u2:tail2)
  -- r1 is fully before r2, so it can't be deleted
  | u1 < l2   = r1 : diffLists tail1 rs2
  -- r2 is fully before r1, so it can't delete anything else
  | u2 < l1   = diffLists rs1 tail2
  -- Overlapping cases, determine what we can keep
  | l1 < l2   = Range l1 (pred l2) : overlapTail
  | otherwise = overlapTail
  where
    -- The recursive tail call for the overlapping cases
    overlapTail = case compare u1 u2 of
      EQ -> diffLists tail1 tail2
      LT -> diffLists tail1 rs2
      GT -> diffLists (Range (succ u2) u1 : tail1) tail2

-- | Set complement of a 'RangeSet'.
complement :: RangeSet -> RangeSet
complement = difference full

-- | Split a 'RangeSet' into two.
--
-- Given @(l, r) = splitLE c x@, @l@ is the subset of @x@ with characters @<= c@ and @r@ is the
-- subset of @x@ with characters @> c@.
splitLE :: Char -> RangeSet -> (RangeSet, RangeSet)
splitLE c (RS list)
  | c == maxBound = (RS list, empty)
  | otherwise     = coerce (go list)
  where
    go [] = ([], [])
    go (r@(Range l u) : rs)
      | u <= c    = first (r:) (go rs)
      | l <= c    = first (Range l c:) (go (Range (succ c) u : rs))
      | otherwise = ([], r:rs)

-- | Convert to an ordered list of characters in the set.
toString :: RangeSet -> String
toString (RS rs) = [ c | Range l u <- rs, c <- [l..u] ]

-- | Convert to an ordered list of non-overlapping ranges.
toList :: RangeSet -> [Range]
toList (RS rs) = rs

-- | Construct from a list of arbitrary ranges.
fromList :: [Range] -> RangeSet
fromList = foldMap (RS . (:[]))

-- | Construct from a list that already satisfies the RangeSet invariants.
--
-- Basically just used for testing.
unsafeFromList :: [Range] -> RangeSet
unsafeFromList = RS
