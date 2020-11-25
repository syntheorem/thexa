module Thexa.Regex.CharSet
( CharSet

-- * Construction
, empty
, full
, singleton
, range

-- * Queries
, null
, findMin

-- * Insertion
, insert
, insertRange

-- * Deletion
, delete
, deleteRange

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
import Data.List (unfoldr)
import Data.String (IsString(fromString))
import Language.Haskell.TH.Syntax (Lift)
import Text.Show (showsPrec)

-- | A set of characters represented using an ordered list of non-overlapping ranges.
data CharSet = Cons {-# UNPACK #-} !Char {-# UNPACK #-} !Char !CharSet | Nil
  deriving (Eq, Ord, Lift)

-- Invariants:
--
-- 1. Each entry @Cons l u _@ in the list represents a non-empty range, so @l <= u@.
--
-- 2. The list is sorted, so for @Cons l1 u1 (Cons l2 u2 _)@, @l1 <= l2@.
--
-- 3. Adjacent ranges are not mergeable into a single range containing exactly the union of both
-- ranges, so for @Cons l1 u1 (Cons l2 u2 _)@, @l2 > u1 + 1@.

instance Semigroup CharSet where
  (<>) = union

instance Monoid CharSet where
  mempty = empty

instance IsString CharSet where
  fromString = foldMap singleton

instance Show CharSet where
  showsPrec p = showsPrec p . toList

-- | The empty set.
empty :: CharSet
empty = Nil

-- | The full set, containing all valid characters.
full :: CharSet
full = range minBound maxBound

-- | Construct a set from a single character.
singleton :: Char -> CharSet
singleton c = Cons c c Nil

-- | Construct a set from a range of characters.
--
-- If the second argument is smaller than the first, results in the empty set.
range :: Char -> Char -> CharSet
range lower upper
  | upper < lower = empty
  | otherwise     = Cons lower upper Nil

-- | Is this the empty set?
null :: CharSet -> Bool
null Nil = True
null _   = False

-- | Lookup the smallest character in the set.
findMin :: CharSet -> Maybe Char
findMin Nil          = Nothing
findMin (Cons c _ _) = Just c

-- | Insert a single character.
insert :: Char -> CharSet -> CharSet
insert c = insertRange c c

-- | Insert a range of characters.
--
-- If the second argument is smaller than the first, does nothing.
insertRange :: Char -> Char -> CharSet -> CharSet
insertRange lower upper = union (range lower upper)

-- | Delete a single character.
delete :: Char -> CharSet -> CharSet
delete = flip difference . singleton

-- | Delete a range of characters.
deleteRange :: Char -> Char -> CharSet -> CharSet
deleteRange lower upper = flip difference (range lower upper)

-- | Union of two 'CharSet's.
union :: CharSet -> CharSet -> CharSet
union Nil cs = cs
union cs Nil = cs
union cs1@(Cons l1 u1 tail1) cs2@(Cons l2 u2 tail2)
  -- No overlap, just order the ranges
  | u1 < maxBound && succ u1 < l2 = Cons l1 u1 (union tail1 cs2)
  | u2 < maxBound && succ u2 < l1 = Cons l2 u2 (union cs1 tail2)
  -- Overlap, so merge the ranges into one. But we have to recurse on the new, larger range since it
  -- may merge with subsequent ranges. So we can prepend to the list where the range we're replacing
  -- had the larger upper bound, since it can't overlap with the next range in that list.
  | u1 <= u2 = union tail1 (Cons (min l1 l2) u2 tail2)
  | u2 <= u1 = union (Cons (min l1 l2) u1 tail1) tail2
  -- The above cases are exhaustive if the CharSet invariants hold.
  | otherwise = error "broken CharSet invariants"

-- | Set difference between two 'CharSet's.
difference :: CharSet -> CharSet -> CharSet
difference cs  Nil = cs
difference Nil _   = Nil
difference cs1@(Cons l1 u1 tail1) cs2@(Cons l2 u2 tail2)
  -- range1 is fully before range2, so it can't be deleted
  | u1 < l2   = Cons l1 u1 (difference tail1 cs2)
  -- range2 is fully before range1, so it can't delete anything else
  | u2 < l1   = difference cs1 tail2
  -- Overlapping cases, determine what we can keep
  | l1 < l2   = Cons l1 (pred l2) overlapTail
  | otherwise = overlapTail
  where
    -- The recursive tail call for the overlapping cases
    overlapTail = case compare u1 u2 of
      EQ -> difference tail1 tail2
      LT -> difference tail1 cs2
      GT -> difference (Cons (succ u2) u1 tail1) tail2

-- | Set complement of a 'CharSet'.
complement :: CharSet -> CharSet
complement = difference full

-- | Split a 'CharSet' into two.
--
-- Given @(l, r) = splitLE c x@, @l@ is the subset of @x@ with characters @<= c@ and @r@ is the
-- subset of @x@ with characters @> c@.
splitLE :: Char -> CharSet -> (CharSet, CharSet)
splitLE c cs
  | c == maxBound = (cs, empty)
  | otherwise     = go cs
  where
    go Nil = (Nil, Nil)
    go (Cons l u cs')
      | u <= c    = first (Cons l u) (go cs')
      | l <= c    = first (Cons l c) (go (Cons (succ c) u cs'))
      | otherwise = (Nil, Cons l u cs')

-- | Convert to an ordered list of characters in the set.
toString :: CharSet -> String
toString cs = [ c | (l, u) <- toList cs, c <- [l..u] ]

-- | Convert to an ordered list of non-overlapping ranges.
toList :: CharSet -> [(Char, Char)]
toList = unfoldr go
  where
    go Nil           = Nothing
    go (Cons l u cs) = Just ((l, u), cs)

-- | Construct from a list of arbitrary ranges.
fromList :: [(Char, Char)] -> CharSet
fromList = foldl' union empty . map (uncurry range)

-- | Construct from a list that already satisfies the CharSet invariants.
--
-- Basically just used for testing.
unsafeFromList :: [(Char, Char)] -> CharSet
unsafeFromList = foldr (uncurry Cons) Nil
