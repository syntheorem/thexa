module Thexa.IntLike.Set
( ILSet

-- * Construction
, empty
, singleton
, fromList
, fromAscList
, fromDistinctAscList

-- * Modification
, insert
, delete

-- * Queries
, member
, notMember
, null
, size

-- * Combining
, union
, unions
, difference
, (\\)
, intersection

-- * Folds
, foldr
, foldl
, foldr'
, foldl'

-- * Conversion
, elems
, toList
, toAscList
, toDescList
, toIntSet
, unsafeFromIntSet
) where

import PreludePrime
  hiding (empty, toList, null, foldr, foldl, foldr', foldl')

import Text.Read qualified as Read
import Text.Show qualified as Show
import Data.Foldable qualified as Foldable
import Data.IntSet qualified as IS
import GHC.Exts qualified as GHC

import Thexa.IntLike.Class

-- | A wrapper for an 'IS.IntSet' that allows any value implementing 'IntLike' to be used.
newtype ILSet a = ILSet { unILSet :: IS.IntSet }
  deriving newtype (Eq, Ord, Semigroup, Monoid, NFData)

type role ILSet nominal

instance IntLike a => GHC.IsList (ILSet a) where
  type Item (ILSet a) = a
  toList = toList
  fromList = fromList

instance (IntLike a, Show a) => Show (ILSet a) where
  showsPrec p = Show.showsPrec p . toList

instance (IntLike a, Read a) => Read (ILSet a) where
  readPrec = fromList <$> Read.readPrec

empty :: ILSet a
empty = ILSet IS.empty

singleton :: IntLike a => a -> ILSet a
singleton = coerce IS.singleton . toInt

fromList :: IntLike a => [a] -> ILSet a
fromList = coerce IS.fromList . map toInt

fromAscList :: IntLike a => [a] -> ILSet a
fromAscList = coerce IS.fromAscList . map toInt

fromDistinctAscList :: IntLike a => [a] -> ILSet a
fromDistinctAscList = coerce IS.fromDistinctAscList . map toInt

insert :: IntLike a => a -> ILSet a -> ILSet a
insert = coerce IS.insert . toInt

delete :: IntLike a => a -> ILSet a -> ILSet a
delete = coerce IS.delete . toInt

member :: IntLike a => a -> ILSet a -> Bool
member = coerce IS.member . toInt

notMember :: IntLike a => a -> ILSet a -> Bool
notMember = coerce IS.notMember . toInt

null :: ILSet a -> Bool
null = coerce IS.null

size :: ILSet a -> Int
size = coerce IS.size

union :: ILSet a -> ILSet a -> ILSet a
union = coerce IS.union

unions :: Foldable f => f (ILSet a) -> ILSet a
unions = Foldable.foldl' union empty

difference :: ILSet a -> ILSet a -> ILSet a
difference = coerce IS.difference

infixl 9 \\
(\\) :: ILSet a -> ILSet a -> ILSet a
(\\) = coerce (IS.\\)

intersection :: ILSet a -> ILSet a -> ILSet a
intersection = coerce IS.intersection

foldr :: IntLike a => (a -> b -> b) -> b -> ILSet a -> b
foldr f b = IS.foldr (f . fromInt) b . unILSet

foldl :: IntLike a => (b -> a -> b) -> b -> ILSet a -> b
foldl f b = IS.foldl (\b' -> f b' . fromInt) b . unILSet

foldr' :: IntLike a => (a -> b -> b) -> b -> ILSet a -> b
foldr' f b = IS.foldr' (f . fromInt) b . unILSet

foldl' :: IntLike a => (b -> a -> b) -> b -> ILSet a -> b
foldl' f b = IS.foldl' (\b' -> f b' . fromInt) b . unILSet

elems :: IntLike a => ILSet a -> [a]
elems = map fromInt . coerce IS.elems

toList :: IntLike a => ILSet a -> [a]
toList = map fromInt . coerce IS.toList

toAscList :: IntLike a => ILSet a -> [a]
toAscList = map fromInt . coerce IS.toAscList

toDescList :: IntLike a => ILSet a -> [a]
toDescList = map fromInt . coerce IS.toDescList

toIntSet :: ILSet a -> IS.IntSet
toIntSet = unILSet

unsafeFromIntSet :: IS.IntSet -> ILSet a
unsafeFromIntSet = ILSet
