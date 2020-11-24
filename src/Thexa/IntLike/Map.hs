module Thexa.IntLike.Map
( ILMap

-- * Construction
, empty
, singleton
, fromSet
, fromList
, fromListWith
, fromListWithKey
, fromAscList
, fromAscListWith
, fromAscListWithKey
, fromDistinctAscList

-- * Modification
, insert
, insertWith
, delete

-- * Queries
, lookup
, member
, notMember
, null
, size

-- * Combining
, union
, unionWith
, unionWithKey
, unionWithSetInsert
, unions
, unionsWith
, difference
, (\\)
, differenceWith
, differenceWithKey
, intersection
, intersectionWith
, intersectionWithKey

-- * Mapping
, mapWithKey
, traverseWithKey

-- * Folds
, foldr
, foldl
, foldrWithKey
, foldlWithKey
, foldMapWithKey
, foldr'
, foldl'
, foldrWithKey'
, foldlWithKey'

-- * Conversion
, elems
, keys
, assocs
, keysSet
, toList

-- * Filter
, restrictKeys
, withoutKeys
) where

import PreludePrime
  hiding (empty, toList, null, foldr, foldl, foldr', foldl')

import Text.Read qualified as Read
import Text.Show qualified as Show
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict qualified as IM
import Data.IntMap.Merge.Strict qualified as IM
import GHC.Exts qualified as GHC

import Thexa.IntLike.Class
import Thexa.IntLike.Set (ILSet)
import Thexa.IntLike.Set qualified as ILSet

-- | A wrapper for a strict 'IM.IntMap' that allows any key implementing 'IntLike' to be used.
newtype ILMap k v = ILMap { unILMap :: IM.IntMap v }
  deriving newtype (Functor, Foldable, Eq, Ord, Semigroup, Monoid, NFData)

instance Traversable (ILMap k) where
  traverse f = fmap ILMap . traverse f . unILMap

instance IntLike k => GHC.IsList (ILMap k v) where
  type Item (ILMap k v) = (k, v)
  toList = toList
  fromList = fromList

instance (IntLike k, Show k, Show v) => Show (ILMap k v) where
  showsPrec p = Show.showsPrec p . toList

instance (IntLike k, Read k, Read v) => Read (ILMap k v) where
  readPrec = fromList <$> Read.readPrec

-- | @unionWithSetInsert x y = 'unionWith' 'ILSet.union' x (map 'ILSet.singleton' y)@, but more
-- efficient by using 'IM.merge'. This isn't wrapping a normal @IntMap@ function, but I need it and
-- writing it here is easier than wrapping the whole merge API.
unionWithSetInsert :: IntLike v => ILMap k (ILSet v) -> ILMap k v -> ILMap k (ILSet v)
unionWithSetInsert (ILMap m1) (ILMap m2) = ILMap $ IM.merge
  IM.preserveMissing                                   -- only in m1: preserve set
  (IM.mapMissing     \_ v      -> ILSet.singleton v)   -- only in m2: make singleton
  (IM.zipWithMatched \_ setV v -> ILSet.insert v setV) -- in both: insert into set
  m1 m2

empty :: ILMap k v
empty = ILMap IM.empty

singleton :: IntLike k => k -> v -> ILMap k v
singleton k = ILMap . IM.singleton (toInt k)

fromSet :: IntLike k => (k -> v) -> ILSet k -> ILMap k v
fromSet f = ILMap . IM.fromSet (f . fromInt) . ILSet.toIntSet

fromList :: IntLike k => [(k, v)] -> ILMap k v
fromList = ILMap . IM.fromList . map (\(k, v) -> (toInt k, v))

fromListWith :: IntLike k => (v -> v -> v) -> [(k, v)] -> ILMap k v
fromListWith f = ILMap . IM.fromListWith f . map (\(k, v) -> (toInt k, v))

fromListWithKey :: IntLike k => (k -> v -> v -> v) -> [(k, v)] -> ILMap k v
fromListWithKey f = ILMap . IM.fromListWithKey (f . fromInt) . map (\(k, v) -> (toInt k, v))

fromAscList :: IntLike k => [(k, v)] -> ILMap k v
fromAscList = ILMap . IM.fromAscList . map (\(k, v) -> (toInt k, v))

fromAscListWith :: IntLike k => (v -> v -> v) -> [(k, v)] -> ILMap k v
fromAscListWith f = ILMap . IM.fromAscListWith f . map (\(k, v) -> (toInt k, v))

fromAscListWithKey :: IntLike k => (k -> v -> v -> v) -> [(k, v)] -> ILMap k v
fromAscListWithKey f = ILMap . IM.fromAscListWithKey (f . fromInt) . map (\(k, v) -> (toInt k, v))

fromDistinctAscList :: IntLike k => [(k, v)] -> ILMap k v
fromDistinctAscList = ILMap . IM.fromDistinctAscList . map (\(k, v) -> (toInt k, v))

insert :: IntLike k => k -> v -> ILMap k v -> ILMap k v
insert k v = ILMap . IM.insert (toInt k) v . unILMap

insertWith :: IntLike k => (v -> v -> v) -> k -> v -> ILMap k v -> ILMap k v
insertWith f k v = ILMap . IM.insertWith f (toInt k) v . unILMap

delete :: IntLike k => k -> ILMap k v -> ILMap k v
delete k = ILMap . IM.delete (toInt k) . unILMap

lookup :: IntLike k => k -> ILMap k v -> Maybe v
lookup k = IM.lookup (toInt k) . unILMap

member :: IntLike k => k -> ILMap k v -> Bool
member k = IM.member (toInt k) . unILMap

notMember :: IntLike k => k -> ILMap k v -> Bool
notMember k = IM.notMember (toInt k) . unILMap

null :: ILMap k v -> Bool
null = IM.null . unILMap

size :: ILMap k v -> Int
size = IM.size . unILMap

union :: ILMap k v -> ILMap k v -> ILMap k v
union (ILMap m1) (ILMap m2) = ILMap (IM.union m1 m2)

unionWith :: (v -> v -> v) -> ILMap k v -> ILMap k v -> ILMap k v
unionWith f (ILMap m1) (ILMap m2) = ILMap (IM.unionWith f m1 m2)

unionWithKey :: IntLike k => (k -> v -> v -> v) -> ILMap k v -> ILMap k v -> ILMap k v
unionWithKey f (ILMap m1) (ILMap m2) = ILMap (IM.unionWithKey (f . fromInt) m1 m2)

unions :: Foldable f => f (ILMap k v) -> ILMap k v
unions = Foldable.foldl' union empty

unionsWith :: Foldable f => (v -> v -> v) -> f (ILMap k v) -> ILMap k v
unionsWith f = Foldable.foldl' (unionWith f) empty

difference :: ILMap k a -> ILMap k b -> ILMap k a
difference (ILMap m1) (ILMap m2) = ILMap (IM.difference m1 m2)

infixl 9 \\
(\\) :: ILMap k a -> ILMap k b -> ILMap k a
(\\) = difference

differenceWith :: (a -> b -> Maybe a) -> ILMap k a -> ILMap k b -> ILMap k a
differenceWith f (ILMap m1) (ILMap m2) = ILMap (IM.differenceWith f m1 m2)

differenceWithKey :: IntLike k => (k -> a -> b -> Maybe a) -> ILMap k a -> ILMap k b -> ILMap k a
differenceWithKey f (ILMap m1) (ILMap m2) = ILMap (IM.differenceWithKey (f . fromInt) m1 m2)

intersection :: ILMap k a -> ILMap k b -> ILMap k a
intersection (ILMap m1) (ILMap m2) = ILMap (IM.intersection m1 m2)

intersectionWith :: (a -> b -> c) -> ILMap k a -> ILMap k b -> ILMap k c
intersectionWith f (ILMap m1) (ILMap m2) = ILMap (IM.intersectionWith f m1 m2)

intersectionWithKey :: IntLike k => (k -> a -> b -> c) -> ILMap k a -> ILMap k b -> ILMap k c
intersectionWithKey f (ILMap m1) (ILMap m2) = ILMap (IM.intersectionWithKey (f . fromInt) m1 m2)

mapWithKey :: IntLike k => (k -> a -> b) -> ILMap k a -> ILMap k b
mapWithKey f = ILMap . IM.mapWithKey (f . fromInt) . unILMap

traverseWithKey :: (Applicative t, IntLike k) => (k -> a -> t b) -> ILMap k a -> t (ILMap k b)
traverseWithKey f = fmap ILMap . IM.traverseWithKey (f . fromInt) . unILMap

foldr :: (a -> b -> b) -> b -> ILMap k a -> b
foldr f b = IM.foldr f b . unILMap

foldl :: (a -> b -> a) -> a -> ILMap k b -> a
foldl f b = IM.foldl f b . unILMap

foldrWithKey :: IntLike k =>  (k -> a -> b -> b) -> b -> ILMap k a -> b
foldrWithKey f b = IM.foldrWithKey (f . fromInt) b . unILMap

foldlWithKey :: IntLike k =>  (a -> k -> b -> a) -> a -> ILMap k b -> a
foldlWithKey f b = IM.foldlWithKey (\a -> f a . fromInt) b . unILMap

foldMapWithKey :: (Monoid m, IntLike k) => (k -> a -> m) -> ILMap k a -> m
foldMapWithKey f = IM.foldMapWithKey (f . fromInt) . unILMap

foldr' :: (a -> b -> b) -> b -> ILMap k a -> b
foldr' f b = IM.foldr' f b . unILMap

foldl' :: (a -> b -> a) -> a -> ILMap k b -> a
foldl' f b = IM.foldl' f b . unILMap

foldrWithKey' :: IntLike k =>  (k -> a -> b -> b) -> b -> ILMap k a -> b
foldrWithKey' f b = IM.foldrWithKey' (f . fromInt) b . unILMap

foldlWithKey' :: IntLike k =>  (a -> k -> b -> a) -> a -> ILMap k b -> a
foldlWithKey' f b = IM.foldlWithKey' (\a -> f a . fromInt) b . unILMap

elems :: ILMap k v -> [v]
elems = IM.elems . unILMap

keys :: IntLike k => ILMap k v -> [k]
keys = map fromInt . IM.keys . unILMap

assocs :: IntLike k => ILMap k v -> [(k, v)]
assocs = toList

keysSet :: ILMap k v -> ILSet k
keysSet = ILSet.unsafeFromIntSet . IM.keysSet . unILMap

toList :: IntLike k => ILMap k v -> [(k, v)]
toList = map (\(k, v) -> (fromInt k, v)) . IM.toList . unILMap

restrictKeys :: ILMap k v -> ILSet k -> ILMap k v
restrictKeys (ILMap im) = ILMap . IM.restrictKeys im . ILSet.toIntSet

withoutKeys :: ILMap k v -> ILSet k -> ILMap k v
withoutKeys (ILMap im) = ILMap . IM.restrictKeys im . ILSet.toIntSet
