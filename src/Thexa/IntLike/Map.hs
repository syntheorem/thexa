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

-- * Min/Max
, lookupMin
, lookupMax
, findMin
, findMax
) where

import PreludePrime
  hiding (empty, toList, null, foldr, foldl, foldr', foldl')

import Text.Read qualified as Read
import Text.Show qualified as Show
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict qualified as IM
import Data.IntMap.Merge.Strict qualified as IM
import GHC.Exts qualified as GHC
import Language.Haskell.TH.Syntax (Lift(liftTyped))

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

instance Lift v => Lift (ILMap k v) where
  liftTyped (ILMap m) = [|| ILMap (IM.fromDistinctAscList kvs) ||]
    where kvs = IM.toAscList m

-- | @unionWithSetInsert x y = 'unionWith' 'ILSet.union' x (map 'ILSet.singleton' y)@, but more
-- efficient by using 'IM.merge'. This isn't wrapping a normal @IntMap@ function, but I need it and
-- writing it here is easier than wrapping the whole merge API.
unionWithSetInsert :: IntLike v => ILMap k (ILSet v) -> ILMap k v -> ILMap k (ILSet v)
unionWithSetInsert (ILMap m1) (ILMap m2) = ILMap $ IM.merge
  IM.preserveMissing                                   -- only in m1: preserve set
  (IM.mapMissing     \_ v      -> ILSet.singleton v)   -- only in m2: make singleton
  (IM.zipWithMatched \_ setV v -> ILSet.insert v setV) -- in both: insert into set
  m1 m2
{-# INLINABLE unionWithSetInsert #-}

empty :: ILMap k v
empty = ILMap IM.empty
{-# INLINE empty #-}

singleton :: IntLike k => k -> v -> ILMap k v
singleton k = ILMap . IM.singleton (toInt k)
{-# INLINE singleton #-}

fromSet :: IntLike k => (k -> v) -> ILSet k -> ILMap k v
fromSet f = ILMap . IM.fromSet (f . fromInt) . ILSet.toIntSet
{-# INLINE fromSet #-}

fromList :: IntLike k => [(k, v)] -> ILMap k v
fromList = ILMap . IM.fromList . map (\(k, v) -> (toInt k, v))
{-# INLINE fromList #-}

fromListWith :: IntLike k => (v -> v -> v) -> [(k, v)] -> ILMap k v
fromListWith f = ILMap . IM.fromListWith f . map (\(k, v) -> (toInt k, v))
{-# INLINE fromListWith #-}

fromListWithKey :: IntLike k => (k -> v -> v -> v) -> [(k, v)] -> ILMap k v
fromListWithKey f = ILMap . IM.fromListWithKey (f . fromInt) . map (\(k, v) -> (toInt k, v))
{-# INLINE fromListWithKey #-}

fromAscList :: IntLike k => [(k, v)] -> ILMap k v
fromAscList = ILMap . IM.fromAscList . map (\(k, v) -> (toInt k, v))
{-# INLINE fromAscList #-}

fromAscListWith :: IntLike k => (v -> v -> v) -> [(k, v)] -> ILMap k v
fromAscListWith f = ILMap . IM.fromAscListWith f . map (\(k, v) -> (toInt k, v))
{-# INLINE fromAscListWith #-}

fromAscListWithKey :: IntLike k => (k -> v -> v -> v) -> [(k, v)] -> ILMap k v
fromAscListWithKey f = ILMap . IM.fromAscListWithKey (f . fromInt) . map (\(k, v) -> (toInt k, v))
{-# INLINE fromAscListWithKey #-}

fromDistinctAscList :: IntLike k => [(k, v)] -> ILMap k v
fromDistinctAscList = ILMap . IM.fromDistinctAscList . map (\(k, v) -> (toInt k, v))
{-# INLINE fromDistinctAscList #-}

insert :: IntLike k => k -> v -> ILMap k v -> ILMap k v
insert k v = ILMap . IM.insert (toInt k) v . unILMap
{-# INLINE insert #-}

insertWith :: IntLike k => (v -> v -> v) -> k -> v -> ILMap k v -> ILMap k v
insertWith f k v = ILMap . IM.insertWith f (toInt k) v . unILMap
{-# INLINE insertWith #-}

delete :: IntLike k => k -> ILMap k v -> ILMap k v
delete k = ILMap . IM.delete (toInt k) . unILMap
{-# INLINE delete #-}

lookup :: IntLike k => k -> ILMap k v -> Maybe v
lookup k = IM.lookup (toInt k) . unILMap
{-# INLINE lookup #-}

member :: IntLike k => k -> ILMap k v -> Bool
member k = IM.member (toInt k) . unILMap
{-# INLINE member #-}

notMember :: IntLike k => k -> ILMap k v -> Bool
notMember k = IM.notMember (toInt k) . unILMap
{-# INLINE notMember #-}

null :: ILMap k v -> Bool
null = IM.null . unILMap
{-# INLINE null #-}

size :: ILMap k v -> Int
size = IM.size . unILMap
{-# INLINE size #-}

union :: ILMap k v -> ILMap k v -> ILMap k v
union (ILMap m1) (ILMap m2) = ILMap (IM.union m1 m2)
{-# INLINE union #-}

unionWith :: (v -> v -> v) -> ILMap k v -> ILMap k v -> ILMap k v
unionWith f (ILMap m1) (ILMap m2) = ILMap (IM.unionWith f m1 m2)
{-# INLINE unionWith #-}

unionWithKey :: IntLike k => (k -> v -> v -> v) -> ILMap k v -> ILMap k v -> ILMap k v
unionWithKey f (ILMap m1) (ILMap m2) = ILMap (IM.unionWithKey (f . fromInt) m1 m2)
{-# INLINE unionWithKey #-}

unions :: Foldable f => f (ILMap k v) -> ILMap k v
unions = Foldable.foldl' union empty
{-# INLINE unions #-}

unionsWith :: Foldable f => (v -> v -> v) -> f (ILMap k v) -> ILMap k v
unionsWith f = Foldable.foldl' (unionWith f) empty
{-# INLINE unionsWith #-}

difference :: ILMap k a -> ILMap k b -> ILMap k a
difference (ILMap m1) (ILMap m2) = ILMap (IM.difference m1 m2)
{-# INLINE difference #-}

infixl 9 \\
(\\) :: ILMap k a -> ILMap k b -> ILMap k a
(\\) = difference
{-# INLINE (\\) #-}

differenceWith :: (a -> b -> Maybe a) -> ILMap k a -> ILMap k b -> ILMap k a
differenceWith f (ILMap m1) (ILMap m2) = ILMap (IM.differenceWith f m1 m2)
{-# INLINE differenceWith #-}

differenceWithKey :: IntLike k => (k -> a -> b -> Maybe a) -> ILMap k a -> ILMap k b -> ILMap k a
differenceWithKey f (ILMap m1) (ILMap m2) = ILMap (IM.differenceWithKey (f . fromInt) m1 m2)
{-# INLINE differenceWithKey #-}

intersection :: ILMap k a -> ILMap k b -> ILMap k a
intersection (ILMap m1) (ILMap m2) = ILMap (IM.intersection m1 m2)
{-# INLINE intersection #-}

intersectionWith :: (a -> b -> c) -> ILMap k a -> ILMap k b -> ILMap k c
intersectionWith f (ILMap m1) (ILMap m2) = ILMap (IM.intersectionWith f m1 m2)
{-# INLINE intersectionWith #-}

intersectionWithKey :: IntLike k => (k -> a -> b -> c) -> ILMap k a -> ILMap k b -> ILMap k c
intersectionWithKey f (ILMap m1) (ILMap m2) = ILMap (IM.intersectionWithKey (f . fromInt) m1 m2)
{-# INLINE intersectionWithKey #-}

mapWithKey :: IntLike k => (k -> a -> b) -> ILMap k a -> ILMap k b
mapWithKey f = ILMap . IM.mapWithKey (f . fromInt) . unILMap
{-# INLINE mapWithKey #-}

traverseWithKey :: (Applicative t, IntLike k) => (k -> a -> t b) -> ILMap k a -> t (ILMap k b)
traverseWithKey f = fmap ILMap . IM.traverseWithKey (f . fromInt) . unILMap
{-# INLINE traverseWithKey #-}

foldr :: (a -> b -> b) -> b -> ILMap k a -> b
foldr f b = IM.foldr f b . unILMap
{-# INLINE foldr #-}

foldl :: (a -> b -> a) -> a -> ILMap k b -> a
foldl f b = IM.foldl f b . unILMap
{-# INLINE foldl #-}

foldrWithKey :: IntLike k =>  (k -> a -> b -> b) -> b -> ILMap k a -> b
foldrWithKey f b = IM.foldrWithKey (f . fromInt) b . unILMap
{-# INLINE foldrWithKey #-}

foldlWithKey :: IntLike k =>  (a -> k -> b -> a) -> a -> ILMap k b -> a
foldlWithKey f b = IM.foldlWithKey (\a -> f a . fromInt) b . unILMap
{-# INLINE foldlWithKey #-}

foldMapWithKey :: (Monoid m, IntLike k) => (k -> a -> m) -> ILMap k a -> m
foldMapWithKey f = IM.foldMapWithKey (f . fromInt) . unILMap
{-# INLINE foldMapWithKey #-}

foldr' :: (a -> b -> b) -> b -> ILMap k a -> b
foldr' f b = IM.foldr' f b . unILMap
{-# INLINE foldr' #-}

foldl' :: (a -> b -> a) -> a -> ILMap k b -> a
foldl' f b = IM.foldl' f b . unILMap
{-# INLINE foldl' #-}

foldrWithKey' :: IntLike k =>  (k -> a -> b -> b) -> b -> ILMap k a -> b
foldrWithKey' f b = IM.foldrWithKey' (f . fromInt) b . unILMap
{-# INLINE foldrWithKey' #-}

foldlWithKey' :: IntLike k =>  (a -> k -> b -> a) -> a -> ILMap k b -> a
foldlWithKey' f b = IM.foldlWithKey' (\a -> f a . fromInt) b . unILMap
{-# INLINE foldlWithKey' #-}

elems :: ILMap k v -> [v]
elems = IM.elems . unILMap
{-# INLINE elems #-}

keys :: IntLike k => ILMap k v -> [k]
keys = map fromInt . IM.keys . unILMap
{-# INLINE keys #-}

assocs :: IntLike k => ILMap k v -> [(k, v)]
assocs = toList
{-# INLINE assocs #-}

keysSet :: ILMap k v -> ILSet k
keysSet = ILSet.unsafeFromIntSet . IM.keysSet . unILMap
{-# INLINE keysSet #-}

toList :: IntLike k => ILMap k v -> [(k, v)]
toList = map (\(k, v) -> (fromInt k, v)) . IM.toList . unILMap
{-# INLINE toList #-}

restrictKeys :: ILMap k v -> ILSet k -> ILMap k v
restrictKeys (ILMap im) = ILMap . IM.restrictKeys im . ILSet.toIntSet
{-# INLINE restrictKeys #-}

withoutKeys :: ILMap k v -> ILSet k -> ILMap k v
withoutKeys (ILMap im) = ILMap . IM.restrictKeys im . ILSet.toIntSet
{-# INLINE withoutKeys #-}

lookupMin :: IntLike k => ILMap k v -> Maybe (k, v)
lookupMin (ILMap im) = map (\(k, v) -> (fromInt k, v)) (IM.lookupMin im)
{-# INLINE lookupMin #-}

lookupMax :: IntLike k => ILMap k v -> Maybe (k, v)
lookupMax (ILMap im) = map (\(k, v) -> (fromInt k, v)) (IM.lookupMax im)
{-# INLINE lookupMax #-}

findMin :: IntLike k => ILMap k v -> (k, v)
findMin (ILMap im) = let (k, v) = IM.findMin im in (fromInt k, v)
{-# INLINE findMin #-}

findMax :: IntLike k => ILMap k v -> (k, v)
findMax (ILMap im) = let (k, v) = IM.findMax im in (fromInt k, v)
{-# INLINE findMax #-}
