module Thexa.PrimMap
( PrimMap
, fromList
, unsafeFromList
, size
, lookup
, toList
) where

import PreludePrime hiding (toList)

import Control.Monad.ST (runST)
import Data.List (zip)
import Data.Map qualified as Map
import Data.Primitive.PrimArray
import Data.Primitive.Types (Prim)
import Language.Haskell.TH.Syntax (Lift)
import Text.Show (showsPrec)
import Thexa.Orphans ()

-- | An efficient lookup table for primitive types.
--
-- Implemented by storing the unboxed keys and values in two separate arrays, sorted according to
-- the ordering of the keys. Lookups are then done using a binary search on the key array.
--
-- Surprisingly, some basic benchmarking shows that @PrimMap@ lookups don't perform any better than
-- @IntMap@ lookups. However, @PrimMap@s are much more space efficient.
data PrimMap k v = PM
  {-# UNPACK #-} !(PrimArray k) -- keys
  {-# UNPACK #-} !(PrimArray v) -- values
  deriving (Eq, Ord, Lift)

instance NFData (PrimMap k v) where
  rnf (PM _ _) = ()

instance (Prim k, Prim v, Show k, Show v) => Show (PrimMap k v) where
  showsPrec p = showsPrec p . toList

-- | Construct from a list of key value pairs. @O(n log n)@
fromList :: (Prim k, Prim v, Ord k) => [(k, v)] -> PrimMap k v
fromList kvs = unsafeFromList (Map.size m) (Map.toAscList m)
  where m = Map.fromList kvs
{-# INLINE fromList #-}

-- | Construct from a list assuming the following unchecked conditions:
--
-- 1. The first argument is the actual length of the list.
-- 2. The keys of the list are sorted in ascending order.
-- 3. No key appears in the list more than once.
--
-- @O(n)@
unsafeFromList :: (Prim k, Prim v) => Int -> [(k, v)] -> PrimMap k v
unsafeFromList n kvs = runST do
  kArr <- newPrimArray n
  vArr <- newPrimArray n

  let writeKV i (k, v) = do
        writePrimArray kArr i k
        writePrimArray vArr i v
        pure (i + 1)

  _ <- foldlM writeKV 0 kvs

  kArr' <- unsafeFreezePrimArray kArr
  vArr' <- unsafeFreezePrimArray vArr
  pure (PM kArr' vArr')
{-# INLINABLE unsafeFromList #-}

-- | Number of entries in the map. @O(1)@
size :: Prim k => PrimMap k v -> Int
size (PM kArr _) = sizeofPrimArray kArr
{-# INLINE size #-}

-- | Lookup the value associated with the given key. @O(log n)@
lookup :: (Prim k, Prim v, Ord k) => k -> PrimMap k v -> Maybe v
lookup k (PM kArr vArr) = go 0 (sizeofPrimArray kArr)
  where
    go l u
      | l == u    = Nothing
      | otherwise = case compare k ki of
          EQ -> Just $! vi
          LT -> go l i
          GT -> go (i + 1) u
      where
        i = l + ((u - l) `div` 2)
        ki = indexPrimArray kArr i
        vi = indexPrimArray vArr i
{-# INLINABLE lookup #-}

-- | Convert to a sorted list of key-value pairs. @O(n)@
toList :: (Prim k, Prim v) => PrimMap k v -> [(k, v)]
toList (PM kArr vArr) = zip (primArrayToList kArr) (primArrayToList vArr)
{-# INLINE toList #-}
