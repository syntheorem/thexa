module Thexa.Internal.GrowVector
( GrowVector
, new
, newWithCapacity
, length
, capacity
, read
, write
, modify
, modify'
, push
, freeze
, unsafeFreeze
) where

import PreludePrime hiding (length)

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector (Vector, MVector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Primitive.MutVar

-- | A 'MVector' with amortized constant time appends.
--
-- Essentially this is like a C++ @vector@, where we maintain an array with some additional capacity
-- so we can cheaply add elements to the end, and then double the capacity when we run out of room.
newtype GrowVector s a = GV (MutVar s (GVData s a))

data GVData s a = GVData
  { gaLength :: {-# UNPACK #-} !Int
  , gaMutArr :: {-# UNPACK #-} !(MVector s a)
  }

-- | Construct a new 'GrowVector' with a default initial capacity.
new :: PrimMonad m => m (GrowVector (PrimState m) a)
new = newWithCapacity 32
{-# INLINE new #-}

-- | Construct a new 'GrowVector' with the given capacity.
newWithCapacity :: PrimMonad m => Int -> m (GrowVector (PrimState m) a)
newWithCapacity cap = do
  vec <- MV.new cap
  var <- newMutVar (GVData 0 vec)
  pure (GV var)
{-# INLINABLE newWithCapacity #-}

-- | Get the number of elements currently in the array.
length :: PrimMonad m => GrowVector (PrimState m) a -> m Int
length = fmap gaLength . readData
{-# INLINE length #-}

-- | Get the current capacity of the array.
capacity :: PrimMonad m => GrowVector (PrimState m) a -> m Int
capacity = fmap (MV.length . gaMutArr) . readData
{-# INLINE capacity #-}

-- | Read an element of the array. Bounds checking is performed.
read :: PrimMonad m => GrowVector (PrimState m) a -> Int -> m a
read ga i = do
  GVData len vec <- readData ga
  if i < 0 || i >= len
    then error "index out of bounds"
    else MV.unsafeRead vec i
{-# INLINABLE read #-}

-- | Write an element of the array. Bounds checking is performed.
write :: PrimMonad m => GrowVector (PrimState m) a -> Int -> a -> m ()
write ga i a = do
  GVData len vec <- readData ga
  if i < 0 || i >= len
    then error "index out of bounds"
    else MV.unsafeWrite vec i a
{-# INLINABLE write #-}

-- | Modify an element of the array. Bounds checking is performed.
modify :: PrimMonad m => GrowVector (PrimState m) a -> Int -> (a -> a) -> m ()
modify ga i f = do
  GVData len vec <- readData ga
  if i < 0 || i >= len
    then error "index out of bounds"
    else do
      a <- MV.unsafeRead vec i
      MV.unsafeWrite vec i (f a)
{-# INLINABLE modify #-}

-- | Strictly modify an element of the array. Bounds checking is performed.
modify' :: PrimMonad m => GrowVector (PrimState m) a -> Int -> (a -> a) -> m ()
modify' ga i f = do
  GVData len vec <- readData ga
  if i < 0 || i >= len
    then error "index out of bounds"
    else do
      a <- MV.unsafeRead vec i
      MV.unsafeWrite vec i $! (f a)
{-# INLINABLE modify' #-}

-- | Append an element to the end of the array. Amortized @O(1)@.
push :: PrimMonad m => GrowVector (PrimState m) a -> a -> m ()
push ga a = do
  GVData len vec <- readData ga

  vec' <- if len == MV.length vec
    then MV.grow vec len
    else pure vec

  MV.unsafeWrite vec' len a
  writeData ga (GVData (len + 1) vec')
{-# INLINABLE push #-}

-- | Copy the current contents of the 'GrowVector' to an immutable 'Vector'.
freeze :: PrimMonad m => GrowVector (PrimState m) a -> m (Vector a)
freeze ga = do
  GVData len vec <- readData ga
  V.freeze (MV.take len vec)
{-# INLINABLE freeze #-}

-- | Convert a 'GrowVector' into an immutable 'Vector' without copying.
--
-- This function is only safe if its argument is no longer used after it is called. Note also that
-- any excess capacity is not freed, possibly wasting memory.
unsafeFreeze :: PrimMonad m => GrowVector (PrimState m) a -> m (Vector a)
unsafeFreeze ga = do
  GVData len vec <- readData ga
  V.unsafeFreeze (MV.take len vec)
{-# INLINABLE unsafeFreeze #-}

----------------------
-- Helper functions --
----------------------

readData :: PrimMonad m => GrowVector (PrimState m) a -> m (GVData (PrimState m) a)
readData (GV var) = readMutVar var
{-# INLINE readData #-}

writeData :: PrimMonad m => GrowVector (PrimState m) a -> GVData (PrimState m) a -> m ()
writeData (GV var) = writeMutVar var
{-# INLINE writeData #-}
