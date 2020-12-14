module Thexa.GrowArray
( GrowArray
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
) where

import PreludePrime hiding (length)

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Primitive.Array
import Data.Primitive.MutVar

-- | A 'MutableArray' with amortized constant time appends.
--
-- Essentially this is like a C++ @vector@, where we maintain an array with some additional capacity
-- so we can cheaply add elements to the end, and then double the capacity when we run out of room.
newtype GrowArray s a = GA (MutVar s (GAData s a))

data GAData s a = GAData
  { gaLength :: {-# UNPACK #-} !Int
  , gaMutArr :: {-# UNPACK #-} !(MutableArray s a)
  }

-- | Construct a new 'GrowArray' with a default initial capacity.
new :: PrimMonad m => m (GrowArray (PrimState m) a)
new = newWithCapacity 32
{-# INLINE new #-}

-- | Construct a new 'GrowArray' with the given capacity
newWithCapacity :: PrimMonad m => Int -> m (GrowArray (PrimState m) a)
newWithCapacity cap
  | cap < 0   = error "cannot create GrowArray with negative capacity"
  | otherwise = do
      arr <- newArray cap uninit
      var <- newMutVar (GAData 0 arr)
      pure (GA var)
{-# INLINABLE newWithCapacity #-}

-- | Get the number of elements currently in the array.
length :: PrimMonad m => GrowArray (PrimState m) a -> m Int
length = fmap gaLength . readData
{-# INLINE length #-}

-- | Get the current capacity of the array.
capacity :: PrimMonad m => GrowArray (PrimState m) a -> m Int
capacity = fmap (sizeofMutableArray . gaMutArr) . readData
{-# INLINE capacity #-}

-- | Read an element of the array. Bounds checking is performed.
read :: PrimMonad m => GrowArray (PrimState m) a -> Int -> m a
read ga i = do
  GAData len arr <- readData ga
  if i < 0 || i >= len
    then error "index out of bounds"
    else readArray arr i
{-# INLINABLE read #-}

-- | Write an element of the array. Bounds checking is performed.
write :: PrimMonad m => GrowArray (PrimState m) a -> Int -> a -> m ()
write ga i a = do
  GAData len arr <- readData ga
  if i < 0 || i >= len
    then error "index out of bounds"
    else writeArray arr i a
{-# INLINABLE write #-}

-- | Modify an element of the array. Bounds checking is performed.
modify :: PrimMonad m => GrowArray (PrimState m) a -> Int -> (a -> a) -> m ()
modify ga i f = do
  GAData len arr <- readData ga
  if i < 0 || i >= len
    then error "index out of bounds"
    else do
      a <- readArray arr i
      writeArray arr i (f a)
{-# INLINABLE modify #-}

-- | Strictly modify an element of the array. Bounds checking is performed.
modify' :: PrimMonad m => GrowArray (PrimState m) a -> Int -> (a -> a) -> m ()
modify' ga i f = do
  GAData len arr <- readData ga
  if i < 0 || i >= len
    then error "index out of bounds"
    else do
      a <- readArray arr i
      writeArray arr i $! (f a)
{-# INLINABLE modify' #-}

-- | Append an element to the end of the array. Amortized @O(1)@.
push :: PrimMonad m => GrowArray (PrimState m) a -> a -> m ()
push ga a = do
  GAData len arr <- readData ga

  arr' <- if len == sizeofMutableArray arr
    then do
      newArr <- newArray (len * 2) uninit
      copyMutableArray newArr 0 arr 0 len
      pure newArr
    else do
      pure arr

  writeArray arr' len a
  writeData ga (GAData (len + 1) arr')
{-# INLINABLE push #-}

-- | Copy the current contents of the 'GrowArray' to an immutable 'Array'.
freeze :: PrimMonad m => GrowArray (PrimState m) a -> m (Array a)
freeze ga = do
  GAData len arr <- readData ga
  freezeArray arr 0 len
{-# INLINABLE freeze #-}

----------------------
-- Helper functions --
----------------------

readData :: PrimMonad m => GrowArray (PrimState m) a -> m (GAData (PrimState m) a)
readData (GA var) = readMutVar var
{-# INLINE readData #-}

writeData :: PrimMonad m => GrowArray (PrimState m) a -> GAData (PrimState m) a -> m ()
writeData (GA var) = writeMutVar var
{-# INLINE writeData #-}

uninit :: a
uninit = error "uninitialized array element"
