module Thexa.IntLike.Class where

import PreludePrime

-- | Class for types which can be losslessly converted to 'Int' and back.
--
-- Laws of this class:
--
-- 1. @'fromInt' . 'toInt' = 'id'@. Note that this requires 'toInt' to be total, but 'fromInt' is
-- only required to be defined for values produced by 'toInt'.
--
-- 2. @'compare' x y = 'compare' ('toInt' x) ('toInt' y)@, which is to say that the 'Ord' instance
-- is consistent with the 'Ord' instance for 'Int'. This law is to allow usage of the various
-- functions of @IntMap@ and @IntSet@ that rely on ordering.
class Ord a => IntLike a where
  toInt :: a -> Int
  fromInt :: Int -> a

instance IntLike Int where
  toInt = id
  fromInt = id

instance IntLike Int64 where
  toInt = fromIntegral
  fromInt = fromIntegral

instance IntLike Int32 where
  toInt = fromIntegral
  fromInt = fromIntegral

instance IntLike Int16 where
  toInt = fromIntegral
  fromInt = fromIntegral

instance IntLike Int8 where
  toInt = fromIntegral
  fromInt = fromIntegral

instance IntLike Word where
  toInt = fromIntegral
  fromInt = fromIntegral

instance IntLike Word64 where
  toInt = fromIntegral
  fromInt = fromIntegral

instance IntLike Word32 where
  toInt = fromIntegral
  fromInt = fromIntegral

instance IntLike Word16 where
  toInt = fromIntegral
  fromInt = fromIntegral

instance IntLike Word8 where
  toInt = fromIntegral
  fromInt = fromIntegral

instance IntLike Char where
  toInt = fromEnum
  fromInt = toEnum
