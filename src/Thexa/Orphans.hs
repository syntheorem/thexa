{-# OPTIONS_GHC -Wno-orphans #-}

-- | 'Lift' orphan instances. These aren't strictly necessary, but I really want to be able to
-- derive 'Lift' for types using arrays. Not just because it's convenient, but because it's less
-- error-prone than writing the instance manually.
module Thexa.Orphans () where

import PreludePrime

import Control.Monad.Primitive (primitive_)
import Control.Monad.ST (runST)
import Data.Primitive.Array
import Data.Primitive.PrimArray
import Data.Primitive.Types (Prim, Ptr(Ptr))
import Data.Primitive.Types qualified as Prim (sizeOf)
import Foreign.ForeignPtr
import GHC.Exts (Addr#, Int(I#), copyAddrToByteArray#)
import Language.Haskell.TH.Syntax (Lift(liftTyped), TExp(TExp))
import Language.Haskell.TH qualified as TH

import Data.Vector qualified as V
import Data.Vector.Storable qualified as SV
import Foreign.Storable (Storable(sizeOf))
import System.IO.Unsafe (unsafePerformIO)

instance Lift a => Lift (Array a) where
  liftTyped arr = [|| arrayFromListN n as ||]
    where
      n  = sizeofArray arr
      as = toList arr

instance Prim a => Lift (PrimArray a) where
  liftTyped arr = do
    -- Copy the array into malloc'd memory so we can pass it to mkBytes
    bytesPtr <- liftIO do
      aPtr <- mallocForeignPtrBytes nBytes
      withForeignPtr aPtr \p ->
        copyPrimArrayToPtr p arr 0 nElems
      pure (castForeignPtr aPtr :: ForeignPtr Word8)

    let bytes = TH.mkBytes bytesPtr 0 (fromIntegral nBytes)
    let bytesExp :: TExp Addr# = TExp (TH.LitE (TH.bytesPrimL bytes))
    [|| copyAddrToNewPrimArray $$(pure bytesExp) nElems ||]
    where
      nElems = sizeofPrimArray arr
      nBytes = nElems * Prim.sizeOf (undefined :: a)

copyAddrToNewPrimArray :: forall a. Prim a => Addr# -> Int -> PrimArray a
copyAddrToNewPrimArray addr# nElems = runST do
  mutArr@(MutablePrimArray mutArr#) <- newPrimArray nElems
  primitive_ (copyAddrToByteArray# addr# mutArr# 0# nBytes#)
  unsafeFreezePrimArray mutArr
  where
    !(I# nBytes#) = nElems * Prim.sizeOf (undefined :: a)
{-# INLINE copyAddrToNewPrimArray #-}

instance Lift a => Lift (V.Vector a) where
  liftTyped vec = [|| V.fromListN n as ||]
    where
      n  = V.length vec
      as = V.toList vec

instance Storable a => Lift (SV.Vector a) where
  liftTyped vec = [|| unsafePerformIO do
    fp <- newForeignPtr_ (Ptr $$(pure bytesAddr))
    pure (SV.unsafeFromForeignPtr0 fp nElems) ||]
    where
      (aPtr, nElems) = SV.unsafeToForeignPtr0 vec

      bytesAddr :: TExp Addr#
      bytesAddr = TExp (TH.LitE (TH.bytesPrimL bytes))
      bytes = TH.mkBytes bytesPtr 0 (fromIntegral nBytes)
      bytesPtr = castForeignPtr aPtr :: ForeignPtr Word8
      nBytes = nElems * sizeOf (undefined :: a)

