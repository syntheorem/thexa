{-# OPTIONS_GHC -Wno-orphans #-}

-- | 'Lift' orphan instances. These aren't strictly necessary, but I really want to be able to
-- derive 'Lift' for types using vectors. Not just because it's convenient, but because it's less
-- error-prone than writing the instance manually.
module Thexa.Orphans () where

import PreludePrime

import Data.Primitive.Types (Ptr(Ptr))
import Foreign.ForeignPtr
import GHC.Exts (Addr#)
import Language.Haskell.TH.Syntax (Lift(liftTyped), TExp(TExp))
import Language.Haskell.TH qualified as TH

import Data.Vector qualified as V
import Data.Vector.Storable qualified as SV
import Foreign.Storable (Storable(sizeOf))
import System.IO.Unsafe (unsafePerformIO)

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

