{-# OPTIONS_GHC -Wno-orphans #-}

-- | 'Lift' orphan instances. These aren't strictly necessary, but I really want to be able to
-- derive 'Lift' for types using arrays. Not just because it's convenient, but because it's less
-- error-prone than writing the instance manually.
module Thexa.Orphans where

import PreludePrime

import Data.Primitive.Array
import Data.Primitive.PrimArray
import Data.Primitive.Types (Prim)
import Language.Haskell.TH.Syntax (Lift(liftTyped))

instance Lift a => Lift (Array a) where
  liftTyped arr = [|| arrayFromListN n as ||]
    where
      n  = sizeofArray arr
      as = toList arr

instance (Prim a, Lift a) => Lift (PrimArray a) where
  liftTyped arr = [|| primArrayFromListN n as ||]
    where
      n  = sizeofPrimArray arr
      as = primArrayToList arr
