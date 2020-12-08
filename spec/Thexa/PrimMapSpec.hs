module Thexa.PrimMapSpec where

import PreludePrime

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Map (Map)
import Data.Map qualified as Map
import Thexa.PrimMap (PrimMap)
import Thexa.PrimMap qualified as PrimMap

spec :: Spec
spec = modifyMaxSuccess (max 10000) $ do
  mapProp "toList" \m pm -> Map.toList m === PrimMap.toList pm
  mapProp "lookup" \m pm k -> Map.lookup k m === PrimMap.lookup k pm

mapProp :: (HasCallStack, Testable prop) => String -> (Map Word8 Word8 -> PrimMap Word8 Word8 -> prop) -> Spec
mapProp name go = prop name $ \kvs -> go (Map.fromList kvs) (PrimMap.fromList kvs)
