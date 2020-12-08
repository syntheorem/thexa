module Thexa.PrimMapBench where

import PreludePrime
import Criterion

import Data.IntMap qualified as IntMap
import Data.Map qualified as Map
import Data.Primitive.Array qualified as A
import Thexa.PrimMap qualified as PrimMap

-- Very simple benchmark of lookup performance between the three map types. Simply populates the map
-- with the given list of keys and benchmarks looking up all of them in order.
benchmarks :: [Benchmark]
benchmarks =
  [ bgroup "16"   $ benchKeys [0..15]
  , bgroup "256"  $ benchKeys [0..255]
  , bgroup "4096" $ benchKeys [0..4095]
  ]

benchKeys :: [Int] -> [Benchmark]
benchKeys ks =
  [ env (pure $ PrimMap.fromList kvs) \pm ->
      bench "PrimMap.lookup" $
        nf (map (flip PrimMap.lookup pm)) ks
  , env (pure $ IntMap.fromList kvs) \im ->
      bench "IntMap.lookup" $
        nf (map (flip IntMap.lookup im)) ks
  , env (pure $ Map.fromList kvs) \m ->
      bench "Map.lookup" $
        nf (map (flip Map.lookup m)) ks
  , env (pure $ A.fromList ks) \arr ->
      bench "indexArray" $
        nf (map (A.indexArray arr)) ks
  ]
  where
    kvs = map (\k -> (k, k)) ks
