module Thexa.DFA.Dense
( DFA
, fromSimple
, toSimple
, step
, matches
) where

import PreludePrime

import Control.Monad.ST (runST)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Vector.Storable qualified as SV
import Data.Vector.Storable.Mutable qualified as SMV
import Foreign.Storable (Storable)
import Language.Haskell.TH.Syntax (Lift)

import Thexa.DFA.Types
import Thexa.IntLike.Map qualified as ILMap
import Thexa.Orphans ()

data DFA ix = DFA
  { nodesVec  :: {-# UNPACK #-} !(SV.Vector ix)
  , matchVec :: {-# UNPACK #-} !(V.Vector MatchSet)
  }
  deriving (Lift)

instance NFData (DFA ix) where
  rnf (DFA _ vec) = rnf vec

fromSimple :: (Storable ix, Integral ix) => SimpleDFA -> DFA ix
fromSimple vec = runST do
  let n = V.length vec
  let noMatch = fromIntegral n

  nodesVec <- SMV.new (n * 256)
  matchVec <- MV.new n

  for_ [0..(n - 1)] \i -> do
    (ms, bm) <- V.indexM vec i
    MV.write matchVec i $! ms
    for_ [0..255] \b -> do
      let bi = 256*i + fromIntegral b
      case ILMap.lookup b bm of
        Just (Node ni) -> SMV.write nodesVec bi (fromIntegral ni)
        Nothing        -> SMV.write nodesVec bi noMatch

  DFA <$> SV.unsafeFreeze nodesVec <*> V.unsafeFreeze matchVec
{-# INLINABLE fromSimple #-}

toSimple :: (Storable ix, Integral ix) => DFA ix -> SimpleDFA
toSimple DFA{..} = runST do
  vec <- MV.new n

  for_ [0..(n - 1)] \i -> do
    let ts = filterMap (indexTrans i) [0..255]
    ms <- V.indexM matchVec i
    MV.write vec i (ms, ILMap.fromDistinctAscList ts)

  V.unsafeFreeze vec
  where
    n = V.length matchVec

    indexTrans :: Int -> Word8 -> Maybe (Word8, Node)
    indexTrans i b
      | i' >= n   = Nothing
      | otherwise = Just (b, Node i')
      where
        i' = fromIntegral ((SV.!) nodesVec bi)
        bi = 256*i + fromIntegral b
{-# INLINABLE toSimple #-}

step :: (Storable ix, Integral ix) => DFA ix -> Node -> Word8 -> Maybe Node
step DFA{..} (Node i) b
  | i' >= n         = Nothing
  | otherwise       = Just (Node i')
  where
    n  = V.length matchVec
    i' = fromIntegral ((SV.!) nodesVec (256*i + fromIntegral b))
{-# INLINABLE step #-}

matches :: DFA ix -> Node -> MatchSet
matches DFA{..} (Node i) = (V.!) matchVec i
{-# INLINABLE matches #-}
