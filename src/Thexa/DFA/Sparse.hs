module Thexa.DFA.Sparse
( DFA
, fromSimple
, toSimple
, step
, matches
) where

import PreludePrime

import Data.Primitive.Array
import Data.Primitive.Types (Prim)
import Language.Haskell.TH.Syntax (Lift)

import Thexa.DFA.Types
import Thexa.IntLike.Map qualified as ILMap
import Thexa.Orphans ()
import Thexa.PrimMap (PrimMap)
import Thexa.PrimMap qualified as PrimMap

newtype DFA ix = DFA (Array (Entry ix))
  deriving (Lift)
  deriving newtype (NFData)

data Entry ix = Entry
  { matchSet :: !MatchSet
  , transitions :: {-# UNPACK #-} !(PrimMap Word8 ix)
  }
  deriving (Lift)

instance NFData (Entry ix) where
  rnf entry = entry `seq` ()

fromSimple :: (Prim ix, Integral ix) => SimpleDFA -> DFA ix
fromSimple arr = DFA $ flip mapArray' arr \(ms, bm) -> Entry
  { matchSet = ms
  , transitions = ILMap.toList bm
                & map (\(b, Node x) -> (b, fromIntegral x))
                & PrimMap.unsafeFromList (ILMap.size bm)
  }
{-# INLINABLE fromSimple #-}

toSimple :: (Prim ix, Integral ix) => DFA ix -> SimpleDFA
toSimple (DFA entries) = flip map entries \e ->
  (matchSet e, ILMap.fromDistinctAscList (transList e))
  where
    transList = map (\(b, i) -> (b, Node (fromIntegral i)))
              . PrimMap.toList . transitions
{-# INLINABLE toSimple #-}

step :: (Prim ix, Integral ix) => DFA ix -> Node -> Word8 -> Maybe Node
step (DFA arr) (Node i) b
  | i < 0 || i >= n = error "invalid node"
  | otherwise       = (Node . fromIntegral) <$> PrimMap.lookup b ts
  where
    n  = sizeofArray arr
    ts = transitions (indexArray arr i)
{-# INLINABLE step #-}

matches :: DFA ix -> Node -> MatchSet
matches (DFA arr) (Node i)
  | i < 0 || i >= n = error "invalid node"
  | otherwise       = matchSet (indexArray arr i)
  where
    n = sizeofArray arr
{-# INLINABLE matches #-}
