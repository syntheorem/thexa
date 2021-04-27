module Thexa.DFA.Sparse
( DFA
, fromSimple
, toSimple
, step
, matches
) where

import PreludePrime

import Data.Primitive.Types (Prim)
import Data.Vector qualified as V
import Language.Haskell.TH.Syntax (Lift)

import Thexa.DFA.Types
import Thexa.IntLike.Map qualified as ILMap
import Thexa.Orphans ()
import Thexa.PrimMap (PrimMap)
import Thexa.PrimMap qualified as PrimMap

newtype DFA ix = DFA (V.Vector (Entry ix))
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
fromSimple vec = DFA $ flip V.map vec \(ms, bm) -> Entry
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
step (DFA vec) (Node i) b = (Node . fromIntegral) <$> PrimMap.lookup b ts
  where ts = transitions ((V.!) vec i)
{-# INLINABLE step #-}

matches :: DFA ix -> Node -> MatchSet
matches (DFA vec) (Node i) = matchSet ((V.!) vec i)
{-# INLINABLE matches #-}
