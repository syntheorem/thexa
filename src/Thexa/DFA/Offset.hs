module Thexa.DFA.Offset
( DFA
, fromSimple
, toSimple
, step
, matches
) where

import PreludePrime

import Data.List (zip)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as SV
import Foreign.Storable (Storable)
import Language.Haskell.TH.Syntax (Lift)

import Thexa.DFA.Types
import Thexa.IntLike.Map qualified as ILMap
import Thexa.Orphans ()

newtype DFA ix = DFA (V.Vector (Entry ix))
  deriving (Lift)
  deriving newtype (NFData)

data Entry ix = Entry
  { matchSet :: !MatchSet
  , transOffset :: {-# UNPACK #-} !Int
  , transitions :: {-# UNPACK #-} !(SV.Vector ix)
  }
  deriving (Lift)

instance NFData (Entry ix) where
  rnf entry = entry `seq` ()

fromSimple :: (Storable ix, Integral ix) => SimpleDFA -> DFA ix
fromSimple vec = DFA (V.map mkEntry vec)
  where
    noMatch = fromIntegral (V.length vec)
    mkEntry (ms, transMap)
      | ILMap.null transMap = Entry
          { matchSet = ms
          , transOffset = 0
          , transitions = SV.empty
          }
      | otherwise = Entry
          { matchSet = ms
          , transOffset = minByte
          , transitions = SV.generate len \i ->
              case ILMap.lookup (fromIntegral (i + minByte)) transMap of
                Nothing       -> noMatch
                Just (Node n) -> fromIntegral n
          }
      where
        minByte = fromIntegral $ fst $ ILMap.findMin transMap
        maxByte = fromIntegral $ fst $ ILMap.findMax transMap
        len = (maxByte + 1) - minByte
{-# INLINABLE fromSimple #-}

toSimple :: (Storable ix, Integral ix) => DFA ix -> SimpleDFA
toSimple (DFA entries) = flip map entries \e ->
  (matchSet e, ILMap.fromDistinctAscList (transList e))
  where
    noMatch = fromIntegral (V.length entries)
    transList e = [ (fromIntegral b, Node (fromIntegral n))
                  | (b, n) <- zip [transOffset e..] (SV.toList (transitions e))
                  , n /= noMatch
                  ]
{-# INLINABLE toSimple #-}

step :: (Storable ix, Integral ix) => DFA ix -> Node -> Word8 -> Maybe Node
step (DFA vec) (Node i) b
  | bi < off || bi >= mx = Nothing
  | i' >= n              = Nothing
  | otherwise            = Just (Node i')
  where
    n   = V.length vec
    bi  = fromIntegral b
    mx  = off + SV.length ts
    ts  = transitions ent
    off = transOffset ent
    ent = (V.!) vec i
    i'  = fromIntegral (SV.unsafeIndex ts (bi - off))
{-# INLINABLE step #-}

matches :: DFA ix -> Node -> MatchSet
matches (DFA vec) (Node i) = matchSet ((V.!) vec i)
{-# INLINABLE matches #-}
