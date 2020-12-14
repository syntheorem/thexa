module Thexa.DFA.Offset
( DFA
, fromSimple
, toSimple
, step
, matches
) where

import PreludePrime

import Data.List (zip)
import Data.Primitive.Array
import Data.Primitive.PrimArray
import Data.Primitive.Types (Prim)
import Language.Haskell.TH.Syntax (Lift)

import Thexa.DFA.Types
import Thexa.IntLike.Map qualified as ILMap
import Thexa.Orphans ()

newtype DFA ix = DFA (Array (Entry ix))
  deriving (Lift)
  deriving newtype (NFData)

data Entry ix = Entry
  { matchSet :: !MatchSet
  , transOffset :: {-# UNPACK #-} !Int
  , transitions :: {-# UNPACK #-} !(PrimArray ix)
  }
  deriving (Lift)

instance NFData (Entry ix) where
  rnf entry = entry `seq` ()

fromSimple :: (Prim ix, Integral ix) => SimpleDFA -> DFA ix
fromSimple arr = DFA (mapArray' mkEntry arr)
  where
    noMatch = fromIntegral (sizeofArray arr)
    mkEntry (ms, transMap)
      | ILMap.null transMap = Entry
          { matchSet = ms
          , transOffset = 0
          , transitions = primArrayFromList []
          }
      | otherwise = Entry
          { matchSet = ms
          , transOffset = minByte
          , transitions = generatePrimArray len \i ->
              case ILMap.lookup (fromIntegral (i + minByte)) transMap of
                Nothing       -> noMatch
                Just (Node n) -> fromIntegral n
          }
      where
        minByte = fromIntegral $ fst $ ILMap.findMin transMap
        maxByte = fromIntegral $ fst $ ILMap.findMax transMap
        len = (maxByte + 1) - minByte
{-# INLINABLE fromSimple #-}

toSimple :: (Prim ix, Integral ix) => DFA ix -> SimpleDFA
toSimple (DFA entries) = flip map entries \e ->
  (matchSet e, ILMap.fromDistinctAscList (transList e))
  where
    noMatch = fromIntegral (sizeofArray entries)
    transList e = [ (fromIntegral b, Node (fromIntegral n))
                  | (b, n) <- zip [transOffset e..] (primArrayToList (transitions e))
                  , n /= noMatch
                  ]
{-# INLINABLE toSimple #-}

step :: (Prim ix, Integral ix) => DFA ix -> Node -> Word8 -> Maybe Node
step (DFA arr) (Node i) b
  | i  < 0   || i  >= n  = error "invalid node"
  | bi < off || bi >= mx = Nothing
  | i' >= n              = Nothing
  | otherwise            = Just (Node i')
  where
    n   = sizeofArray arr
    bi  = fromIntegral b
    mx  = off + sizeofPrimArray ts
    ts  = transitions ent
    off = transOffset ent
    ent = indexArray arr i
    i'  = fromIntegral (indexPrimArray ts (bi - off))
{-# INLINE step #-}

matches :: DFA ix -> Node -> MatchSet
matches (DFA arr) (Node i)
  | i < 0 || i >= n = error "invalid node"
  | otherwise       = matchSet (indexArray arr i)
  where
    n = sizeofArray arr
{-# INLINE matches #-}
