module Thexa.DFA.Dense
( DFA
, fromSimple
, step
, matches
, toSimple
) where

import PreludePrime

import Control.Monad.ST (runST)
import Data.Primitive.Array
import Data.Primitive.PrimArray
import Data.Primitive.Types (Prim)
import Language.Haskell.TH.Syntax (Lift(liftTyped))

import Thexa.DFA.Types
import Thexa.IntLike.Map qualified as ILMap

data DFA ix = DFA
  { nodeArr  :: {-# UNPACK #-} !(PrimArray ix)
  , matchArr :: {-# UNPACK #-} !(Array MatchSet)
  }

instance (Lift ix, Prim ix) => Lift (DFA ix) where
  liftTyped DFA{..} = [|| DFA (primArrayFromListN nsLen ns) (arrayFromListN msLen ms) ||]
    where
      ns = primArrayToList nodeArr
      ms = toList matchArr
      nsLen = sizeofPrimArray nodeArr
      msLen = sizeofArray matchArr

instance NFData (DFA ix) where
  rnf (DFA _ arr) = rnf arr

fromSimple :: (Prim ix, Integral ix) => SimpleDFA -> DFA ix
fromSimple arr = runST do
  let n = sizeofArray arr
  let noMatch = fromIntegral n

  nodesArr <- newPrimArray (n * 256)
  matchArr <- newArray n (error "uninitialized array element")

  for_ [0..(n - 1)] \i -> do
    let (ms, bm) = indexArray arr i
    writeArray matchArr i ms
    for_ [0..255] \b -> do
      let bi = 256*i + fromIntegral b
      case ILMap.lookup b bm of
        Just (Node ni) -> writePrimArray nodesArr bi (fromIntegral ni)
        Nothing        -> writePrimArray nodesArr bi noMatch

  DFA <$> unsafeFreezePrimArray nodesArr <*> unsafeFreezeArray matchArr
{-# INLINABLE fromSimple #-}

toSimple :: (Prim ix, Integral ix) => DFA ix -> SimpleDFA
toSimple DFA{..} = runST do
  arr <- newArray n (error "uninitialized array element")

  for_ [0..(n - 1)] \i -> do
    let ms = indexArray matchArr i
    let ts = filterMap (indexTrans i) [0..255]
    writeArray arr i (ms, ILMap.fromDistinctAscList ts)

  unsafeFreezeArray arr
  where
    n = sizeofArray matchArr

    indexTrans :: Int -> Word8 -> Maybe (Word8, Node)
    indexTrans i b
      | i' >= n   = Nothing
      | otherwise = Just (b, Node i')
      where
        i' = fromIntegral (indexPrimArray nodeArr bi)
        bi = 256*i + fromIntegral b
{-# INLINABLE toSimple #-}

step :: (Prim ix, Integral ix) => DFA ix -> Node -> Word8 -> Maybe Node
step DFA{..} (Node i) b
  | i < 0 || i >= n = error "invalid node"
  | i' >= n         = Nothing
  | otherwise       = Just (Node i')
  where
    n  = sizeofArray matchArr
    i' = fromIntegral (indexPrimArray nodeArr (256*i + fromIntegral b))
{-# INLINE step #-}

matches :: DFA ix -> Node -> MatchSet
matches DFA{..} (Node i)
  | i < 0 || i >= n = error "invalid node"
  | otherwise       = indexArray matchArr i
  where
    n = sizeofArray matchArr
{-# INLINE matches #-}
