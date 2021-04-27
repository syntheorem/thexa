module Thexa.DFA.Types
( Node(Node)
, MatchKey
, MatchSet
, ByteMap
, SimpleDFA
) where

import PreludePrime

import Data.Vector (Vector)
import Thexa.IntLike.Class (IntLike)
import Thexa.NFA (MatchKey, MatchSet, ByteMap)

-- | A node (also called a state) of a DFA.
--
-- This is simply represented as an index into an array, so one must be careful to avoid mixing
-- nodes from multiple DFAs. Functions in this module will throw an exception if there is an attempt
-- to access a node with an index that is out of bounds.
newtype Node = Node Int
  deriving (Show)
  deriving newtype (Eq, Ord, IntLike, NFData)

-- | Simple but inefficient representation of a DFA. This is used as sort of an interchange format,
-- where we convert an NFA to a 'SimpleDFA' and then convert that to the actual DFA representation.
type SimpleDFA = Vector (MatchSet, ByteMap Node)
