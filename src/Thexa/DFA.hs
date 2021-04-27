-- TODO: after benchmarking, consider the following options:
-- 1. get rid of the whole IntLike thing, just use Map and Set instead
-- 2. don't bother with match arrays, just do NodeMap MatchSet instead
-- 3. get rid of Sparse, Offset seems to be better anyway
-- 4. change Offset transitions to use 2 PrimArrays for faster lifting
-- 5. use Storable Vector instead of PrimArray so we can use static pointers when lifting

module Thexa.DFA
( DFA(..)
, Node
, MatchKey
, MatchSet
, denseFromNFA
, offsetFromNFA
, sparseFromNFA
, startNode
, step
, matches
, prettyPrint

-- TODO: remove or flesh these out
, Stats(..)
, computeStats
) where

import PreludePrime

import Control.Monad.Reader (ReaderT, runReaderT, ask, asks, lift)
import Control.Monad.ST (ST, runST)
import Data.HashTable.ST.Basic qualified as HT
import Data.List (zip)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Foreign (Ptr, sizeOf)
import Language.Haskell.TH.Syntax (Lift)
import Numeric (showHex)

import Thexa.IntLike.Map qualified as ILMap
import Thexa.IntLike.Set qualified as ILSet
import Thexa.GrowVector qualified as GV
import Thexa.NFA (NFA)
import Thexa.NFA qualified as NFA

import Thexa.DFA.Types
import Thexa.DFA.Dense qualified as Dense
import Thexa.DFA.Offset qualified as Offset
import Thexa.DFA.Sparse qualified as Sparse

-- | Deterministic Finite Automaton with byte-labeled transitions.
--
-- A DFA is a set of nodes (a.k.a. states) where each node has a set of transitions. A transition
-- indicates which node we should go to next based on the next byte of input. Unlike in an NFA, each
-- transition maps to exactly one node, so the current state of a DFA is represented by a single
-- node rather than a set of them.
--
-- A DFA can be implemented as a big 2D array, where each node is an index into the first dimension
-- and the second dimension has 256 elements which give us the next node when indexed by the next
-- byte of input. I call this the *dense* representation, and it is very fast to simulate since
-- everything can be done as simple array indexing.
--
-- However, in the DFA generated for a typical lexer, most nodes don't have valid transitions for
-- every possible byte of input, and in fact likely only accept a small subset of possible bytes.
-- This means that the dense representation wastes a lot of space, which can be significant for a
-- large DFA. So there is also a *sparse* representation which essentially uses a map to lookup the
-- transitions for a node. This is somewhat slower but can potentially save a lot of memory,
-- although how much memory depends on the specific DFA.
--
-- TODO: describe offset representation
--
-- There is another optimization which is performed for all DFA representations. We probably don't
-- need the full range of an @Int@ to represent the nodes, so we save space by using an @Int16@ or
-- @Int32@, depending on the number of nodes in the DFA. There are no @Int8@ variants because at
-- that point the DFA is very small anyway, and no @Int64@ variants because at that point the DFA is
-- too large to reasonably fit into memory.
data DFA
  = Dense16 !(Dense.DFA Word16)
  | Dense32 !(Dense.DFA Word32)
  | Offset16 !(Offset.DFA Word16)
  | Offset32 !(Offset.DFA Word32)
  | Sparse16 !(Sparse.DFA Word16)
  | Sparse32 !(Sparse.DFA Word32)
  deriving (Lift, Generic, NFData)

-- | Construct a DFA from an NFA using the dense representation.
denseFromNFA :: NFA -> DFA
denseFromNFA nfa
  -- We need strictly less than because the dense DFA
  -- uses an extra index to represent invalid transitions.
  | n < maxNodes16 = Dense16 (Dense.fromSimple simple)
  | n < maxNodes32 = Dense32 (Dense.fromSimple simple)
  | otherwise      = error "too many nodes to fit in memory"
  where
    n = V.length simple
    simple = simpleFromNFA nfa

-- | Construct a DFA from an NFA using the offset representation.
offsetFromNFA :: NFA -> DFA
offsetFromNFA nfa
  -- We need strictly less than because the offset DFA
  -- uses an extra index to represent invalid transitions.
  | n < maxNodes16 = Offset16 (Offset.fromSimple simple)
  | n < maxNodes32 = Offset32 (Offset.fromSimple simple)
  | otherwise      = error "too many nodes to fit in memory"
  where
    n = V.length simple
    simple = simpleFromNFA nfa

-- | Construct a DFA from an NFA using the sparse representation.
sparseFromNFA :: NFA -> DFA
sparseFromNFA nfa
  | n <= maxNodes16 = Sparse16 (Sparse.fromSimple simple)
  | n <= maxNodes32 = Sparse32 (Sparse.fromSimple simple)
  | otherwise       = error "too many nodes to fit in memory"
  where
    n = V.length simple
    simple = simpleFromNFA nfa

-- | The start node which each DFA is initialized with.
startNode :: Node
startNode = Node 0

-- | Step the DFA simulation by feeding it the current node and the next byte of input. Returns the
-- new node, or 'Nothing' if the current node doesn't have a transition for the given byte,
-- indicating a failure to match the input.
step :: DFA -> Node -> Word8 -> Maybe Node
step = \case
  Dense16  dfa -> Dense.step  dfa
  Dense32  dfa -> Dense.step  dfa
  Offset16 dfa -> Offset.step dfa
  Offset32 dfa -> Offset.step dfa
  Sparse16 dfa -> Sparse.step dfa
  Sparse32 dfa -> Sparse.step dfa
{-# INLINE step #-}

-- | Lookup the matches for the given DFA node. The returned set is empty when the node is not a
-- match node.
matches :: DFA -> Node -> MatchSet
matches = \case
  Dense16  dfa -> Dense.matches  dfa
  Dense32  dfa -> Dense.matches  dfa
  Offset16 dfa -> Offset.matches dfa
  Offset32 dfa -> Offset.matches dfa
  Sparse16 dfa -> Sparse.matches dfa
  Sparse32 dfa -> Sparse.matches dfa
{-# INLINE matches #-}

maxNodes16 :: Int
maxNodes16 = maxNodes @Word16

maxNodes32 :: Int
maxNodes32 = maxNodes @Word32

-- | The maximum number of representable nodes for the given unsigned index type.
--
-- This works by taking @maxBound :: Int@, converting it to the given type, then converting back.
-- This way, if @Int@ has a smaller bound, it will be losslessly converted and we'll get the @Int@
-- bound. If it has a larger bound, then it will be truncated to the bound for the given type.
--
-- Basically, the point is that it will work regardless of the number of bits in an @Int@.
maxNodes :: forall a. Integral a => Int
maxNodes = fromIntegral (fromIntegral (maxBound @Int) :: a)

------------------------
-- Building SimpleDFA --
------------------------

-- | Monad in which we build the DFA.
type Build s = ReaderT (BuildState s) (ST s)

data BuildState s = BS
  { bsNodeArray :: {-# UNPACK #-} !(GV.GrowVector s (Either NFA.NodeSet (MatchSet, ByteMap Node)))
  -- ^ Array of DFA nodes. When we first create a node, we store the set of NFA nodes that it
  -- represents. Then we eventually process each node to compute its match set and its transitions
  -- to other DFA nodes, so that when we're done every element of the array is 'Right'.
  , bsNodeTable :: {-# UNPACK #-} !(HT.HashTable s NFA.NodeSet Node)
  -- ^ Mapping from a set of NFA nodes to its corresponding DFA node. Since comparison of sets is
  -- relatively expensive, my intuition is that a hash table will perform better than an ordered
  -- map. However, benchmarks are needed to determine the optimal data structure here.
  }

simpleFromNFA :: NFA -> SimpleDFA
simpleFromNFA nfa = runST do
  bs <- BS <$> GV.new <*> HT.new

  flip runReaderT bs do
    _ <- lookupNode (NFA.startNodes nfa)
    processNodes 0 nfa

  let nodes = bsNodeArray bs
  len <- GV.length nodes
  vec <- MV.new len

  for_ [0..len-1] \i -> do
    GV.read nodes i >>= \case
      Left  _ -> error "somehow failed to process a node"
      Right x -> MV.write vec i x

  V.unsafeFreeze vec

-- | Traverse the node array, computing the transitions for each node. When computing the
-- transitions we may create new nodes and add them to the array, so we keep going until there are
-- no unprocessed nodes left.
processNodes :: Int -> NFA -> Build s ()
processNodes i nfa = do
  vec <- asks bsNodeArray
  len <- GV.length vec

  unless (i >= len) do
    GV.read vec i >>= \case
      Right _    -> error "somehow reached already processed node"
      Left nodes -> do
        let trans = NFA.stepAll nfa nodes
        trans' <- traverse lookupNode trans
        GV.write vec i (Right (NFA.matches nfa nodes, trans'))
        processNodes (i + 1) nfa

-- | Lookup the node corresponding to a set of NFA nodes, or create it if it doesn't exist.
lookupNode :: NFA.NodeSet -> Build s Node
lookupNode nodes = do
  BS{bsNodeArray = vec, bsNodeTable = tbl} <- ask
  lift $ HT.mutateST tbl nodes \case
    Just n  -> pure (Just n, n)
    Nothing -> do
      n <- Node <$> GV.length vec
      GV.push vec (Left nodes)
      pure (Just n, n)

---------------
-- Debugging --
---------------

-- | Pretty-print a human-readable description of the DFA structure for debugging purposes.
prettyPrint :: DFA -> String
prettyPrint = foldMap ppNode . zip [0..] . toList . toSimple
  where
    ppNode :: (Int, (MatchSet, ByteMap Node)) -> String
    ppNode (i, (ms, ts))
      | ILSet.null ms = prefix <> transStr
      | otherwise     = prefix <> matchStr <> transStr
      where
        prefix = show i
        matchStr = "\t: MatchSet    " <> show (ILSet.toList ms) <> "\n"
        transStr = "\t: Transitions " <> show (map ppTrans (ILMap.toList ts)) <> "\n"

    ppTrans :: (Word8, Node) -> String
    ppTrans (b, Node i) = "0x"<>pad<>bHex<>" => "<>show i
      where
        pad  = if length bHex < 2 then "0" else ""
        bHex = showHex b ""

toSimple :: DFA -> SimpleDFA
toSimple = \case
  Dense16  dfa -> Dense.toSimple  dfa
  Dense32  dfa -> Dense.toSimple  dfa
  Offset16 dfa -> Offset.toSimple dfa
  Offset32 dfa -> Offset.toSimple dfa
  Sparse16 dfa -> Sparse.toSimple dfa
  Sparse32 dfa -> Sparse.toSimple dfa

data Stats = Stats
  { statNodeCount :: Int
  -- ^ Number of nodes in the DFA.
  , statBytesPerNodeIndexSparse :: Int
  -- ^ Number of bytes used to represent a node in the sparse representation.
  , statBytesPerNodeIndexDense :: Int
  -- ^
  , statAvgTransitionsPerNode :: Double
  , statMatchNodeCount :: Int
  , statAvgMatchesPerMatchNode :: Double
  , statSizeOfSparse :: Int
  , statSizeOfDense :: Int
  }
  deriving (Show)

computeStats :: DFA -> Stats
computeStats (toSimple -> dfa) = Stats
  { statNodeCount = n
  , statBytesPerNodeIndexSparse = sparseIxBytes
  , statBytesPerNodeIndexDense = denseIxBytes
  , statAvgTransitionsPerNode = avg (map ILMap.size transitions)
  , statMatchNodeCount = length matches
  , statAvgMatchesPerMatchNode = avg (map ILSet.size matches)
  , statSizeOfSparse = sparseSize
  , statSizeOfDense = denseSize
  }
  where
    n = V.length dfa
    sparseIxBytes
      | n <= maxNodes16 = 2
      | n <= maxNodes32 = 4
      | otherwise       = error "too many nodes to fit in memory"
    denseIxBytes
      | n < maxNodes16 = 2
      | n < maxNodes32 = 4
      | otherwise      = error "too many nodes to fit in memory"

    matches = filter (not . ILSet.null) $ map fst $ toList dfa
    transitions = map snd $ toList dfa

    avg :: Real a => [a] -> Double
    avg [] = 0
    avg as = foldl' (\s a -> s + realToFrac a) 0 as / realToFrac (length as)

    -- TODO: calculate size via compact regions?
    ptrSize = sizeOf (undefined :: Ptr Int)
    denseSize = n * denseIxBytes * 256 -- node array contents
              + 2 * ptrSize -- node array overhead
              + n * ptrSize -- match array contents
              + 2 * ptrSize -- match array overhead (excluding card table)
              + 2 * ptrSize -- pointers to the arrays
    sparseSize = n * 3 * ptrSize -- per-node overhead (ptrs to match set and PrimMap arrays)
               + n * 4 * ptrSize -- array overhead for transition PrimMaps
               + (1 + sparseIxBytes) * sum (map ILMap.size transitions) -- transition map contents
               + 2 * ptrSize -- array overhead (excluding card table)
