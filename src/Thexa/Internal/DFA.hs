{-# OPTIONS_GHC -Wno-partial-type-signatures -Wno-missing-signatures #-}

-- TODO: after benchmarking, consider the following options:
-- 1. get rid of the whole IntLike thing, just use Map and Set instead
-- 2. use NodeMap MatchSet or Vector MatchSet?

module Thexa.Internal.DFA
( DFA
, Node
, MatchKey
, MatchSet
, fromNFA
, startNode
, step
, matches
, isMatchNode

-- * Representation of Transitions
, Transitions
, Dense
, Dense16
, Dense32
, Sparse
, Sparse16
, Sparse32

-- * Debugging
, prettyPrint
, Stats(..)
, computeStats
) where

import PreludePrime

import Control.Monad.Reader (ReaderT, runReaderT, ask, asks, lift)
import Control.Monad.ST (ST, runST)
import Data.HashTable.ST.Basic qualified as HT
import Data.List (zip)
import Data.Primitive.MutVar
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Vector.Storable qualified as SV
import Data.Vector.Storable.Mutable qualified as SMV
import Foreign.Storable (Storable(sizeOf, alignment, peek, poke, peekByteOff, pokeByteOff))
import Numeric (showHex)

-- imports for lifting storable vectors
import Data.Primitive.Types (Ptr(Ptr))
import Foreign.ForeignPtr
import GHC.Exts (Addr#)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Lift(liftTyped))
import Language.Haskell.TH.Syntax.Compat (unsafeSpliceCoerce)
import System.IO.Unsafe (unsafePerformIO)

import Thexa.Internal.IntLike.Class (IntLike)
import Thexa.Internal.IntLike.Map qualified as ILMap
import Thexa.Internal.IntLike.Set qualified as ILSet
import Thexa.Internal.GrowVector qualified as GV
import Thexa.Internal.NFA (NFA, MatchKey, MatchSet, ByteMap)
import Thexa.Internal.NFA qualified as NFA

-- | Deterministic Finite Automaton with byte-labeled transitions.
--
-- A DFA is a set of nodes (a.k.a. states) where each node has a set of transitions. A transition
-- indicates which node we should go to next based on the next byte of input. Unlike in an 'NFA', each
-- transition maps to exactly one node, so the current state of a DFA is represented by a single
-- node rather than a set of them.
--
-- A DFA can be implemented as a big 2D array, where each node is an index into the first dimension
-- and the second dimension has 256 elements which give us the next node when indexed by the next
-- byte of input. I call this the 'Dense' representation, and it is very fast to simulate since
-- everything can be done as simple array indexing.
--
-- However, in the DFA generated for a typical lexer, most nodes don't have valid transitions for
-- every possible byte of input, and in fact likely only accept a small subset of possible bytes.
-- This means that the dense representation wastes a lot of space, which can be significant for a
-- large DFA. So there is also a 'Sparse' representation which essentially uses an additional level
-- of indirection to avoid storing the transition for every possible byte. This is somewhat slower
-- but can potentially save a lot of memory, although how much memory depends on the specific DFA.
--
-- There is another optimization which is available for both DFA representations. We probably don't
-- need the full range of an 'Int' to represent the node indices, so we can save space by using a
-- 'Word16' or 'Word32', depending on the number of nodes in the DFA. There are no 'Word8' variants
-- because at that point the DFA is very small anyway, and no 'Word64' variants because at that
-- point the DFA is too large to reasonably fit into memory.
--
-- This choice of representations is enabled by @DFA@'s type parameter, which should be an instance
-- of the 'Transitions' class. Although this forces the representation to be selected at compile
-- time, it allows the compiler to specialize the 'step' function to the specific representation
-- used, which is important for performance. The main difficulty is not knowing whether the DFA has
-- few enough nodes to use 'Word16' indices, which may require trial and error to determine.
data DFA t = DFA !t !MatchNodes
  deriving (Lift)

instance NFData t => NFData (DFA t) where
  rnf (DFA t matchNodes) = rnf t `seq` rnf matchNodes

-- | A node (also called a state) of a DFA.
--
-- This is simply represented as an index into an array, so one must be careful to avoid mixing
-- nodes from multiple DFAs. Functions in this module will throw an exception if there is an attempt
-- to access a node with an index that is out of bounds.
newtype Node = Node Int
  deriving (Show)
  deriving newtype (Eq, Ord, IntLike, NFData)

-- | Map of nodes to their non-empty match sets.
type MatchNodes = ILMap.ILMap Node MatchSet

-- | Convert an 'NFA' to a 'DFA'.
fromNFA :: Transitions t => NFA -> DFA t
fromNFA nfa = DFA trans matchNodes
  where
    (simple, matchNodes) = simpleFromNFA nfa
    trans = transFromSimple simple

-- | The start node which each DFA is initialized with.
startNode :: Node
startNode = Node 0

-- | Step the DFA simulation by feeding it the current node and the next byte of input. Returns the
-- new node, or 'Nothing' if the current node doesn't have a transition for the given byte,
-- indicating a failure to match the input.
step :: Transitions t => DFA t -> Node -> Word8 -> Maybe Node
step (DFA t _) = transStep t
{-# INLINE step #-}

-- | Lookup the matches for the given DFA node. The returned set is empty when the node is not a
-- match node.
matches :: DFA t -> Node -> MatchSet
matches (DFA _ matchNodes) = fromMaybe ILSet.empty . flip ILMap.lookup matchNodes

-- | Check if the given node is a match node.
isMatchNode :: DFA t -> Node -> Bool
isMatchNode (DFA _ matchNodes) = flip ILMap.member matchNodes

-- | Simple but inefficient representation of DFA transitions. This is used as sort of an
-- interchange format, where we convert an NFA to this and then convert that to the actual DFA
-- representation.
type Simple = V.Vector (ByteMap Node)

-- | Class for types which can be used to represent 'DFA' transitions.
class Transitions t where
  transFromSimple :: Simple -> t
  transToSimple :: t -> Simple
  transStep :: t -> Node -> Word8 -> Maybe Node
  transSize :: t -> Int

---------------------------------
-- Building Simple Transitions --
---------------------------------

-- | Monad in which we build the DFA.
type Build s = ReaderT (BuildState s) (ST s)

data BuildState s = BS
  { bsNodeArray :: {-# UNPACK #-} !(GV.GrowVector s (Either NFA.NodeSet (ByteMap Node)))
  -- ^ Array of DFA nodes. When we first create a node, we store the set of NFA nodes that it
  -- represents. Then we eventually process each node to compute its match set and its transitions
  -- to other DFA nodes, so that when we're done every element of the array is 'Right'.
  , bsNodeTable :: {-# UNPACK #-} !(HT.HashTable s NFA.NodeSet Node)
  -- ^ Mapping from a set of NFA nodes to its corresponding DFA node. Since comparison of sets is
  -- relatively expensive, my intuition is that a hash table will perform better than an ordered
  -- map. However, benchmarks are needed to determine the optimal data structure here.
  , bsMatchNodes :: {-# UNPACK #-} !(MutVar s MatchNodes)
  -- ^ Map of match nodes to their respective MatchSets.
  }

simpleFromNFA :: NFA -> (Simple, MatchNodes)
simpleFromNFA nfa = runST do
  -- Initialize the BuildState
  bs <- BS <$> GV.new <*> HT.new <*> newMutVar ILMap.empty

  flip runReaderT bs do
    _ <- lookupNode (NFA.startNodes nfa)
    processNodes 0 nfa

  let nodes = bsNodeArray bs
  len <- GV.length nodes
  vec <- MV.new len

  -- Write node transitions into the new array, removing the Either constructor
  for_ [0..len-1] \i -> do
    GV.read nodes i >>= \case
      Left  _ -> error "somehow failed to process a node"
      Right x -> MV.write vec i x

  trans <- V.unsafeFreeze vec
  matchNodes <- readMutVar (bsMatchNodes bs)
  pure (trans, matchNodes)

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
        -- Compute transitions for this node
        let trans = NFA.stepAll nfa nodes
        trans' <- traverse lookupNode trans
        GV.write vec i (Right trans')

        -- Add this node as a match node if applicable
        let matchSet = NFA.matches nfa nodes
        unless (ILSet.null matchSet) do
          matchNodesVar <- asks bsMatchNodes
          modifyMutVar' matchNodesVar (ILMap.insert (Node i) matchSet)

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

-----------------------
-- Dense Transitions --
-----------------------

-- | Dense 'DFA' transitions representation.
newtype Dense ix = Dense (SV.Vector ix)
  deriving newtype (NFData)

-- | Dense 'DFA' transitions using 'Word16' indicies.
type Dense16 = Dense Word16

-- | Dense 'DFA' transitions using 'Word32' indicies.
type Dense32 = Dense Word32

instance Transitions Dense16 where
  transToSimple = denseToSimple
  transStep = denseStep
  transSize = denseSize

  transFromSimple simpleVec
    | n <= 0xFFFF = denseFromSimple simpleVec
    | otherwise   = error "too many nodes to use Word16 indices"
    where n = V.length simpleVec

instance Transitions Dense32 where
  transToSimple = denseToSimple
  transStep = denseStep
  transSize = denseSize

  transFromSimple simpleVec
    | n <= 0x7FFFFFFF = denseFromSimple simpleVec
    | otherwise       = error "too many nodes to use Word32 indices"
    where n = V.length simpleVec

{-# INLINABLE denseFromSimple #-}
denseFromSimple :: (Storable ix, Integral ix) => Simple -> Dense ix
denseFromSimple simpleVec = runST do
  let n = V.length simpleVec
  let noMatch = fromIntegral n
  denseVec <- SMV.new (n * 256)

  for_ [0..(n - 1)] \i -> do
    bm <- V.indexM simpleVec i
    for_ [0..255] \b -> do
      let bi = 256*i + fromIntegral b
      case ILMap.lookup b bm of
        Just (Node ni) -> SMV.write denseVec bi (fromIntegral ni)
        Nothing        -> SMV.write denseVec bi noMatch

  Dense <$> SV.unsafeFreeze denseVec

{-# INLINABLE denseToSimple #-}
denseToSimple :: (Storable ix, Integral ix) => Dense ix -> Simple
denseToSimple (Dense denseVec) = runST do
  vec <- MV.new n

  for_ [0..(n - 1)] \i -> do
    let ts = filterMap (indexTrans i) [0..255]
    MV.write vec i (ILMap.fromDistinctAscList ts)

  V.unsafeFreeze vec
  where
    n = SV.length denseVec `div` 256

    indexTrans :: Int -> Word8 -> Maybe (Word8, Node)
    indexTrans i b
      | i' >= n   = Nothing
      | otherwise = Just (b, Node i')
      where
        i' = fromIntegral ((SV.!) denseVec bi)
        bi = 256*i + fromIntegral b

{-# INLINABLE denseStep #-}
denseStep :: (Storable ix, Integral ix) => Dense ix -> Node -> Word8 -> Maybe Node
denseStep (Dense denseVec) (Node i) b
  | i' >= n         = Nothing
  | otherwise       = Just (Node i')
  where
    n  = SV.length denseVec `div` 256
    i' = fromIntegral ((SV.!) denseVec (256*i + fromIntegral b))

denseSize :: forall ix. Storable ix => Dense ix -> Int
denseSize (Dense denseVec) = svSizeInBytes denseVec

------------------------
-- Sparse Transitions --
------------------------

-- | Sparse 'DFA' transitions representation.
data Sparse ix = Sparse
  !(SV.Vector SparseNode)
  -- ^ One entry for each node of the DFA. Each entry contains the information needed to lookup
  -- transitions for that node in the transition vector.
  --
  -- Although using a storable vector should provide performance benefits, the primary reason is to
  -- use the 'Lift' instance for storable vectors, which embeds the bytes directly into the program
  -- binary rather than syntactically construct a large array which the compiler will then spend far
  -- too much time attempting to optimize.
  !(SV.Vector ix)
  -- ^ Transition vector. Constructed by taking the dense transitions for each node (i.e., the
  -- 256-element array with the transition for each byte) and trimming the invalid transitions off
  -- the beginning and end. For example, if a node has valid transitions for 3 and 10, then we will
  -- store 8 transitions (from 3 to 10) but 6 of them (from 4 to 9) will be marked as invalid.
  --
  -- We then concatenate these trimmed transition arrays into a single vector. So to lookup a
  -- transition for a node, we need the offset of its transition array in this vector, the number of
  -- elements trimmed off the start, and the length of the vector.

-- | Sparse 'DFA' transitions using 'Word16' indicies.
type Sparse16 = Sparse Word16

-- | Sparse 'DFA' transitions using 'Word32' indicies.
type Sparse32 = Sparse Word32

instance NFData (Sparse ix) where
  rnf (Sparse v0 v1) = rnf v0 `seq` rnf v1

-- | Node metadata telling us how to lookup the transitions. The fields are all 'Int', but this is
-- essentially a lie because we actually store them as smaller 'Word' types. I originally had their
-- types be accurate, but frankly all the integral type conversions added a lot of noise to the
-- code, so instead I'm just doing the conversions in the 'Storable' instance.
data SparseNode = SparseNode
  {-# UNPACK #-} !Int
  -- ^ Word32. Sparse of this node's transitions in the transition vector.
  {-# UNPACK #-} !Int
  -- ^ Word16. The number of transitions stored for this node. May be 0 for no valid transitions.
  {-# UNPACK #-} !Int
  -- ^ Word16. The number of invalid transitions that were trimmed off the front. This value is
  -- irrelevant if the number of stored transitions is 0.

instance Storable SparseNode where
  sizeOf    _ = 8
  alignment _ = 4

  poke ptr (SparseNode x0 x1 x2) = do
    pokeByteOff @Word32 ptr 0 (fromIntegral x0)
    pokeByteOff @Word16 ptr 4 (fromIntegral x1)
    pokeByteOff @Word16 ptr 6 (fromIntegral x2)

  peek ptr = do
    x0 <- fromIntegral <$> peekByteOff @Word32 ptr 0
    x1 <- fromIntegral <$> peekByteOff @Word16 ptr 4
    x2 <- fromIntegral <$> peekByteOff @Word16 ptr 6
    pure (SparseNode x0 x1 x2)

instance Transitions Sparse16 where
  transToSimple = sparseToSimple
  transStep = sparseStep
  transSize = sparseSize

  transFromSimple simpleVec
    | n <= 0xFFFF = sparseFromSimple simpleVec
    | otherwise   = error "too many nodes to use Word16 indices"
    where n = V.length simpleVec

instance Transitions Sparse32 where
  transToSimple = sparseToSimple
  transStep = sparseStep
  transSize = sparseSize

  transFromSimple simpleVec
    | n <= 0x7FFFFFFF = sparseFromSimple simpleVec
    | otherwise       = error "too many nodes to use Word32 indices"
    where n = V.length simpleVec

{-# INLINABLE sparseFromSimple #-}
sparseFromSimple :: (Storable ix, Integral ix) => Simple -> Sparse ix
sparseFromSimple simpleVec = runST do
  let nNodes = V.length simpleVec
  let noMatch = fromIntegral nNodes
  nodesVec <- SMV.new nNodes

  -- We fold over the nodes twice. The first time, we determine the length of the trimmed transition
  -- array for each node, which we use to populate nodesVec and accumulate the length of the final
  -- transition vector.
  let go1 :: Int -> Int -> ByteMap Node -> ST _ Int
      go1 transVecLen nodeIdx transMap = do
        let (transLen, byteOff) = transLengthAndOffset transMap
        SMV.write nodesVec nodeIdx $ SparseNode transVecLen transLen byteOff
        pure (transVecLen + transLen)

  transVecLen <- V.ifoldM' go1 0 simpleVec

  when (transVecLen > 0x7FFFFFFF) do
    errorM "too many transitions for Word32 offsets"

  transVec <- SMV.new transVecLen

  -- For the second fold, we populate the transition vector with the actual transitions for each
  -- node, again accumulating the length of the transition vector so far.
  let go2 :: Int -> ByteMap Node -> ST _ Int
      go2 transVecIdx transMap = do
        let (transLen, byteOff) = transLengthAndOffset transMap

        for_ [0..transLen-1] \i -> do
          let b = fromIntegral (i + byteOff) :: Word8
          let ix = case ILMap.lookup b transMap of
                Just (Node n) -> fromIntegral n
                Nothing -> noMatch

          SMV.write transVec (transVecIdx + i) ix

        pure (transVecIdx + transLen)

  V.foldM'_ go2 0 simpleVec

  Sparse <$> SV.unsafeFreeze nodesVec
         <*> SV.unsafeFreeze transVec
  where
    transLengthAndOffset :: ByteMap Node -> (Int, Int)
    transLengthAndOffset transMap
      | ILMap.null transMap = (0, 0)
      | otherwise           = (maxByte - minByte + 1, minByte)
      where
        minByte = fromIntegral (fst (ILMap.findMin transMap)) :: Int
        maxByte = fromIntegral (fst (ILMap.findMax transMap)) :: Int

{-# INLINABLE sparseStep #-}
sparseStep :: (Storable ix, Integral ix) => Sparse ix -> Node -> Word8 -> Maybe Node
sparseStep (Sparse nodesVec transVec) (Node nodeIdx) b
  -- This node has no valid transitions
  | transLen == 0        = Nothing
  -- Transition index must be in bounds to be valid
  | transIdx <  0        = Nothing
  | transIdx >= transLen = Nothing
  -- Check that the stored transition is valid
  | nodeIdx' >= nNodes   = Nothing
  -- Otherwise return the new node
  | otherwise            = Just (Node nodeIdx')
  where
    byte = fromIntegral b :: Int
    nNodes = SV.length nodesVec

    -- Lookup sparse node info
    !(SparseNode transStart transLen byteOff) = (SV.!) nodesVec nodeIdx

    -- Lookup transition
    -- TODO: could use unsafeIndex
    transIdx = byte - byteOff
    nodeIdx' = fromIntegral ((SV.!) transVec (transStart + transIdx)) :: Int

{-# INLINABLE sparseToSimple #-}
sparseToSimple :: (Storable ix, Integral ix) => Sparse ix -> Simple
sparseToSimple (Sparse nodesVec transVec) = V.map toTransMap (V.convert nodesVec)
  where
    nNodes = SV.length nodesVec

    toTransMap :: SparseNode -> ByteMap Node
    toTransMap (SparseNode transStart transLen byteOff) = ILMap.fromList
      [ (fromIntegral byte, Node (fromIntegral ix))
      | (byte, ix) <- zip [byteOff..] $ SV.toList $ SV.slice transStart transLen transVec
      , fromIntegral ix < nNodes
      ]

sparseSize :: forall ix. Storable ix => Sparse ix -> Int
sparseSize (Sparse nodesVec transVec) = svSizeInBytes nodesVec + svSizeInBytes transVec

--------------------
-- Lift Instances --
--------------------

instance Storable ix => Lift (Dense ix) where
  liftTyped (Dense vec) = [|| Dense $$(liftSV vec) ||]

instance Storable ix => Lift (Sparse ix) where
  liftTyped (Sparse v0 v1) = [|| Sparse $$(liftSV v0) $$(liftSV v1) ||]

-- If we lift vectors for these DFAs using the normal Lift instance, they end up calling fromList on
-- a very large list, which generates a massive amount of syntax and GHC takes a long time to
-- compile it. But in TH 2.16, they added the ability to directly embed an array of bytes, which is
-- both far faster to compile and more efficient at run time
--
-- Below is the type signature for TH 2.16, but it changes in 2.17, so just let it be inferred.
--
-- liftSV :: Storable a => SV.Vector a -> TH.TExpQ (SV.Vector a)
liftSV vec = [|| unsafePerformIO do
    fp <- newForeignPtr_ (Ptr $$bytesAddr)
    pure (SV.unsafeFromForeignPtr0 fp nElems) ||]
    where
      (aPtr, nElems) = SV.unsafeToForeignPtr0 vec

      -- Use bytesPrimL to embed the bytes into the binary as an Addr# literal
      bytesAddr = unsafeSpliceCoerce @_ @Addr#
        (pure (TH.LitE (TH.bytesPrimL bytes)))

      bytes = TH.mkBytes bytesPtr 0 (fromIntegral nBytes)
      bytesPtr = castForeignPtr aPtr :: ForeignPtr Word8
      nBytes = svSizeInBytes vec

svSizeInBytes :: forall a. Storable a => SV.Vector a -> Int
svSizeInBytes vec = SV.length vec * sizeOf (undefined :: a)

---------------
-- Debugging --
---------------

-- | Pretty-print a human-readable description of the DFA structure for debugging purposes.
prettyPrint :: Transitions t => DFA t -> String
prettyPrint dfa@(DFA t _) = foldMap ppNode $ zip [0..] $ toList $ transToSimple t
  where
    ppNode :: (Int, ByteMap Node) -> String
    ppNode (i, ts)
      | ILSet.null ms = prefix <> transStr
      | otherwise     = prefix <> matchStr <> transStr
      where
        ms = matches dfa (Node i)
        prefix = show i
        matchStr = "\t: MatchSet    " <> show (ILSet.toList ms) <> "\n"
        transStr = "\t: Transitions " <> show (map ppTrans (ILMap.toList ts)) <> "\n"

    ppTrans :: (Word8, Node) -> String
    ppTrans (b, Node i) = "0x"<>pad<>bHex<>" => "<>show i
      where
        pad  = if length bHex < 2 then "0" else ""
        bHex = showHex b ""

data Stats = Stats
  { statNodeCount :: Int
  , statAvgTransitionsPerNode :: Double
  , statMatchNodeCount :: Int
  , statAvgMatchesPerMatchNode :: Double
  , statByteSizeOfTransitions :: Int
  }
  deriving (Show)

computeStats :: Transitions t => DFA t -> Stats
computeStats (DFA t matchNodes) = Stats
  { statNodeCount = n
  , statAvgTransitionsPerNode = avg (map ILMap.size (V.toList simpleVec))
  , statMatchNodeCount = ILMap.size matchNodes
  , statAvgMatchesPerMatchNode = avg (map ILSet.size (ILMap.elems matchNodes))
  , statByteSizeOfTransitions = transSize t
  }
  where
    n = V.length simpleVec
    simpleVec = transToSimple t

    avg :: Real a => [a] -> Double
    avg [] = 0
    avg as = foldl' (\s a -> s + realToFrac a) 0 as / realToFrac (length as)

