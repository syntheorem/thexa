module Thexa.NFA
( NFA
, Node
, Transitions(..)

-- * Type aliases
, MatchKey
, MatchSet
, NodeSet
, NodeMap
, ByteMap

-- * NFA accessors
, startNode
, nodeCount
, matchNodes
, transitions
, byteTransitions
, epsilonTransitions

-- * NFA simulation
, startNodes
, step
, stepAll
, stepByte
, epsilonClosure
, matches

-- * Building an NFA
, Build
, runBuild
, execBuild
, newNode
, newMatchNode
, addTransition
, addTransitions
, addEpsilonTransition
) where

import PreludePrime

import Control.Monad.ST (ST, runST)
import Data.Primitive.Array
import Data.Primitive.MutVar

import Thexa.GrowArray qualified as GA
import Thexa.IntLike.Class (IntLike)
import Thexa.IntLike.Map qualified as ILMap
import Thexa.IntLike.Set qualified as ILSet

-- | Non-deterministic Finite Automaton with byte-labeled transitions.
--
-- An NFA is a set of nodes (a.k.a. states) where each node has a set of transitions. A transition
-- indicates which node we should go to next based on the next byte of input. It is called
-- non-deterministic because a single byte of input can transition to multiple nodes, and we can
-- transition to a node without consuming the next byte of input (this is known as an epsilon
-- transition).
--
-- An NFA has a unique start node that serves as the initial state. It also has a set of match nodes
-- (a.k.a. end nodes) that indicate a successful match when they are reached. Each match node is
-- associated with a 'MatchKey'.
data NFA = NFA
  { nfaTransitions :: {-# UNPACK #-} !(Array Transitions)
  , nfaMatchNodes  :: !(NodeMap MatchKey)
  }

-- | A node (also called a state) of an NFA.
--
-- This is simply represented as an index into an array, so one must be careful to avoid mixing
-- nodes from multiple NFAs. Functions in this module will throw an exception if there is an attempt
-- to access a node with an index that is out of bounds.
newtype Node = Node Int
  deriving (Show)
  deriving newtype (Eq, Ord, IntLike, NFData)

-- | Set of transitions for a given node.
data Transitions = Transitions
  { epsilon :: !NodeSet
  -- ^ Epsilon transitions, i.e. the set of nodes we can transition to without consuming input.
  , byteMap :: !(ByteMap NodeSet)
  -- ^ Map from bytes to the set of nodes we can transition to after consuming that byte.
  }

-- | A key to identify which pattern was matched.
--
-- Since we're compiling multiple regexes into a single automaton, we need a way to associate each
-- match node with the regex that it matches. To do this, we can simply give a distinct 'MatchKey'
-- to each regex and associate it with the relevant match node.
type MatchKey = Int

-- | A set of 'MatchKey's.
type MatchSet = ILSet.ILSet MatchKey

-- | A set of 'Node's.
type NodeSet = ILSet.ILSet Node

-- | A map from 'Node's to some other value.
type NodeMap = ILMap.ILMap Node

-- | A map from bytes to some other value.
type ByteMap = ILMap.ILMap Word8

-- | The start node that each NFA is initialized with.
startNode :: Node
startNode = Node 0

-- | The number of nodes in the NFA.
nodeCount :: NFA -> Int
nodeCount = sizeofArray . nfaTransitions

-- | The set of match nodes of an NFA and their associated 'MatchKey's.
matchNodes :: NFA -> NodeMap MatchKey
matchNodes = nfaMatchNodes

-- | The 'Transitions' for a given node in the NFA.
transitions :: NFA -> Node -> Transitions
transitions NFA{nfaTransitions = arr} (Node i)
  | i < 0 || i >= n = error ("invalid node ("<>show (Node i)<>")")
  | otherwise       = indexArray arr i
  where
    n = sizeofArray arr

-- | Set of nodes we can transition to from the given node after consuming the given byte, excluding
-- subsequent epsilon transitions.
byteTransitions :: NFA -> Node -> Word8 -> NodeSet
byteTransitions nfa n byte = fromMaybe ILSet.empty (ILMap.lookup byte (byteMap (transitions nfa n)))

-- | Set of nodes with an epsilon transition from the given node.
epsilonTransitions :: NFA -> Node -> NodeSet
epsilonTransitions nfa n = epsilon (transitions nfa n)

--------------------
-- NFA Simulation --
--------------------

-- | Initial set of nodes for the NFA simulation, i.e. the epsilon closure of 'startNode'.
startNodes :: NFA -> NodeSet
startNodes nfa = epsilonClosure nfa (ILSet.singleton startNode)

-- | Step the NFA simulation by feeding it the given byte.
--
-- Returns the new set of nodes, which may be empty to indicate a failure to match.
step :: NFA -> NodeSet -> Word8 -> NodeSet
step nfa nodes byte = epsilonClosure nfa (stepByte nfa nodes byte)

-- | Step the NFA simulation for each possible input byte.
--
-- Any bytes not in the returned map simply transition to the empty set.
stepAll :: NFA -> NodeSet -> ByteMap NodeSet
stepAll nfa nodes = map (epsilonClosure nfa) byteStepped
  where
    byteStepped = ILSet.foldl' f ILMap.empty nodes
    f bm n = ILMap.unionWith ILSet.union bm (byteMap (transitions nfa n))

-- | Step the NFA simulation without following epsilon transitions.
stepByte :: NFA -> NodeSet -> Word8 -> NodeSet
stepByte nfa nodes byte = ILSet.foldl' f ILSet.empty nodes
  where f ns n = ILSet.union ns (byteTransitions nfa n byte)

-- | Compute the epsilon closure of the given set of nodes.
--
-- The epsilon closure is the given set unioned with the set of all nodes that can be reached by
-- only following epsilon transitions from nodes in the original set.
epsilonClosure :: NFA -> NodeSet -> NodeSet
epsilonClosure nfa nodes = go nodes ILSet.empty
  where
    go !ns !visited
      | ILSet.null unvisited = ns
      | otherwise            = go (ILSet.foldl' f ns unvisited) ns
      where
        unvisited = ILSet.difference ns visited

    f ns n = ILSet.union ns (epsilonTransitions nfa n)

-- | Compute the matches in a set of nodes.
matches :: NFA -> NodeSet -> MatchSet
matches nfa nodes = ILMap.foldl' (flip ILSet.insert) ILSet.empty matchMap
  where matchMap = ILMap.restrictKeys (matchNodes nfa) nodes

------------------
-- NFA Building --
------------------

-- | A monad in which we can efficiently build an NFA.
newtype Build a = Build (forall s. BuildState s -> ST s a)

instance Functor Build where
  fmap f (Build ma) = Build \bs -> fmap f (ma bs)

instance Applicative Build where
  pure a = Build \_ -> pure a
  Build mf <*> Build ma = Build \bs -> mf bs <*> ma bs

instance Monad Build where
  Build ma >>= f = Build \bs -> ma bs >>= \a -> let Build mb = f a in mb bs
  Build ma >> Build mb = Build \bs -> ma bs >> mb bs

data BuildState s = BS
  { bsTransitions :: {-# UNPACK #-} !(GA.GrowArray s Transitions)
  -- ^ Array of node transitions. A node is represented by its index into this array.
  , bsMatchNodes  :: {-# UNPACK #-} !(MutVar s (NodeMap MatchKey))
  -- ^ Set of nodes which represent a successful match in the NFA. Each match node is associated
  -- with an identifier which can be used to determine which pattern matched.
  }

-- | Run a 'Build' action to construct the 'NFA'.
runBuild :: Build a -> (a, NFA)
runBuild b = runST do
  -- Construct initial state and run the action after adding the start node
  let Build ma = newNode >> b
  bs <- initBuildState
  a <- ma bs

  -- Convert state to immutable NFA
  arr <- GA.freeze (bsTransitions bs)
  mNodes <- readMutVar (bsMatchNodes bs)
  pure (a, NFA{nfaTransitions = arr, nfaMatchNodes = mNodes})
  where
    initBuildState :: ST s (BuildState s)
    initBuildState = do
      arr <- GA.new
      var <- newMutVar ILMap.empty
      pure BS{bsTransitions = arr, bsMatchNodes = var}

-- | Like 'runBuild', but ignoring the result of the build action.
execBuild :: Build () -> NFA
execBuild = snd . runBuild

-- | Add a new node to the NFA. Initially has no transitions.
newNode :: Build Node
newNode = Build \BS{bsTransitions = arr} -> do
  n <- GA.length arr
  GA.push arr $! Transitions ILSet.empty ILMap.empty
  pure (Node n)

-- | Add a new match node to the NFA and associate it with the given MatchKey.
newMatchNode :: MatchKey -> Build Node
newMatchNode k = do
  n <- newNode
  Build \bs -> modifyMutVar' (bsMatchNodes bs) (ILMap.insert n k)
  pure n

-- | Add a transition from the first node to the second, labeled with the given byte.
addTransition :: Node -> Word8 -> Node -> Build ()
addTransition from byte to = modifyTransitions from \ts ->
  ts { byteMap = ILMap.insertWith ILSet.union byte (ILSet.singleton to) (byteMap ts) }

-- | Add transitions from a node for multiple bytes at once.
--
-- More efficient than repeated calls to 'addTransition'.
addTransitions :: Node -> ByteMap Node -> Build ()
addTransitions from toMap = modifyTransitions from \ts ->
  ts { byteMap = ILMap.unionWithSetInsert (byteMap ts) toMap }

-- | Add an epsilon transition from the first node to the second.
addEpsilonTransition :: Node -> Node -> Build ()
addEpsilonTransition from to = modifyTransitions from \ts ->
  ts { epsilon = ILSet.insert to (epsilon ts) }

----------------------------
-- Build helper functions --
----------------------------

modifyTransitions :: Node -> (Transitions -> Transitions) -> Build ()
modifyTransitions (Node from) f = Build \bs -> do
  GA.modify' (bsTransitions bs) from f
{-# INLINE modifyTransitions #-}
