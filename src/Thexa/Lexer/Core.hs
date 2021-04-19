module Thexa.Lexer.Core where

import PreludePrime

import Data.Primitive.Array
import Thexa.IntLike.Set qualified as ILSet

import Thexa.DFA (DFA, MatchKey)
import Thexa.DFA qualified as DFA

-- | A precompiled lexer, parameterized on the types of its rules' conditions and actions.
data Lexer cond act = Lexer
  { lexerDFA :: !DFA
  -- ^ The DFA used to match input to the lexer.
  , lexerMatchInfo :: {-# UNPACK #-} !(Array (MatchInfo cond act))
  -- ^ Array of info on how we should handle a match for each rule. The DFA is constructed so that
  -- the 'MatchKey' for each regex is an index into this array.
  }

data MatchInfo cond act = MatchInfo
  { matchAction :: Maybe act
  -- ^ Action to run on match, or 'Nothing' to skip matched input.
  , matchFollowedBy :: Maybe DFA
  -- ^ Optional DFA that should match the input following this match.
  , matchConditions :: [cond]
  -- ^ List of conditions which must be satisfied in order for the rule to match. Conditions are
  -- resolved to a 'Bool' by a user-supplied 'EvalCondition' function.
  }
  deriving (Generic, NFData)

instance (NFData cond, NFData act) => NFData (Lexer cond act) where
  rnf (Lexer dfa matchArr) = rnf dfa `seq` rnf matchArr

-- | Type of the function used to get the next byte of the input stream.
--
-- Such a function should return the next byte and the remaining input, or 'Nothing' if the input
-- stream is empty.
type GetNextByte str = str -> Maybe (Word8, str)

-- | Type of the function used to evaluate rule conditions.
--
-- The function is provided the input stream at the start of the match, the input stream at the end
-- of the match, and the condition to evaluate. Returns whether the condition was satisfied.
type EvalCondition str cond = str -> str -> cond -> Bool

-- | Result of trying to get the 'nextMatch' of the input.
--
-- On a successful match, the result contains the remaining unconsumed input stream. Note that it
-- does not contain the string that was actually matched; to provide that, you need a way to derive
-- it from the initial input and the remaining input, e.g. by having @str@ contain an offset that is
-- incremented by the 'GetNextByte' function.
data MatchResult str act

  -- | Successfully matched a rule that skips the input it matched.
  = MatchSkip str

  -- | Successfully matched a rule that should run the given action when matched.
  | MatchAction str act

  -- | Failed to match any rule.
  | MatchError

  -- | Reached the end of the input.
  --
  -- Note that this result is only returned if the input provided to 'nextMatch' is already empty.
  -- Otherwise, if we reach the end of the input but haven't found a match, we return 'MatchError'.
  | MatchEOF

-- | Attempts to find the next match from the start of the provided input stream.
nextMatch :: forall cond act str
   . Lexer cond act         -- ^ The lexer to match the input against.
  -> GetNextByte str        -- ^ Function to get the next byte of the input stream.
  -> EvalCondition str cond -- ^ Function to evaluate match conditions.
  -> str                    -- ^ The input stream to match.
  -> MatchResult str act
nextMatch (Lexer dfa matchArr) getNextByte evalCond = go
  where
    -- This is a separate function so we can inline nextMatch when it's applied to only the first
    -- three arguments.
    go :: str -> MatchResult str act
    go str = case find (validMatch str) matchStack of
      Just (str', MatchInfo{matchAction=mAct})
        | Just act <- mAct -> MatchAction str' act
        | Nothing  <- mAct -> MatchSkip str'
      _ | isEOF            -> MatchEOF
      _ | otherwise        -> MatchError
      where
        matchStack = buildMatchStack DFA.startNode str []
        isEOF = isNothing (getNextByte str)

    -- Recursively build the "match stack" for the given input stream. The idea is we step the DFA
    -- as far as we can along the input. At each step, we push any possible matches onto the match
    -- stack. At this point they are only "possible" matches because they may have additional
    -- conditions to check. But we don't want to check these conditions eagerly, because we always
    -- prefer the longest match, so many possible matches will be ruled out by the end anyway. So
    -- instead we build the whole stack of possible matches and then once we're done we can simply
    -- find the top-most match that satisfies its additional conditions, if any.
    buildMatchStack :: DFA.Node -> str -> [(str, MatchInfo cond act)] -> [(str, MatchInfo cond act)]
    buildMatchStack node str matchStack
      | Just (b, str') <- getNextByte str
      , Just node' <- DFA.step dfa node b = buildMatchStack node' str' matchStack'
      | otherwise                         = matchStack'
      where
        -- Push the matches for the current node onto the match stack. Note that the order is
        -- important, because matches higher on the stack are prioritized. Since the match keys are
        -- the index of the rule that we're matching, and we want to prioritize earlier rules, using
        -- a foldr respects this ordering since we'll fold over the match keys in descending order.
        matchStack' = ILSet.foldr pushMatch matchStack (DFA.matches dfa node)
        pushMatch k ms = (str, matchKeyToInfo k) : ms

    matchKeyToInfo :: MatchKey -> MatchInfo cond act
    matchKeyToInfo k
      -- This shouldn't happen by construction, but better safe than sorry
      | k < 0 || k >= sizeofArray matchArr = error "invalid match key"
      | otherwise = indexArray matchArr k

    -- Returns whether all conditions of the given match are satisfied.
    validMatch :: str -> (str, MatchInfo cond act) -> Bool
    validMatch str (str', match) = and
      [ case matchFollowedBy match of
          Nothing    -> True
          Just fbDFA -> dfaCanMatch fbDFA DFA.startNode str'

      , all (evalCond str str') (matchConditions match)
      ]

    -- Returns whether the given DFA matches the input stream when starting from the given node.
    -- Used to check the followedBy condition.
    dfaCanMatch :: DFA -> DFA.Node -> str -> Bool
    dfaCanMatch fbDFA node str
      | not (ILSet.null matches)          = True
      | Just (b, str') <- getNextByte str
      , Just node' <- DFA.step dfa node b = dfaCanMatch fbDFA node' str'
      | otherwise                         = False
      where
        matches = DFA.matches fbDFA node
{-# INLINE nextMatch #-}
