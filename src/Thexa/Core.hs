module Thexa.Core
( module Thexa.Rule

-- * Lexer construction
, Lexer
, makeLexer

-- * Running the lexer
, GetNextByte
, EvalCondition
, MatchResult(..)
, nextMatch
) where

import PreludePrime

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as V
import Language.Haskell.TH (TExpQ)

import Thexa.Internal.DFA (MatchKey)
import Thexa.Internal.DFA qualified as DFA
import Thexa.Internal.IntLike.Set qualified as ILSet
import Thexa.Internal.Regex.Compiler

import Thexa.Position (GetNextByte)
import Thexa.Rule

type DFA = DFA.DFA DFA.Dense32

-- | A precompiled lexer, parameterized on the types of its rules' modes, conditions, and actions.
data Lexer mode cond act = Lexer
  {-# UNPACK #-} !(Vector DFA)
  -- ^ The DFAs used to match input to the lexer, one for each mode.
  {-# UNPACK #-} !(Vector (MatchInfo cond act))
  -- ^ Array of info on how we should handle a match for each rule. The DFAs are constructed so that
  -- the 'MatchKey' for each regex is an index into this array.

-- Specify that the mode is nominal since it would otherwise be phantom
type role Lexer nominal representational representational

data MatchInfo cond act = MatchInfo
  { matchAction :: Maybe act
  -- ^ Action to run on match, or 'Nothing' to skip matched input.
  , matchFollowedBy :: Maybe DFA
  -- ^ Optional DFA that should match the input following this match.
  , matchNotFollowedBy :: Maybe DFA
  -- ^ Optional DFA that should not match the input following this match.
  , matchConditions :: [cond]
  -- ^ List of conditions which must be satisfied in order for the rule to match. Conditions are
  -- resolved to a 'Bool' by a user-supplied 'EvalCondition' function.
  }
  deriving (Generic, NFData)

instance (NFData cond, NFData act) => NFData (Lexer mode cond act) where
  rnf (Lexer dfas matchVec) = rnf dfas `seq` rnf matchVec

-- | Construct a lexer at compile-time from the list of rules it should match.
--
-- The order of the rules in the list is important; the resulting lexer will always prefer the
-- longest match, but in the case that multiple rules match the same length of input, the rule that
-- appears earliest in the list will be chosen.
makeLexer :: forall mode cond act. LexerMode mode => [Rule mode cond act] -> TExpQ (Lexer mode cond act)
makeLexer rules
  | modesAreValid = [|| Lexer dfas (V.fromListN matchListLen $$matchList) ||]
  | otherwise = fail "invalid Enum instance for LexerMode"
  where
    dfas = V.fromList $ map (DFA.fromNFA . compileRegexes) regexesByMode

    -- Extract the modes, regex, and rule index from each rule
    regexes :: [(Set mode, (Regex, Int))]
    regexes = [ (if Set.null modes then defaultMode else modes, (ruleRegex rule, i))
              | i <- [0..]
              | rule <- rules
              , let modes = ruleModes rule
              ]

    -- Get the list of regexes for each mode
    regexesByMode :: [[(Regex, Int)]]
    regexesByMode = allModes & map \mode ->
      [ regex | (modes, regex) <- regexes, Set.member mode modes ]

    -- Enforce that the enum instance is equivalent to a derived one
    modesAreValid = map fromEnum allModes == [0..fromEnum (maxBound @mode)]
    defaultMode = Set.singleton minBound
    allModes = [minBound @mode .. maxBound @mode]

    -- Construct the list of MatchInfos
    matchList = liftListWith liftMatchInfo rules
    matchListLen = length rules

    liftMatchInfo :: Rule mode cond act -> TExpQ (MatchInfo cond act)
    liftMatchInfo rule = [|| MatchInfo
      { matchAction = $$matchAct
      , matchFollowedBy = fbDFA
      , matchNotFollowedBy = nfDFA
      , matchConditions = $$matchConds
      }||]
      where
        fbDFA = mkDFA <$> ruleFollowedBy rule
        nfDFA = mkDFA <$> ruleNotFollowedBy rule
        mkDFA = DFA.fromNFA . compileRegex
        matchConds = liftListWith id (ruleConditions rule)
        matchAct = case ruleAction rule of
          Nothing  -> [|| Nothing ||]
          Just act -> [|| Just $$act ||]

    liftListWith :: (a -> TExpQ b) -> [a] -> TExpQ [b]
    liftListWith f = foldr (\a bsQ -> [|| $$(f a) : $$bsQ ||]) [|| [] ||]

---------------------
-- Lexer Execution --
---------------------

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
nextMatch :: forall mode cond act str
   . LexerMode mode
  => Lexer mode cond act    -- ^ The lexer to match the input against.
  -> GetNextByte str        -- ^ Function to get the next byte of the input stream.
  -> EvalCondition str cond -- ^ Function to evaluate match conditions.
  -> mode                   -- ^ The currently active mode for the lexer.
  -> str                    -- ^ The input stream to match.
  -> MatchResult str act
nextMatch (Lexer dfas matchVec) getNextByte evalCond mode = go
  where
    dfa = (V.!) dfas (fromEnum mode)

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
    matchKeyToInfo k = (V.!) matchVec k

    -- Returns whether all conditions of the given match are satisfied.
    validMatch :: str -> (str, MatchInfo cond act) -> Bool
    validMatch str (str', match) = and
      [ case matchFollowedBy match of
          Nothing    -> True
          Just fbDFA -> dfaCanMatch fbDFA DFA.startNode str'

      , case matchNotFollowedBy match of
          Nothing    -> True
          Just nfDFA -> not (dfaCanMatch nfDFA DFA.startNode str')

      , all (evalCond str str') (matchConditions match)
      ]

    -- Returns whether the given DFA matches the input stream when starting from the given node.
    -- Used to check the followedBy and notFollowedBy conditions.
    dfaCanMatch :: DFA -> DFA.Node -> str -> Bool
    dfaCanMatch fbDFA node str
      | not (ILSet.null matches)          = True
      | Just (b, str') <- getNextByte str
      , Just node' <- DFA.step dfa node b = dfaCanMatch fbDFA node' str'
      | otherwise                         = False
      where
        matches = DFA.matches fbDFA node
{-# INLINE nextMatch #-}
