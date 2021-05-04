module Thexa.Lexer.Core
( Lexer

-- * Rules
, Rule(..)
, IsRule(..)
, LexerMode
, inMode
, inModes
, matchIf
, followedBy
, notFollowedBy
, onMatch
, skipMatch

-- * Construction
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
import Thexa.IntLike.Set qualified as ILSet

import Thexa.DFA (MatchKey)
import Thexa.DFA qualified as DFA
import Thexa.Regex (Regex)
import Thexa.Regex.Compiler

type DFA = DFA.DFA (DFA.Dense Word32)

-- | A precompiled lexer, parameterized on the types of its rules' conditions and actions.
data Lexer mode cond act = Lexer
  { lexerDFA :: {-# UNPACK #-} !(Vector DFA)
  -- ^ The DFAs used to match input to the lexer, one for each mode.
  , lexerMatchInfo :: {-# UNPACK #-} !(Vector (MatchInfo cond act))
  -- ^ Array of info on how we should handle a match for each rule. The DFAs are constructed so that
  -- the 'MatchKey' for each regex is an index into this array.
  }

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

-----------------
-- Lexer Rules --
-----------------

-- | A lexer rule, describing when the rule matches and what to do if it matches.
--
-- The intent is not to construct this type directly, but rather use the rule combinators to
-- construct a rule from a 'Regex'. For example:
--
-- @@@
-- [re|(a|b)+|]
--     `'followedBy'` [re|c|]
--     `'matchIf'` [|| someCondition ||]
--     `'onMatch'` [|| someAction ||]
-- @@@
--
-- Note that multiple 'matchIf' clauses are allowed, and all clauses other than the regex itself are
-- optional (but if 'onMatch' is omitted, then 'skipMatch' should be used instead).
data Rule mode cond act = Rule
  { ruleRegex :: Regex
  -- ^ The regex that must be matched for the rule to match.
  , ruleAction :: Maybe (TExpQ act)
  -- ^ The action that should be run when the rule matches. If this is absent, then the rule simply
  -- skips any input that it matches.
  , ruleFollowedBy :: Maybe Regex
  -- ^ If this is present, then the rule only matches if this regex matches the input immediately
  -- following the input matched by 'ruleRegex'. The difference from just appending this to
  -- 'ruleRegex' is that the input matched by this is not consumed by the rule.
  , ruleNotFollowedBy :: Maybe Regex
  -- ^ The same as 'ruleFollowedBy' except that the rule only matches if this regex DOESN'T match
  -- the input immediately following the input matched by 'ruleRegex'. It is valid to include both a
  -- 'ruleFollowedBy' and a 'ruleNotFollowedBy'.
  , ruleConditions :: [TExpQ cond]
  -- ^ The list of additional, user-defined conditions that must all be satisfied in order for the
  -- rule to match.
  , ruleModes :: Set mode
  -- ^ Set of 'LexerMode's in which this rule is active. If empty, the rule will only be active in
  -- the default mode.
  }

-- | Class to enable transparently treating a 'Regex' as a 'Rule'.
class IsRule rule mode cond act where
  toRule :: rule -> Rule mode cond act

instance IsRule Regex mode cond act where
  toRule regex = Rule regex Nothing Nothing Nothing [] Set.empty

instance IsRule (Rule mode cond act) mode cond act where
  toRule = id

-- TODO: document
type LexerMode a = (Enum a, Bounded a, Ord a)

-- | Insert the given mode into 'ruleModes'.
inMode :: (IsRule rule mode cond act, LexerMode mode) => rule -> mode -> Rule mode cond act
inMode (toRule -> rule) mode = rule { ruleModes = Set.insert mode (ruleModes rule)}

-- | Insert all the given modes into 'ruleModes'.
inModes :: (IsRule rule mode cond act, LexerMode mode) => rule -> [mode] -> Rule mode cond act
inModes (toRule -> rule) (Set.fromList -> modes) = rule { ruleModes = Set.union modes (ruleModes rule)}

-- | Prepend the given condition to 'ruleConditions'.
matchIf :: IsRule rule mode cond act => rule -> TExpQ cond -> Rule mode cond act
matchIf (toRule -> rule) cond = rule { ruleConditions = cond : ruleConditions rule }

-- | Set 'ruleFollowedBy' to the given regex.
--
-- Calls 'error' if 'ruleFollowedBy' has already been set for this rule.
followedBy :: (Partial, IsRule rule mode cond act) => rule -> Regex -> Rule mode cond act
followedBy (toRule -> rule) regex =
  case ruleFollowedBy rule of
    Nothing -> rule { ruleFollowedBy = Just regex }
    Just _  -> error "lexer rule already has a followedBy regex"

-- | Set 'ruleNotFollowedBy' to the given regex.
--
-- Calls 'error' if 'ruleNotFollowedBy' has already been set for this rule.
notFollowedBy :: (Partial, IsRule rule mode cond act) => rule -> Regex -> Rule mode cond act
notFollowedBy (toRule -> rule) regex =
  case ruleNotFollowedBy rule of
    Nothing -> rule { ruleNotFollowedBy = Just regex }
    Just _  -> error "lexer rule already has a notFollowedBy regex"

-- | Set 'ruleAction' to the given action.
--
-- Calls 'error' if 'ruleAction' has already been set for this rule.
onMatch :: forall rule mode cond act. (Partial, IsRule rule mode cond act) => rule -> TExpQ act -> Rule mode cond act
onMatch (toRule @_ @_ @_ @act -> rule) action =
  case ruleAction rule of
    Nothing -> rule { ruleAction = Just action }
    Just _  -> error "lexer rule already has an onMatch action"

-- | Indicate that the rule should simply skip the consumed input when it matches.
--
-- Doesn't actually do anything other than call 'error' if the rule already has a 'ruleAction', but
-- is useful both to indicate intent and to convert a bare 'Regex' to a 'Rule', so a rule to skip
-- whitespace might look like @[re|$space+|] & skipMatch@.
skipMatch :: (Partial, IsRule rule mode cond act) => rule -> Rule mode cond act
skipMatch (toRule -> rule) =
  case ruleAction rule of
    Nothing -> rule
    Just _  -> error "lexer rule already has an onMatch action"

-- | Construct a lexer at compile-time from the list of rules it should match.
--
-- The order of the rules in the list is important; the resulting lexer will always prefer the
-- longest match, but in the case that multiple rules match the same length of input, the rule that
-- appears earliest in the list will be chosen.
makeLexer :: forall mode cond act. LexerMode mode => [Rule mode cond act] -> TExpQ (Lexer mode cond act)
makeLexer rules = [|| Lexer dfas (V.fromListN matchListLen $$matchList) ||]
  where
    regexes = [(if Set.null modes then defaultMode else modes, (ruleRegex rule, i))
              | i <- [0..]
              | rule <- rules
              , let modes = ruleModes rule
              ]

    dfas = V.fromList $ map (DFA.fromNFA . compileRegexes) regexesByMode

    regexesByMode = allModes & map \mode ->
      regexes & filterMap \(modes, regex) ->
        if Set.member mode modes then Just regex else Nothing

    allModes :: [mode]
    allModes = [minBound .. maxBound]

    defaultMode = Set.singleton (toEnum 0)
    validModes = and
      [ fromEnum (minBound @mode) == 0
      , map (fromEnum @mode) [minBound..maxBound] == [0..fromEnum (maxBound @mode)]
      ]

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
