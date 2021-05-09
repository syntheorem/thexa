module Thexa.Rule
( Rule(..)
, LexerMode

-- * Template Haskell splices
-- | This type is re-exported from the @th-compat@ library. The type of typed TH splices was changed
-- in GHC 9.0, so using this alias lets us transparently support earlier versions.
, SpliceQ

-- * Regular expressions
, Regex
, CharSet
, re
, cs

-- * Rule combinators
, IsRule(..)
, onMatch
, skipMatch
, matchIf
, followedBy
, notFollowedBy
, inMode
, inModes
, rulesInMode
, rulesInModes
) where

import PreludePrime

import Data.Set (Set)
import Data.Set qualified as Set
import Language.Haskell.TH.Syntax.Compat (SpliceQ)

import Thexa.Regex (Regex, CharSet, re, cs)

-- | A lexer rule, describing when the rule matches and what to do if it matches.
--
-- The intent is not to construct this type directly, but rather use the rule combinators to
-- construct a rule from a 'Regex'. For example:
--
-- @@@
-- [re|(a|b)+|]
--   `'followedBy'` [re|c|]
--   `'matchIf'` [|| someCondition ||]
--   `'onMatch'` [|| someAction ||]
-- @@@
--
-- Note that multiple 'matchIf' clauses are allowed, and all clauses other than the regex itself are
-- optional (but if 'onMatch' is omitted, then 'skipMatch' should be used instead).
--
-- Conditions (including followedBy and notFollowedBy) are only checked AFTER the regex has matched,
-- and if the regex matches multiple lengths of input, then the conditions will be checked for each
-- length until they are satisfied, starting with the longest. The implication here is to avoid
-- using rules that use overly general regexes and then restrict the matches via conditions.
--
-- However, lexer modes do not work like conditions in this respect, because rules which are not
-- enabled for the active mode are not matched against in the first place. See 'LexerMode' for more
-- details.
data Rule mode cond act = Rule
  { ruleRegex :: Regex
  -- ^ The regex that must be matched for the rule to match.
  , ruleAction :: Maybe (SpliceQ act)
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
  , ruleConditions :: [SpliceQ cond]
  -- ^ The list of additional, user-defined conditions that must all be satisfied in order for the
  -- rule to match.
  , ruleModes :: Set mode
  -- ^ Set of 'LexerMode's in which this rule is active. If empty, the rule will only be active in
  -- the default mode.
  }

-- | Constraint for types that can be used as lexer modes.
--
-- A lexer can operate in one or more modes. Each mode is like a "sublexer" which only matches
-- against rules which are active in its mode. Custom lexer mode types can easily be created:
--
-- @@@
-- data MyLexerMode
--   = DefaultMode
--   | Mode1
--   | Mode2
--   deriving (Eq, Ord, Enum, Bounded)
-- @@@
--
-- Essentially, a lexer mode is just an index into an array containing the state matchines used to
-- match input for each mode. Rather than use the indices directly, we use a custom type to provide
-- more type safety and use the 'Enum' instance to convert each mode constructor to its index.
--
-- This does mean that an additional constraint is required for the 'Enum' instance: given @n@
-- constructors, the constructors are numbered from @0@ to @n-1@. But this is exactly what is done
-- when the 'Enum' instance is derived by the compiler, so simply deriving that instance for an
-- enumerated type is sufficient.
--
-- The first constructor for the mode (i.e. 'minBound') is considered the default mode for the
-- lexer, meaning that rules without any modes explicitly set will only be active in that mode.
--
-- If your lexer only has a single mode, then you can use the unit type @()@ as your lexer mode
-- rather than having to create a type with a single constructor.
type LexerMode a = (Enum a, Bounded a, Ord a)

-- | Class to enable transparently treating a 'Regex' as a 'Rule'.
class IsRule rule mode cond act where
  toRule :: rule -> Rule mode cond act

instance IsRule Regex mode cond act where
  toRule regex = Rule regex Nothing Nothing Nothing [] Set.empty

instance IsRule (Rule mode cond act) mode cond act where
  toRule = id

-- | Set 'ruleAction' to the given action.
--
-- Calls 'error' if 'ruleAction' has already been set for this rule.
onMatch :: forall rule mode cond act. (Partial, IsRule rule mode cond act) => rule -> SpliceQ act -> Rule mode cond act
onMatch (toRule @_ @_ @_ @act -> rule) action =
  case ruleAction rule of
    Nothing -> rule { ruleAction = Just action }
    Just _  -> error "lexer rule already has an onMatch action"

-- | Indicate that the rule should simply skip the consumed input when it matches.
--
-- Doesn't actually do anything other than call 'error' if the rule already has a 'ruleAction', but
-- is useful both to indicate intent and to convert a bare 'Regex' to a 'Rule', so a rule to skip
-- whitespace might look like @[re|[:space:]+|] & skipMatch@.
skipMatch :: (Partial, IsRule rule mode cond act) => rule -> Rule mode cond act
skipMatch (toRule -> rule) =
  case ruleAction rule of
    Nothing -> rule
    Just _  -> error "lexer rule already has an onMatch action"

-- | Prepend the given condition to 'ruleConditions'.
matchIf :: IsRule rule mode cond act => rule -> SpliceQ cond -> Rule mode cond act
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

-- | Insert the given mode into 'ruleModes'.
inMode :: (IsRule rule mode cond act, LexerMode mode) => rule -> mode -> Rule mode cond act
inMode (toRule -> rule) mode = rule { ruleModes = Set.insert mode (ruleModes rule)}

-- | Insert all the given modes into 'ruleModes'.
inModes :: (IsRule rule mode cond act, LexerMode mode) => rule -> [mode] -> Rule mode cond act
inModes (toRule -> rule) (Set.fromList -> modes) = rule { ruleModes = Set.union modes (ruleModes rule)}

-- | Insert the given mode into 'ruleModes' for each rule in the list.
rulesInMode :: LexerMode mode => mode -> [Rule mode cond act] -> [Rule mode cond act]
rulesInMode mode = map (`inMode` mode)

-- | Insert the given modes into 'ruleModes' for each rule in the list.
rulesInModes :: LexerMode mode => [mode] -> [Rule mode cond act] -> [Rule mode cond act]
rulesInModes modes = map (`inModes` modes)
