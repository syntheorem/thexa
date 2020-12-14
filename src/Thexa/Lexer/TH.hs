module Thexa.Lexer.TH where

import PreludePrime

import Data.Primitive.Array
import Language.Haskell.TH (TExpQ)

import Thexa.DFA qualified as DFA
import Thexa.Lexer.Core
import Thexa.Regex (Regex)
import Thexa.Regex.Compiler qualified as RE

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
data Rule cond act = Rule
  { ruleRegex :: Regex
  -- ^ The regex that must be matched for the rule to match.
  , ruleAction :: Maybe (TExpQ act)
  -- ^ The action that should be run when the rule matches. If this is absent, then the rule simply
  -- skips any input that it matches.
  , ruleFollowedBy :: Maybe Regex
  -- ^ If this is present, then the rule only matches if this regex matches the input immediately
  -- following the input matched by 'ruleRegex'. The difference from just appending this to
  -- 'ruleRegex' is that the input matched by this is not consumed by the rule.
  , ruleConditions :: [TExpQ cond]
  -- ^ The list of additional, user-defined conditions that must all be satisfied in order for the
  -- rule to match.
  }

-- | Class to enable transparently treating a 'Regex' as a 'Rule'.
class IsRule rule cond act where
  toRule :: rule -> Rule cond act

instance IsRule Regex cond act where
  toRule regex = Rule regex Nothing Nothing []

instance IsRule (Rule cond act) cond act where
  toRule = id

-- | Prepend the given condition to 'ruleConditions'.
matchIf :: IsRule rule cond act => rule -> TExpQ cond -> Rule cond act
matchIf (toRule -> rule) cond = rule { ruleConditions = cond : ruleConditions rule }

-- | Set 'ruleFollowedBy' to the given regex.
--
-- Calls 'error' if 'ruleFollowedBy' has already been set for this rule.
followedBy :: (Partial, IsRule rule cond act) => rule -> Regex -> Rule cond act
followedBy (toRule -> rule) regex =
  case ruleFollowedBy rule of
    Nothing -> rule { ruleFollowedBy = Just regex }
    Just _  -> error "lexer rule already has a followedBy regex"

-- | Set 'ruleAction' to the given action.
--
-- Calls 'error' if 'ruleAction' has already been set for this rule.
onMatch :: forall rule cond act. (Partial, IsRule rule cond act) => rule -> TExpQ act -> Rule cond act
onMatch (toRule @_ @_ @act -> rule) action =
  case ruleAction rule of
    Nothing -> rule { ruleAction = Just action }
    Just _  -> error "lexer rule already has an onMatch action"

-- | Indicate that the rule should simply skip the consumed input when it matches.
--
-- Doesn't actually do anything other than call 'error' if the rule already has a 'ruleAction', but
-- is useful both to indicate intent and to convert a bare 'Regex' to a 'Rule', so a rule to skip
-- whitespace might look like @[re|$space+|] & skipMatch@.
skipMatch :: (Partial, IsRule rule cond act) => rule -> Rule cond act
skipMatch (toRule -> rule) =
  case ruleAction rule of
    Nothing -> rule
    Just _  -> error "lexer rule already has an onMatch action"

-- | Construct a lexer at compile-time from the list of rules it should match.
--
-- The order of the rules in the list is important; the resulting lexer will always prefer the
-- longest match, but in the case that multiple rules match the same length of input, the rule that
-- appears earliest in the list will be chosen.
makeLexer :: [Rule cond act] -> TExpQ (Lexer cond act)
makeLexer rules = [|| Lexer dfa (arrayFromListN matchListLen $$matchList) ||]
  where
    dfa = DFA.denseFromNFA (RE.compile regexes)
    regexes = [(ruleRegex rule, i) | rule <- rules | i <- [0..]]
    matchList = liftListWith liftMatchInfo rules
    matchListLen = length rules

    liftMatchInfo :: Rule cond act -> TExpQ (MatchInfo cond act)
    liftMatchInfo rule = [|| MatchInfo
        { matchAction = $$matchAct
        , matchFollowedBy = fbDFA
        , matchConditions = $$matchConds
        }||]
      where
        fbDFA = (DFA.denseFromNFA . RE.compile . \re -> [(re, 0)]) <$> ruleFollowedBy rule
        matchConds = liftListWith id (ruleConditions rule)
        matchAct = case ruleAction rule of
          Nothing  -> [|| Nothing ||]
          Just act -> [|| Just $$act ||]

    liftListWith :: (a -> TExpQ b) -> [a] -> TExpQ [b]
    liftListWith f = foldr (\a bsQ -> [|| $$(f a) : $$bsQ ||]) [|| [] ||]
