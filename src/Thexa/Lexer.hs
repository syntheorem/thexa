module Thexa.Lexer
( Lexer

-- * Rules
, Rule(..)
, IsRule(..)
, matchIf
, followedBy
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

import Thexa.Lexer.Core
