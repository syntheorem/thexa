module Thexa.Regex
( Regex
, CharSet

-- * Quasi-quoters
, re
, cs

-- * Compile to NFA
, compile

-- * Construction
, char
, chars
, tryChars
, charRange
, string
, append
, concat
, alt
, alts
, plus
, star
, opt
, repeat
, repeatUnbounded
, repeatBounded
) where

import Thexa.Regex.AST
import Thexa.Regex.CharSet (CharSet)
import Thexa.Regex.Compiler (compile)
import Thexa.Regex.QuasiQuoters (re, cs)
