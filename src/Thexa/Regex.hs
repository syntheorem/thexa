module Thexa.Regex
( Regex
, CharSet

-- * Quasi-quoters
, re
, cs

-- * Construction
-- | These functions can be used to programatically construct 'Regex'es as an alternative to using
-- the quasi-quoters. They are intended to be used with a qualified import, e.g.
-- @import qualified Thexa.Regex as RE@.
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

import Prelude hiding (concat)

import Thexa.CharSet (CharSet)
import Thexa.Internal.Regex.AST (Regex)
import Thexa.Internal.Regex.AST qualified as AST
import Thexa.Internal.Regex.QuasiQuoters (re, cs)

-- Functions from Regex.AST with their types specialized to Regex
-----------------------------------------------------------------

-- | Regex that matches the given character.
char :: Char -> Regex
char = AST.char

-- | Regex that matches a set of characters.
--
-- Calls 'error' if the provided set is empty. Technically, an empty set is not invalid, but then
-- the returned regex will never match anything, which is very likely not what the user intended.
-- But because of splicing, we can't necessarily check that a char set is non-empty when we parse
-- it. So we have the regex quasi-quoter use this smart constructor when wrapping a char set,
-- causing an exception (with a backtrace) to be thrown when we try to compile the regex. It's not
-- an ideal error reporting mechanism, but the alternatives are silently generating a regex that
-- matches nothing or generating the error in the regex compiler, at which point we've lost context
-- for where the offending char set originated.
chars :: Partial => CharSet -> Regex
chars = AST.chars

-- | Like 'chars', but returns 'Nothing' when the char set is empty.
tryChars :: CharSet -> Maybe Regex
tryChars = AST.tryChars

-- | Regex that matches the given range of characters.
--
-- Like 'chars', calls 'error' for an empty range.
charRange :: Partial => Char -> Char -> Regex
charRange = AST.charRange

-- | Regex that matches the given string exactly.
string :: String -> Regex
string = AST.string

-- | Regex that matches its first argument and then its second.
append :: Regex -> Regex -> Regex
append = AST.append

-- | 'append' a list of regexes in order.
concat :: [Regex] -> Regex
concat = AST.concat

-- | Regex that matches either of its two arguments.
alt :: Regex -> Regex -> Regex
alt = AST.alt

-- | 'alt' a list of regexes.
alts :: [Regex] -> Regex
alts = AST.alts

-- | Regex that matches its argument one or more times.
plus :: Regex -> Regex
plus = AST.plus

-- | Regex that matches its argument zero or more times.
star :: Regex -> Regex
star = AST.star

-- | Regex that matches its argument zero or one times.
opt :: Regex -> Regex
opt = AST.opt

-- | Regex that matches its argument exactly @n@ times.
repeat :: Natural -> Regex -> Regex
repeat = AST.repeat

-- | Regex that matches its argument @n@ or more times.
repeatUnbounded :: Natural -> Regex -> Regex
repeatUnbounded = AST.repeatUnbounded

-- | Regex that matches its argument at least @n@ times and no more than @m@ times.
repeatBounded :: Natural -> Natural -> Regex -> Regex
repeatBounded = AST.repeatBounded
