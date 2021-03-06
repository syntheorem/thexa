module Thexa.Internal.Regex.AST where

import Prelude hiding (concat)

import Data.Foldable (foldr1)
import Data.String (IsString(fromString))

import Thexa.CharSet (CharSet)
import Thexa.CharSet qualified as CS
import Thexa.Internal.CharSet.AST (CharSetAST, IsCharSet(fromCharSet))
import Thexa.Internal.CharSet.AST qualified as CS (fromAST, normalize)

-- | An abstract representation of a regular expression.
--
-- Please consider the aliased type to be an internal implementation detail.
type Regex = RegexAST' CharSet Void
type RegexAST = RegexAST' CharSetAST String

data RegexAST' cs s
  -- | Matches a single character which matches the set.
  = Chars cs

  -- | Matches strings which match either regex.
  | Alt (RegexAST' cs s) (RegexAST' cs s)

  -- | Matches strings which match the first regex and then the second.
  | Seq (RegexAST' cs s) (RegexAST' cs s)

  -- | Matches strings which match the regex @n@ or more times.
  --
  -- If the third argument is provided, the number of times it will match after the minimum is
  -- bounded by that argument. So given @n@ and @Just m@, this will match the regex between @n@ and
  -- @n + m@ times, inclusive.
  | Repeat (RegexAST' cs s) Natural (Maybe Natural)

  -- | Matches the empty string.
  | Empty

  -- | The name of a Haskell value that will be spliced in using Template Haskell.
  --
  -- The type parameter allows us to use this data structure for parsing (where we may encounter
  -- splices) and for representing the final regex with all splices already evaluated. For parsing,
  -- the parameter should be a 'String' with the (possibly qualified) name of the Haskell value to
  -- splice. For fully evaluated regexes, the type should be 'Void' to indicate that there are no
  -- splices. Since this constructor is strict in its parameter, the type checker knows that it
  -- can't be used when the parameter is 'Void', so there is no need to pattern match on it.
  | Splice !s

  deriving (Eq, Show)

instance Semigroup (RegexAST' cs s) where
  (<>) = append

instance Monoid (RegexAST' cs s) where
  mempty = Empty

-- | Regex that matches the given string literal.
instance IsCharSet cs => IsString (RegexAST' cs s) where
  fromString = string

-- | Attempt to convert a 'RegexAST' to a 'Regex'. This can only fail if the AST contains splices.
fromAST :: RegexAST -> Maybe Regex
fromAST = \case
  Splice _      -> Nothing
  Empty         -> Just Empty
  Chars cs      -> Chars <$> CS.fromAST cs
  Alt re1 re2   -> Alt <$> fromAST re1 <*> fromAST re2
  Seq re1 re2   -> Seq <$> fromAST re1 <*> fromAST re2
  Repeat re n m -> fromAST re <&> \re' -> Repeat re' n m

-- | Regex that matches the given character.
char :: IsCharSet cs => Char -> RegexAST' cs s
char = chars . CS.singleton

-- | Regex that matches a set of characters.
--
-- Calls 'error' if the provided set is empty. Technically, an empty set is not invalid, but then
-- the returned regex will never match anything, which is very likely not what the user intended.
-- But because of splicing, we can't necessarily check that a char set is non-empty when we parse
-- it. So we have the regex quasi-quoter use this smart constructor when wrapping a char set,
-- causing an exception (with a backtrace) to be thrown when we try to compile the regex. It's not
-- an ideal error reporting mechanism, but the alternatives are silently generating a regex that
-- matches nothing or generating the error in the compiler, at which point we've lost context for
-- where the offending char set originated.
chars :: (Partial, IsCharSet cs) => CharSet -> RegexAST' cs s
chars = fromMaybe (error "empty character set in regex will match nothing") . tryChars

-- | Like 'chars', but returns 'Nothing' when the char set is empty.
tryChars :: IsCharSet cs => CharSet -> Maybe (RegexAST' cs s)
tryChars cs
  | CS.null cs = Nothing
  | otherwise  = Just (Chars (fromCharSet cs))

-- | Regex that matches the given range of characters.
--
-- Like 'chars', calls 'error' for an empty range.
charRange :: (Partial, IsCharSet cs) => Char -> Char -> RegexAST' cs s
charRange l u = chars (CS.range l u)

-- | Regex that matches the given string exactly.
string :: IsCharSet cs => String -> RegexAST' cs s
string = concat . map char

-- | Regex that matches its first argument and then its second.
append :: RegexAST' cs s -> RegexAST' cs s -> RegexAST' cs s
append = Seq

-- | 'append' a list of regexes in order.
concat :: [RegexAST' cs s] -> RegexAST' cs s
concat [] = Empty
concat rs = foldr1 append rs

-- | Regex that matches either of its two arguments.
alt :: RegexAST' cs s -> RegexAST' cs s -> RegexAST' cs s
alt = Alt

-- | 'alt' a list of regexes.
alts :: [RegexAST' cs s] -> RegexAST' cs s
alts [] = Empty
alts rs = foldr1 alt rs

-- | Regex that matches its argument one or more times.
plus :: RegexAST' cs s -> RegexAST' cs s
plus = repeatUnbounded 1

-- | Regex that matches its argument zero or more times.
star :: RegexAST' cs s -> RegexAST' cs s
star = repeatUnbounded 0

-- | Regex that matches its argument zero or one times.
opt :: RegexAST' cs s -> RegexAST' cs s
opt = repeatBounded 0 1

-- | Regex that matches its argument exactly @n@ times.
repeat :: Natural -> RegexAST' cs s -> RegexAST' cs s
repeat n r = Repeat r n (Just 0)

-- | Regex that matches its argument @n@ or more times.
repeatUnbounded :: Natural -> RegexAST' cs s -> RegexAST' cs s
repeatUnbounded n r = Repeat r n Nothing

-- | Regex that matches its argument at least @n@ times and no more than @m@ times.
repeatBounded :: Natural -> Natural -> RegexAST' cs s -> RegexAST' cs s
repeatBounded n m r
  | m < n     = Empty
  | otherwise = Repeat r n (Just (m - n))

-- | Normalize 'RegexAST's so that if @a@ and @b@ are equivalent up to reassociation, @normalize a
-- == normalize b@. This serves no real purpose other than testing the parser without relying on the
-- specific AST that is generated.
--
-- The normalized AST satisfies the following conditions:
-- 1. The first argument to a 'Seq' is not a 'Seq'.
-- 2. The first argument to an 'Alt' is not an 'Alt'.
-- 3. The argument to 'Chars' is normalized.
normalize :: RegexAST -> RegexAST
normalize ast@Empty      = ast
normalize ast@(Splice _) = ast
normalize (Chars cs)     = Chars (CS.normalize cs)

normalize (Repeat _  n (Just m)) | m < n = Empty
normalize (Repeat re n m) = Repeat (normalize re) n m

normalize (Seq ast1 ast2) = normalizeSeq ast1 (normalize ast2)
  where
    normalizeSeq (Seq ast1' ast2') rest = normalizeSeq ast1' (normalizeSeq ast2' rest)
    normalizeSeq ast rest = Seq (normalize ast) rest

normalize (Alt ast1 ast2) = normalizeAlt ast1 (normalize ast2)
  where
    normalizeAlt (Alt ast1' ast2') rest = normalizeAlt ast1' (normalizeAlt ast2' rest)
    normalizeAlt ast rest = Alt (normalize ast) rest
