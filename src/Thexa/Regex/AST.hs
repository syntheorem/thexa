module Thexa.Regex.AST where

import PreludePrime

import Data.Foldable (foldl1, foldr1)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.String (IsString(fromString))

type Regex = Regex_ Void
type RegexQ = Regex_ String

data Regex_ s
  -- | Matches a single character which matches the set.
  = RECharSet (CharSet_ s)

  -- | Matches strings which match either regex.
  | REAlt (Regex_ s) (Regex_ s)

  -- | Matches strings which match the first regex and then the second.
  | RESeq (Regex_ s) (Regex_ s)

  -- | Matches strings which match the regex @n@ or more times.
  --
  -- If the second argument is provided, the number of times it will match after the minimum is
  -- bounded by that argument. So given @n@ and @Just m@, this will match the regex between @n@ and
  -- @n + m@ times, inclusive.
  | RERepeat (Regex_ s) Natural (Maybe Natural)

  -- | Matches the empty string.
  | REEmpty

  -- | The name of a Haskell value that will be spliced in using Template Haskell.
  --
  -- The type parameter allows us to use this data structure for parsing (where we may encounter
  -- splices) and for representing the final regex with all splices already evaluated. For parsing,
  -- the parameter should be a 'String' with the (possibly qualified) name of the Haskell value to
  -- splice. For fully evaluated regexes, the type should be 'Void' to indicate that there are no
  -- splices. Since this constructor is strict in its parameter, the type checker knows that it
  -- can't be used when the parameter is 'Void', so there is no need to pattern match on it.
  | RESplice !s

  deriving (Eq, Show)

type CharSet = CharSet_ Void
type CharSetQ = CharSet_ String

data CharSet_ s
  -- | Matches a specific character.
  = CSSingle Char

  -- | Matches any character with a code point between the given characters, inclusive.
  | CSRange Char Char

  -- | Matches any character that matches either set.
  | CSUnion (CharSet_ s) (CharSet_ s)

  -- | Matches characters which match the first set but not the second.
  | CSDiff (CharSet_ s) (CharSet_ s)

  -- | The name of a Haskell value that will be spliced in using Template Haskell.
  --
  -- See documentation for 'RESplice' for more information.
  | CSSplice !s

  deriving (Eq, Show)

instance IsString (Regex_ s) where
  fromString = reString

instance IsString (CharSet_ s) where
  fromString []     = error "empty CharSet"
  fromString (c:cs) = csUnionList (map CSSingle (c:|cs))

reChar :: Char -> Regex_ s
reChar = RECharSet . CSSingle

reString :: String -> Regex_ s
reString = reSeqList . map reChar

reSeq :: Regex_ s -> Regex_ s -> Regex_ s
reSeq r1 (RESeq r2 r3) = reSeq (reSeq r1 r2) r3
reSeq r1 r2            = RESeq r1 r2

reSeqList :: [Regex_ s] -> Regex_ s
reSeqList [] = REEmpty
reSeqList rs = foldl1 reSeq rs

reAlt :: Regex_ s -> Regex_ s -> Regex_ s
reAlt (REAlt r1 r2) r3 = reAlt r1 (reAlt r2 r3)
reAlt r1            r2 = REAlt r1 r2

reAltList :: [Regex_ s] -> Regex_ s
reAltList [] = REEmpty
reAltList rs = foldr1 reAlt rs

rePlus :: Regex_ s -> Regex_ s
rePlus = reRepeatUnbounded 1

reStar :: Regex_ s -> Regex_ s
reStar = reRepeatUnbounded 0

reOpt :: Regex_ s -> Regex_ s
reOpt = reRepeatBounded 0 1

reRepeat :: Natural -> Regex_ s -> Regex_ s
reRepeat n r = RERepeat r n (Just 0)

reRepeatUnbounded :: Natural -> Regex_ s -> Regex_ s
reRepeatUnbounded n r = RERepeat r n Nothing

reRepeatBounded :: Natural -> Natural -> Regex_ s -> Regex_ s
reRepeatBounded n m r
  | m < n     = REEmpty
  | otherwise = RERepeat r n (Just (m - n))

csUniverse :: CharSet_ s
csUniverse = CSRange minBound maxBound

csInverse :: CharSet_ s -> CharSet_ s
csInverse = CSDiff csUniverse

csUnion :: CharSet_ s -> CharSet_ s -> CharSet_ s
csUnion c1 (CSUnion c2 c3) = csUnion (csUnion c1 c2) c3
csUnion c1 c2              = CSUnion c1 c2

csUnionList :: NonEmpty (CharSet_ s) -> CharSet_ s
csUnionList = foldl1 csUnion
