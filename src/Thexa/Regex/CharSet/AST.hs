module Thexa.Regex.CharSet.AST where

import PreludePrime
import Data.Foldable (foldl1)
import Data.String (IsString(fromString))

import Thexa.Regex.CharSet (CharSet)
import Thexa.Regex.CharSet qualified as CS

-- | Syntax tree for a char set.
--
-- This type is essentially only used for parsing. Once splices are evaluated, a 'CharSetAST' can
-- always trivially be collapsed to a 'CharSet'.
data CharSetAST
  -- | Matches any character in the set.
  = Chars CharSet

  -- | Matches any character that matches either set.
  | Union CharSetAST CharSetAST

  -- | Matches characters which match the first set but not the second.
  | Diff CharSetAST CharSetAST

  -- | The (possibly qualified) name of a Haskell value that will be spliced in using Template
  -- Haskell. The type of the named value is expected to be 'CharSet'.
  | Splice String

  deriving (Eq, Show)

instance Semigroup CharSetAST where
  (<>) = union

instance Monoid CharSetAST where
  mempty = chars CS.empty

-- | Union of all characters in the string.
instance IsString CharSetAST where
  fromString = string

-- | Class for types which can be built from a 'CharSet'.
class IsCharSet cs where
  fromCharSet :: CharSet -> cs

instance IsCharSet CharSetAST where
  fromCharSet = Chars

instance IsCharSet CharSet where
  fromCharSet = id

fromAST :: CharSetAST -> Maybe CharSet
fromAST = \case
  Splice _      -> Nothing
  Chars cs      -> Just cs
  Union cs1 cs2 -> CS.union      <$> fromAST cs1 <*> fromAST cs2
  Diff  cs1 cs2 -> CS.difference <$> fromAST cs1 <*> fromAST cs2

empty :: CharSetAST
empty = chars CS.empty

char :: Char -> CharSetAST
char = Chars . CS.singleton

chars :: CharSet -> CharSetAST
chars = Chars

string :: String -> CharSetAST
string = chars . fromString

range :: Char -> Char -> CharSetAST
range l u = chars (CS.range l u)

union :: CharSetAST -> CharSetAST -> CharSetAST
union (Chars cs1) (Chars cs2) = Chars (CS.union cs1 cs2)
union ast1        ast2        = Union ast1 ast2

unions :: [CharSetAST] -> CharSetAST
unions [] = Chars CS.empty
unions cs = foldl1 union cs

difference :: CharSetAST -> CharSetAST -> CharSetAST
difference (Chars cs1) (Chars cs2) = Chars (CS.difference cs1 cs2)
difference ast1        ast2        = Diff ast1 ast2

complement :: CharSetAST -> CharSetAST
complement = difference (Chars CS.full)

-- | Normalize 'CharSetAST's so that if @a@ and @b@ are equivalent up to reassociation, @normalize a
-- == normalize b@. This serves no real purpose other than testing the parser without relying on the
-- specific AST that is generated.
--
-- The only thing this really modifies is nested trees of 'Union's. Any 'Chars' at the leaves of
-- these trees are combined and bubbled up to the left branch of the root of the 'Union' tree. If
-- the final 'Chars' constructor contains the empty set, then it is eliminated altogether. If it is
-- possible to reduce the entire AST to a single 'Chars' constructor, then it will be. The left
-- branch of a 'Union' will not be 'Union'
normalize :: CharSetAST -> CharSetAST
normalize ast@(Chars  _)   = ast
normalize ast@(Splice _)   = ast
normalize (Diff ast1 ast2) = difference (normalize ast1) (normalize ast2)

normalize (Union ast1 ast2) = case normalize ast2 of
  Chars cs2 | Chars cs1 <- ast1 -> Chars (CS.union cs1 cs2)
  ast2'@(Chars _)               -> normalize (Union ast2' ast1)
  Union (Chars cs) ast2'        -> Union (Chars (CS.union cs ast1cs)) (normalizeUnion ast1 ast2')
  ast2' | CS.null ast1cs        -> ast2'
        | otherwise             -> Union (Chars ast1cs) ast2'
  where
    ast1cs = extractCharSet ast1

    extractCharSet = \case
      Union x y -> CS.union (extractCharSet x) (extractCharSet y)
      Chars cs  -> cs
      _         -> CS.empty

    normalizeUnion (Union x y) rest = normalizeUnion x (normalizeUnion y rest)
    normalizeUnion (Chars _)   rest = rest
    normalizeUnion ast         rest = Union (normalize ast) rest




