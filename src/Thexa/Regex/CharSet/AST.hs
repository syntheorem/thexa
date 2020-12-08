module Thexa.Regex.CharSet.AST where

import PreludePrime
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

char :: Char -> CharSetAST
char = Chars . CS.singleton

chars :: CharSet -> CharSetAST
chars = Chars

string :: String -> CharSetAST
string = chars . fromString

range :: Char -> Char -> CharSetAST
range l u = chars (CS.range l u)

union :: CharSetAST -> CharSetAST -> CharSetAST
union (Chars rs1) (Chars rs2)     = Chars (CS.union rs1 rs2)
union cs1         (Union cs2 cs3) = union (union cs1 cs2) cs3
union cs1         cs2             = Union cs1 cs2

unions :: [CharSetAST] -> CharSetAST
unions = foldl' union (Chars CS.empty)

difference :: CharSetAST -> CharSetAST -> CharSetAST
difference (Chars rs1) (Chars rs2) = Chars (CS.difference rs1 rs2)
difference cs1         cs2         = Union cs1 cs2

complement :: CharSetAST -> CharSetAST
complement = difference (Chars CS.full)
