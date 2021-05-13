module Thexa.Internal.Unicode.Properties where

-- In a few places, we insert values in a map that we've looked up from another map using (!), which
-- will error if the value is not in the map. By using a strict map, we ensure these values will be
-- evaluated so that our tests will trigger any potential errors as long as we evaluate the maps.
import Data.Map.Strict (Map, (!))
import Data.Map.Strict qualified as Map

import Thexa.CharSet (CharSet)
import Thexa.Internal.Unicode.Parser (readUnicodeDataFile)

-- | Mapping from a Unicode property name to the set of characters with that property.
type PropertyMap = Map String CharSet

blocks :: PropertyMap
blocks = $$(readUnicodeDataFile "unicode/Blocks.txt")

scripts :: PropertyMap
scripts = $$(readUnicodeDataFile "unicode/Scripts.txt")

generalCategories :: PropertyMap
generalCategories = Map.unionWithKey
  (\k _ _ -> error ("duplicate general category key: "<>k))
  generalCategoriesLong
  generalCategoriesAbbr

generalCategoriesLong :: PropertyMap
generalCategoriesLong = Map.fromList (map lookupAbbr gcAliases)
  where
    lookupAbbr (catAbbr, cat) = (cat, generalCategoriesAbbr ! catAbbr)

    -- Copied from http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table
    gcAliases =
      [ ("C",  "Other")
      , ("Cc", "Control")
      , ("Cf", "Format")
      , ("Cn", "Unassigned")
      , ("Co", "Private_Use")
      , ("Cs", "Surrogate")
      , ("L",  "Letter")
      , ("LC", "Cased_Letter")
      , ("Ll", "Lowercase_Letter")
      , ("Lm", "Modifier_Letter")
      , ("Lo", "Other_Letter")
      , ("Lt", "Titlecase_Letter")
      , ("Lu", "Uppercase_Letter")
      , ("M",  "Mark")
      , ("Mc", "Spacing_Mark")
      , ("Me", "Enclosing_Mark")
      , ("Mn", "Nonspacing_Mark")
      , ("N",  "Number")
      , ("Nd", "Decimal_Number")
      , ("Nl", "Letter_Number")
      , ("No", "Other_Number")
      , ("P",  "Punctuation")
      , ("Pc", "Connector_Punctuation")
      , ("Pd", "Dash_Punctuation")
      , ("Pe", "Close_Punctuation")
      , ("Pf", "Final_Punctuation")
      , ("Pi", "Initial_Punctuation")
      , ("Po", "Other_Punctuation")
      , ("Ps", "Open_Punctuation")
      , ("S",  "Symbol")
      , ("Sc", "Currency_Symbol")
      , ("Sk", "Modifier_Symbol")
      , ("Sm", "Math_Symbol")
      , ("So", "Other_Symbol")
      , ("Z",  "Separator")
      , ("Zl", "Line_Separator")
      , ("Zp", "Paragraph_Separator")
      , ("Zs", "Space_Separator")
      ]

generalCategoriesAbbr :: PropertyMap
generalCategoriesAbbr = foldl' (flip insertDerivedCategory) gcMap derivedCategories
  where
    gcMap = $$(readUnicodeDataFile "unicode/DerivedGeneralCategory.txt")

    insertDerivedCategory (catName, subcats) =
      Map.insert catName (foldMap (gcMap !) subcats)

    -- DerivedGeneralCategory.txt only lists the subcategory for each character, but there are also
    -- categories consisting of the union of several subcategories.
    --
    -- Copied from http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table
    derivedCategories =
      [ ("C",  ["Cc", "Cf", "Cs", "Co", "Cn"])
      , ("L",  ["Lu", "Ll", "Lt", "Lm", "Lo"])
      , ("LC", ["Lu", "Ll", "Lt"])
      , ("M",  ["Mn", "Mc", "Me"])
      , ("N",  ["Nd", "Nl", "No"])
      , ("P",  ["Pc", "Pd", "Ps", "Pe", "Pi", "Pf", "Po"])
      , ("S",  ["Sm", "Sc", "Sk", "So"])
      , ("Z",  ["Zs", "Zl", "Zp"])
      ]
