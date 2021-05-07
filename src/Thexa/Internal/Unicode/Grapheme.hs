module Thexa.Internal.Unicode.Grapheme where

import Data.Map ((!))

import Thexa.Regex (Regex, re)
import Thexa.Internal.Unicode.Properties (PropertyMap)
import Thexa.Internal.Unicode.Parser (readUnicodeDataFile)

-- | A regular expression that matches a single extended grapheme cluster as defined by the Unicode
-- standard.
--
-- The purpose of this is that it can be used to find grapheme cluster boundaries by starting from
-- one boundary and using the length of the longest match of this regex to get the offset to the
-- next boundary.
--
-- Based on the regex defined by this table:
-- https://www.unicode.org/reports/tr29/tr29-37.html#Table_Combining_Char_Sequences_and_Grapheme_Clusters
grapheme :: Regex
grapheme = [re| \r\n | [[:control:]\r\n] | {{precore}}* {{core}} {{postcore}}* |]
  where
    -- Regexes from https://www.unicode.org/reports/tr29/tr29-37.html#Regex_Definitions
    core           = [re| {{hangulSyllable}}
                        | {{riSequence}}
                        | {{xpictoSequence}}
                        | [^[:control:]\r\n]
                        |]
    postcore       = [re| [[:extend:] [:zwj:] [:spacingMark:]] |]
    precore        = [re| [:prepend:] |]
    riSequence     = [re| [:ri:]{2} |]
    hangulSyllable = [re| [:l:]* ([:v:]+ | [:lv:] [:v:]* | [:lvt:]) [:t:]*
                        | [:l:]+
                        | [:t:]+
                        |]
    xpictoSequence = [re| [:extPicto:] ([:extend:]* [:zwj:] [:extPicto:])* |]

    -- CharSets from
    -- https://www.unicode.org/reports/tr29/tr29-37.html#Grapheme_Cluster_Break_Property_Values
    control     = graphemeBreakProps ! "Control"
    extend      = graphemeBreakProps ! "Extend"
    zwj         = graphemeBreakProps ! "ZWJ"
    ri          = graphemeBreakProps ! "Regional_Indicator"
    prepend     = graphemeBreakProps ! "Prepend"
    spacingMark = graphemeBreakProps ! "SpacingMark"
    l           = graphemeBreakProps ! "L"
    v           = graphemeBreakProps ! "V"
    t           = graphemeBreakProps ! "T"
    lv          = graphemeBreakProps ! "LV"
    lvt         = graphemeBreakProps ! "LVT"

    extPicto = emojiProps ! "Extended_Pictographic"

graphemeBreakProps :: PropertyMap
graphemeBreakProps = $$(readUnicodeDataFile "unicode/GraphemeBreakProperty.txt")

emojiProps :: PropertyMap
emojiProps = $$(readUnicodeDataFile "unicode/emoji-data.txt")
