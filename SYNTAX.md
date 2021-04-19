Regular Expression Syntax and Semantics
=======================================

This document describes the syntax and semantics of the regular expressions parsed by Thexa's `re` and `cs` quasiquoters. A general understanding of regular expressions is assumed, and indeed the syntax is largely a subset of the syntax used by typical regex libraries.

Characters
----------

In this document, *character* refers to a single Unicode *code point* (the same as a `Char` in Haskell). This is important because some sequences of code points (known as a *grapheme cluster*) may be rendered as a single glyph, making them look like a single character even though they are comprised of multiple code points (for example, emojis). If this sort of construct is used within a character set, it becomes confusing because, although your editor may render a single glyph, multiple code points will actually be added to the character set.

Additionally, note that no normalization (in the sense of Unicode normalization) is done on the input, so if you want to match multiple graphme clusters that are equivalent under normalization, you must do that manually.

Whitespace
----------

All whitespace in a regular expression is ignored, with the exception of an escaped space character (see **CharEscape**) or when included in strings (see **RegexString**). This includes all characters in the Unicode "Space Separator" Category, as well as the ASCII control characters for tab (`0x9`), newline (`0xA`), vertical tab (`0xB`), form feed (`0xC`), and carriage return (`0xD`).

Regular Expressions
-------------------

> **Regex** :
>>  ( _RegexAtom_ _RegexQuantifier_? )\* ( `|` _Regex_ )?
>
> **RegexAtom** :
>>  `(` _Regex_ `)` | `[` _CharSet_ `]` | _RegexChar_ | _RegexString_ | _RegexSplice_
>
> **RegexChar** :
>>  _RegexEscape_ | \[^`(){}[]*+?|"\`\]

At its most basic, a regex matches a particular sequence of characters; e.g., the regex `abc` matches the string `abc`. Any non-whitespace character without special meaning in the regex syntax is interpreted as itself. The list of characters with special meaning is given in the second pattern of **RegexChar**.

A regex can be empty, in which case it will only match the empty string. Or, if you consider a regex in terms of matching a subset of a string, it will always match immediately without consuming any input.

Regex *alternation* is introduced by separating two regexes with the `|` character, in which case the regex will match if and only if at least one of the alternatives matches; e.g., the regex `ab|cd` will match the string `ab` and the string `cd`.

It is also possible to match any character that is contained in a *character set*. This is done using the **CharSet** syntax enclosed in square brackets.

Enclosing a regex in parentheses is useful to affect the way it is parsed (see the section on quantifiers), but otherwise has no semantic meaning for the enclosed regex.

> **RegexString** :
>>  `"` ( \[^`"\`\] | _CharEscape_ | `\"` )\* `"`

A string enclosed in double-quotes can be used to match a literal string of characters without escaping special characters or ignoring whitespace. The only characters which must be escaped within a string are `\` and `"`.

> **RegexQuantifier** :
>>  `*` | `+` | `?` | _RegexRepeater_
>
> **RegexRepeater** :
>>  `{` ( _Number_ | _Number_ `,` _Number_ | _Number_ `,` | `,` _Number_ ) `}`
>
> **Number** :
>>  \[`0`-`9`\]\+

A *quantifier* can be used as a suffix to allow a regex to match multiple times. Note that the quantifier only applies to the previous **RegexAtom**, not the entire preceeding regex. So `ab+` matches `abb` but not `abab`. To apply it to a larger regex, simply enclose the regex in parentheses or use a double-quoted string.

The most general form of the quantifier is the **RegexRepeater** enclosed in curly braces:

- `{n}`: matches exactly `n` repetitions
- `{n,}`: matches at least `n` repetitions
- `{n,m}`: matches at least `n` repetitions and at most `m` repetitions (it is an error if `m < n`)
- `{,m}`: equivalent to `{0,m}`

The other quantifiers can be expressed in terms of the general repeater, and are provided for convenience:

- `*`: equivalent to `{0,}`
- `+`: equivalent to `{1,}`
- `?`: equivalent to `{0,1}`

Character Sets
--------------

> **CharSet** :
>>  `^`? _CharSetAtom_\* | _CharSetAtom_\+ _CharSetDiff_?
>
> **CharSetAtom** :
>>  `[` _CharSet_ `]` | _CharSetRange_ | _CharSetChar_ | _CharSetSplice_
>
> **CharSetChar** :
>>  _CharSetEscape_ | \[^`[]^-\`\]

A *character set* is exactly what it sounds like: a set of characters. A charset matches a character if and only if that character is in the set. Syntactically, a charset is written as a sequence of characters; each character in the sequence is added to the set. So `[ab]` matches `a` or `b` but not `c`.

Characters which have special meaning within a charset must be escaped; the list of these characters is given in the second pattern of **CharSetChar**. Note that characters that are special within a regex but not special within a charset do not have to be escaped (and in fact cannot be escaped).

Like a regex, a charset can be empty. However, an empty charset doesn't match any characters, so a regex containing an empty charset can never match any string. Since this is almost certainly never intended, the library will report an error if an empty charset is used in a regex.

Charsets can be nested using square brackets. This is rarely necessary, but potentially useful for readability or when using the `^` operator.

A charset is negated when it begins with `^`. In this case, the charset is the set complement of the charset following the `^`, e.g. `[^ab]` matches any character *except* for `a` and `b`. Since the subsequent charset can be empty, `[^]` can be used to match any character.

> **CharSetRange** :
>>  _CharSetChar_ `-` _CharSetChar_

Ranges can be used to represent a set of contiguous characters, e.g. `[a-c]` will match `a`, `b`, or `c`. This works by taking the integer code points of the start and end characters, and including every code point in between them, so to avoid confusion it is best to only use this for well known character ranges (like `a-z` or `0-9`) or with explicit code point escapes (like `\x00-\xFF`).

> **CharSetDiff** :
>>  `^` _CharSetAtom_\*

Charsets also support set difference using the `^` operator to separate two charsets. Such a charset matches characters that are in the first set but not in the second, e.g. `[[a-d] ^ b]` matches `a`, `c`, and `d`.

Note that to avoid ambiguity, it is not allowed to combine multiple `^` operations in a single charset, like in `[^ x ^ y ^ z]`. However, multiple uses are allowed if they are explicitly nested, like in `[^[x ^ [y ^ z]]]`.

Escaped Characters
------------------

> **RegexEscape** :
>>  _CharEscape_ | `\` \[`(){}[]*+?|" `\]
>
> **CharSetEscape** :
>>  _CharEscape_ | `\` \[`[]^- `\]

Characters with special meaning can be escaped by putting `\` in front of them. Escapes are parsed as a single token, so the `\` and the escaped character cannot be separated by whitespace. Note that the space character (`0x20`) can also be escaped in order to match a space character that would otherwise be ignored. Other whitespace characters must be escaped using a **CharEscape**.

> **CharEscape** :
>>  `\n` | `\t` | `\r` | `\f` | `\v` | `\0` | `\\` |\
>>   _ControlCodeEscape_ | _AsciiEscape_ | _UnicodeEscape_

These special escapes map to particular characters:

- `\0`: null character (`0x0`)
- `\t`: tab (`0x9`)
- `\n`: newline (`0xA`)
- `\v`: vertical tab (`0xB`)
- `\f`: form feed (`0xC`)
- `\r`: carriage return (`0xD`)
- `\\`: backslash (`0x5C`)

> **ControlCodeEscape** :
>>  `\x{` _AsciiControlCode_ `}`
>
> **AsciiControlCode** :
>>  `NUL` | `SOH` | `STX` | `ETX` | `EOT` | `ENQ` | `ACK` | `BEL` | `BS` | `HT` | `LF` | \
>>  `VT` | `FF` | `CR` | `SO` | `SI` | `DLE` | `DC1` | `DC2` | `DC3` | `DC4` | `NAK` | \
>>  `SYN` | `ETB` | `CAN` | `EM` | `SUB` | `ESC` | `FS` | `GS` | `RS` | `US` | `DEL`

Any ASCII control code (code points `0x0`-`0x1F`, `0x7F`) can be escaped using the shorthand name of the control code between `\x{` and `}`. Note that there can be no whitespace in the `\x{` token, but whitespace is ignored between the braces as long as it comes before or after the **AsciiControlCode** token.

> **AsciiEscape** :
>>  `\x` _HexDigit_ _HexDigit_
>
> **UnicodeEscape** :
>>  `\u{` `0x`? _HexDigit_\+ `}`
>
> **HexDigit** :
>>  [`0`-`9` `a`-`f` `A`-`F`]

It is also possible to use an escape to explicitly specify a particular code point by number. The most general form of this is the **UnicodeEscape**, where a hexadecimal number between `\u{` and `}` is used to specify the numeric code point. This hexadecimal number can optionally start with `0x` for clarity, but it is interpreted as hexadecimal either way. It is an error if the number is larger than `0x10FFFF`, the maximum Unicode code point.

Similarly to the **ControlCodeEscape**, there can be no whitespace in the `\u{` token, but whitespace is ignored between the braces as long as it comes before or after the hexadecimal number.

A shorthand version of this is available for code points in the range `0x0`-`0xFF`. It uses `\x` followed by exactly two hexadecimal digits (this is parsed as a single token, so no whitespace is allowed within it). This is called an **AsciiEscape** because it has enough range to express any ASCII character (i.e., the Basic Latin block in Unicode), but it can also be used for characters in the Latin-1 Supplement block.

Splices
-------

> **RegexSplice** :
>>  `{{` _HaskellVar_ `}}`
>
> **CharSetSplice** :
>>  `[:` _HaskellVar_ `:]`

When creating a lexer, it can be useful to factor out common portions of a regex into a Haskell variable. These variables can then be *spliced* into a regex or charset quasiquotation using this splicing syntax. The variable must be in scope at the site of the quasiquotation usage.

The **HaskellVar** production corresponds to the **qvarid** production in the (Haskell syntax reference)[https://www.haskell.org/onlinereport/haskell2010/haskellch10.html], and can therefore be qualified with a module name.

The variable must have the type `Regex` or `CharSet` when used in a **RegexSplice** or **CharSetSplice**, respectively. Note that this means that a `CharSet` variable cannot be spliced directly into a regex using the **RegexSplice** syntax, but must instead be enclosed in an extra pair of brackets, like `[[:charSetVar:]]`. This syntax was chosen to resemble POSIX character classes in other regex syntaxes.

Note that the splice delimiters are parsed as a single token, and thus cannot have whitespace between the two characters. Additionally, in accordance with the **qvarid** production, there can be no whitespace inside the **HaskellVar**; all other whitespace between the delimiters is ignored as usual.

Finally, there is an ambiguity between the **CharSetSplice** syntax and a bracketed **CharSet** beginning with `:`. TODO: FINISH THIS EXPLANATION

Omissions
---------

As is typical for a lexer, these regexes are intended to be compiled into a simple finite state machine, and don't support more advanced regex features such as lookahead, lookbehind, capture groups, or backreferences. This includes other features that rely on lookahead or lookbehind, such as matching the beginning or end of a line with `^` or `$`, or using `\b` to match word boundaries.

There are also no built-in character classes such as `\s` or `[[:space:]]` to match whitespace characters. These are intended to be replaced by splices, and as such some common character classes are included in the `Thexa.CharClass` module as `CharSet` variables. Note also that `.` matches a literal period character rather than any character (or any non-newline character, depending on the regex engine settings). To match any character, use `[^]` (or `[^\n]`) instead.
