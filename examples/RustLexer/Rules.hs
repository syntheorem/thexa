{-# OPTIONS_GHC -Wno-missing-signatures #-}
module RustLexer.Rules where

import Control.Monad.State.Strict (State, evalState)
import Control.Monad.Except (ExceptT, throwError, runExceptT)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BS
import Data.ByteString.Lazy qualified as BS (toStrict)
import Data.ByteString.Char8 qualified as Char8
import Data.Char (digitToInt)
import Data.String.UTF8 (UTF8)
import Data.String.UTF8 qualified as UTF8

-- We're hiding a few exports from Thexa so we can redefine them to be specific to our lexer.
import Thexa hiding (Action, Condition, Lexer, Rule, runLexer)
import Thexa qualified
import Thexa.Regex qualified as RE

-- First, we will define the types used by our lexer.

-- This is the type of the tokens emitted by our lexer, which associates the actual token data
-- (defined below) with its location in the source using Thexa's Span type.
data Token = Token !TokenData !Span
  deriving (Eq, Show)

data TokenData
  -- Keywords
  = KW_as | KW_async | KW_await | KW_break | KW_const | KW_continue | KW_crate | KW_dyn | KW_else | KW_enum
  | KW_extern | KW_false | KW_fn | KW_for | KW_if | KW_impl | KW_in | KW_let | KW_loop | KW_match | KW_mod
  | KW_move | KW_mut | KW_pub | KW_ref | KW_return | KW_self | KW_Self | KW_static | KW_struct | KW_super
  | KW_trait | KW_true | KW_type | KW_unsafe | KW_use | KW_where | KW_while

  -- Symbols
  | SYM_Plus | SYM_Minus | SYM_Star | SYM_Slash | SYM_Percent | SYM_Caret | SYM_Not | SYM_And | SYM_Or
  | SYM_AndAnd | SYM_OrOr | SYM_Shl | SYM_Shr | SYM_PlusEq | SYM_MinusEq | SYM_StarEq | SYM_SlashEq
  | SYM_PercentEq | SYM_CaretEq | SYM_AndEq | SYM_OrEq | SYM_ShlEq | SYM_ShrEq | SYM_Eq | SYM_EqEq | SYM_Ne
  | SYM_Gt | SYM_Lt | SYM_Ge | SYM_Le | SYM_At | SYM_Underscore | SYM_Dot | SYM_DotDot | SYM_DotDotDot
  | SYM_DotDotEq | SYM_Comma | SYM_Semi | SYM_Colon | SYM_PathSep | SYM_RArrow | SYM_FatArrow | SYM_Pound
  | SYM_Dollar | SYM_Question

  -- Delimiters
  | DELIM_OpenBrace | DELIM_CloseBrace
  | DELIM_OpenBracket | DELIM_CloseBracket
  | DELIM_OpenParen | DELIM_CloseParen

  -- Literals
  | LIT_Char !Char
  | LIT_ByteChar !Word8
  -- We're wrapping the string contents with the UTF8 type so the Show instance will properly decode
  -- it. Since all the other tokens are ASCII-only, we don't bother to use UTF8 anywhere else.
  | LIT_String !(UTF8 ByteString)
  | LIT_ByteString !ByteString
  -- Normally we would also parse int and float literals into actual numeric types, but for
  -- demonstration purposes it's not really necessary.
  | LIT_Int !ByteString
  | LIT_Float !ByteString

  -- Lifetimes and identifiers
  | TOK_Lifetime !(Maybe ByteString)
  | TOK_Ident !ByteString
  deriving (Eq, Show)

-- A lexer can be defined with multiple modes. Each lexer rule specifies which modes it is active
-- in, and the lexer operates in one mode at a time. Here we're primarily using modes to handle
-- strings.
data Mode
  -- The first constructor is the default mode, which is active when we first start lexing input.
  -- Any rule which doesn't explicitly define which modes it operates in is only active in the
  -- default mode.
  = MainMode
  | StringMode
  | ByteStringMode
  | RawStringMode
  | RawByteStringMode
  | BlockCommentMode
  -- These derives are necessary for a type to work as a lexer mode.
  deriving (Eq, Ord, Enum, Bounded)

-- This is the custom state for our lexer, which we're using to lex strings.
data RLState = RLState
  -- Rust has a construct called raw strings, which allow one to write strings without escaping
  -- special characters. They are written like r#"string contents"#, where there can be any number
  -- of # characters and the string only ends when the number of # characters at the end matches the
  -- number of # characters at the beginning, so we track the number of # characters here so we can
  -- know when the string ends.
  { rawStringHashCount :: !Int
  -- Since we use multiple rules to lex strings, we need to keep track of the position where the
  -- string started so we can set the span correctly when we finally emit the string token.
  , stringStartPos :: !Position
  -- While in the string modes, we use a ByteString builder to accumulate the contents of the string
  -- token.
  , stringBuilder :: !BS.Builder
  }

-- Custom error type.
data LexerError
  = InvalidInput Position
  | NonAsciiChar Position
  | InvalidEscape Span
  | UnfinishedString Position
  | UnfinishedBlockComment

-- This is the monad in which our lexer runs. We use the State monad to maintain the state of the
-- lexer, with an ExceptT transformer to provide error handling. The first parameter to LexerState
-- is our input string type, which in this case is a strict ByteString. Lazy ByteStrings and normal
-- Strings are also supported.
type LexerM = ExceptT LexerError (State (LexerState ByteString Mode RLState Token))

-- Here we define type aliases for types exported by Thexa, but specialized to the specific types of
-- our lexer.

-- This is the actual type of our lexer.
type Lexer = Thexa.Lexer ByteString Mode RLState Token LexerM

-- This is the type of our lexer actions that are executed when a lexer rule is matched. Expanding
-- Thexa.Action, we get the type:
--
-- Span -> ByteString -> LexerM ()
--
-- The first argument is the positional span of the input that was matched, and the second is the
-- contents of the input that was matched.
type Action = Thexa.Action ByteString LexerM

-- This is the type of our lexer conditions. Conditions can be used to restrict when a rule matches
-- in cases where regexes and modes are insufficient. Expanding Thexa.Condition, we get:
--
-- Span -> ByteString -> LexerState ByteString Mode RLState Token -> Bool
--
-- The first two arguments are the same as for Action, and the third argument is the state of the
-- lexer after the match. The match is only used if the Condition evaluates to True.
type Condition = Thexa.Condition ByteString Mode RLState Token

-- This is the type of the rules for our lexer.
type Rule = Thexa.Rule Mode Condition Action

space = [cs|\t\n\v\f\r\ \u{85}\u{200E}\u{200F}\u{2028}\u{2029}|]

decDigit = [cs|0-9|]
binDigit = [cs|0-1|]
octDigit = [cs|0-7|]
hexDigit = [cs|0-9 a-f A-F|]

decLit = [re| [:decDigit:] [[:decDigit:]_]* |]
binLit = [re| 0b [[:binDigit:]_]* [:binDigit:] [[:binDigit:]_]* |]
octLit = [re| 0o [[:octDigit:]_]* [:octDigit:] [[:octDigit:]_]* |]
hexLit = [re| 0x [[:hexDigit:]_]* [:hexDigit:] [[:hexDigit:]_]* |]

floatExp = [re| [eE] [+\-]? [[:decDigit:]_]* [:decDigit:] [[:decDigit:]_]* |]

intSuffix = [re| [iu](8|16|32|64|128|size) |]
floatSuffix = [re| f(32|64) |]

ident     = [re| [a-z A-Z][:identChar:]* | _[:identChar:]+ |]
identChar = [cs| a-z A-Z 0-9 _ |]

ascii = [cs|\x00-\x7F|]

basicEscape   = [re| \\ [nrt0'"\\] |]
byteEscape    = [re| \\ [:hexDigit:]{2} |]
asciiEscape   = [re| \\ [:octDigit:] [:hexDigit:] |]
unicodeEscape = [re| \\u \{ {{unicodeHexLit}} \} |]
unicodeHexLit = [re| 10[:hexDigit:]{4} | 0[:hexDigit:]{5} | [:hexDigit:]{1,5} |]

outOfBoundsEscape = [re| {{byteEscape}} | \\u \{ [:hexDigit:]+ \} |]

lexerRules :: [Rule]
lexerRules =
  [ [re| [:space:]+ |]
      & skipMatch

  , [re| // [^\n]* |]
      & skipMatch

  , [re| "/*" |]
      `inModes` [MainMode, BlockCommentMode]
      `onMatch` [|| \_ _ -> pushMode BlockCommentMode ||]

  , [re| "*/" |]
      `inMode` BlockCommentMode
      `onMatch` [|| \_ _ -> popMode ||]

  , [re| [^*/]+ | "*" | "/" |]
      `inMode` BlockCommentMode
      & skipMatch

  , [re| ( {{decLit}} | {{binLit}} | {{octLit}} | {{hexLit}} ) {{intSuffix}}? |]
      `onMatch` [|| emitTokenData LIT_Int ||]

  , [re| {{decLit}}. |]
      `notFollowedBy` [re| . | _ | {{ident}} |]
      `onMatch` [|| emitTokenData LIT_Float ||]

  , [re| {{decLit}} {{floatExp}}
       | {{decLit}}.{{decLit}} {{floatExp}}?
       | {{decLit}} (.{{decLit}})? {{floatExp}}? {{floatSuffix}}
       |]
      `onMatch` [|| emitTokenData LIT_Float ||]

  , [re| '[^'\n\r\t\\]' |]
      `onMatch` [|| emitTokenData (LIT_Char . bsHeadToChar) ||]

  , [re| '{{basicEscape}}' |]
      `onMatch` [|| emitTokenData (LIT_Char . basicEscapeToChar . BS.drop 2) ||]

  , [re| '{{asciiEscape}}' |]
      `onMatch` [|| emitTokenData (LIT_Char . hexCodeToChar . bsSlice 3 1) ||]

  , [re| b'[[:ascii:] ^ '\n\r\t\\]' |]
      `onMatch` [|| emitTokenData (LIT_ByteChar . charToByte . bsHeadToChar) ||]

  , [re| b'{{basicEscape}}' |]
      `onMatch` [|| emitTokenData (LIT_ByteChar . charToByte . basicEscapeToChar . BS.drop 3) ||]

  , [re| b'{{byteEscape}}' |]
      `onMatch` [|| emitTokenData (LIT_ByteChar . charToByte . hexCodeToChar . bsSlice 4 1) ||]

  , [re| b?'({{outOfBoundsEscape}} | \\[^])' |]
      `onMatch` [|| invalidEscapeError ||]

  , [re| \" |]
      `onMatch` [|| beginString 1 StringMode ||]

  , [re| [^"\\]+ |]
      `inMode` StringMode
      `onMatch` [|| appendToStringBuilder ||]

  , [re| {{basicEscape}} |]
      `inMode` StringMode
      `onMatch` [|| appendCharToStringBuilder (basicEscapeToChar . BS.drop 1) ||]

  , [re| {{asciiEscape}} |]
      `inMode` StringMode
      `onMatch` [|| appendCharToStringBuilder (hexCodeToChar . BS.drop 2) ||]

  , [re| {{unicodeEscape}} |]
      `inMode` StringMode
      `onMatch` [|| appendCharToStringBuilder (hexCodeToChar . bsSlice 3 1) ||]

  , [re| \" |]
      `inMode` StringMode
      `onMatch` [|| endString (LIT_String . UTF8.fromRep) ||]

  , [re| b\" |]
      `onMatch` [|| beginString 2 ByteStringMode ||]

  , [re| [[:ascii:] ^ "\\]+ |]
      `inMode` ByteStringMode
      `onMatch` [|| appendToStringBuilder ||]

  , [re| {{basicEscape}} |]
      `inMode` ByteStringMode
      `onMatch` [|| appendCharToStringBuilder (basicEscapeToChar . BS.drop 1) ||]

  , [re| {{byteEscape}} |]
      `inMode` ByteStringMode
      `onMatch` [|| appendCharToStringBuilder (hexCodeToChar . BS.drop 2) ||]

  , [re| \" |]
      `inMode` ByteStringMode
      `onMatch` [|| endString LIT_ByteString ||]

  , [re| \\\n [[:space:] ^ \n]* |]
      `inModes` [StringMode, ByteStringMode]
      & skipMatch

  , [re| {{outOfBoundsEscape}} | \\[^] |]
      `inModes` [StringMode, ByteStringMode]
      `onMatch` [|| invalidEscapeError ||]

  , [re| r#*\" |]
      `onMatch` [|| beginString 2 RawStringMode ||]

  , [re| [^"]+ |]
      `inMode` RawStringMode
      `onMatch` [|| appendToStringBuilder ||]

  , [re| \"#* |]
      `inMode` RawStringMode
      `matchIf` [|| isRawStringEnd ||]
      `onMatch` [|| endString (LIT_String . UTF8.fromRep) ||]

  , [re| br#*\" |]
      `onMatch` [|| beginString 3 RawByteStringMode ||]

  , [re| [[:ascii:] ^ "]+ |]
      `inMode` RawByteStringMode
      `onMatch` [|| appendToStringBuilder ||]

  , [re| [^[:ascii:]] |]
      `inModes` [ByteStringMode, RawByteStringMode]
      `onMatch` [|| nonAsciiCharError ||]

  , [re| \"#* |]
      `inMode` RawByteStringMode
      `matchIf` [|| isRawStringEnd ||]
      `onMatch` [|| endString LIT_ByteString ||]

  , [re| \" |]
      `inModes` [RawStringMode, RawByteStringMode]
      `onMatch` [|| appendToStringBuilder ||]
  ]
  <>
  stringTokens
  [ ("as"       , [|| KW_as              ||])
  , ("async"    , [|| KW_async           ||])
  , ("await"    , [|| KW_await           ||])
  , ("break"    , [|| KW_break           ||])
  , ("const"    , [|| KW_const           ||])
  , ("continue" , [|| KW_continue        ||])
  , ("crate"    , [|| KW_crate           ||])
  , ("dyn"      , [|| KW_dyn             ||])
  , ("else"     , [|| KW_else            ||])
  , ("enum"     , [|| KW_enum            ||])
  , ("extern"   , [|| KW_extern          ||])
  , ("false"    , [|| KW_false           ||])
  , ("fn"       , [|| KW_fn              ||])
  , ("for"      , [|| KW_for             ||])
  , ("if"       , [|| KW_if              ||])
  , ("impl"     , [|| KW_impl            ||])
  , ("in"       , [|| KW_in              ||])
  , ("let"      , [|| KW_let             ||])
  , ("loop"     , [|| KW_loop            ||])
  , ("match"    , [|| KW_match           ||])
  , ("mod"      , [|| KW_mod             ||])
  , ("move"     , [|| KW_move            ||])
  , ("mut"      , [|| KW_mut             ||])
  , ("pub"      , [|| KW_pub             ||])
  , ("ref"      , [|| KW_ref             ||])
  , ("return"   , [|| KW_return          ||])
  , ("self"     , [|| KW_self            ||])
  , ("Self"     , [|| KW_Self            ||])
  , ("static"   , [|| KW_static          ||])
  , ("struct"   , [|| KW_struct          ||])
  , ("super"    , [|| KW_super           ||])
  , ("trait"    , [|| KW_trait           ||])
  , ("true"     , [|| KW_true            ||])
  , ("type"     , [|| KW_type            ||])
  , ("unsafe"   , [|| KW_unsafe          ||])
  , ("use"      , [|| KW_use             ||])
  , ("where"    , [|| KW_where           ||])
  , ("while"    , [|| KW_while           ||])

  , ("+"        , [|| SYM_Plus           ||])
  , ("-"        , [|| SYM_Minus          ||])
  , ("*"        , [|| SYM_Star           ||])
  , ("/"        , [|| SYM_Slash          ||])
  , ("%"        , [|| SYM_Percent        ||])
  , ("^"        , [|| SYM_Caret          ||])
  , ("!"        , [|| SYM_Not            ||])
  , ("&"        , [|| SYM_And            ||])
  , ("|"        , [|| SYM_Or             ||])
  , ("&&"       , [|| SYM_AndAnd         ||])
  , ("||"       , [|| SYM_OrOr           ||])
  , ("<<"       , [|| SYM_Shl            ||])
  , (">>"       , [|| SYM_Shr            ||])
  , ("+="       , [|| SYM_PlusEq         ||])
  , ("-="       , [|| SYM_MinusEq        ||])
  , ("*="       , [|| SYM_StarEq         ||])
  , ("/="       , [|| SYM_SlashEq        ||])
  , ("%="       , [|| SYM_PercentEq      ||])
  , ("^="       , [|| SYM_CaretEq        ||])
  , ("&="       , [|| SYM_AndEq          ||])
  , ("|="       , [|| SYM_OrEq           ||])
  , ("<<="      , [|| SYM_ShlEq          ||])
  , (">>="      , [|| SYM_ShrEq          ||])
  , ("="        , [|| SYM_Eq             ||])
  , ("=="       , [|| SYM_EqEq           ||])
  , ("!="       , [|| SYM_Ne             ||])
  , (">"        , [|| SYM_Gt             ||])
  , ("<"        , [|| SYM_Lt             ||])
  , (">="       , [|| SYM_Ge             ||])
  , ("<="       , [|| SYM_Le             ||])
  , ("@"        , [|| SYM_At             ||])
  , ("_"        , [|| SYM_Underscore     ||])
  , ("."        , [|| SYM_Dot            ||])
  , (".."       , [|| SYM_DotDot         ||])
  , ("..."      , [|| SYM_DotDotDot      ||])
  , ("..="      , [|| SYM_DotDotEq       ||])
  , (","        , [|| SYM_Comma          ||])
  , (";"        , [|| SYM_Semi           ||])
  , (":"        , [|| SYM_Colon          ||])
  , ("::"       , [|| SYM_PathSep        ||])
  , ("->"       , [|| SYM_RArrow         ||])
  , ("=>"       , [|| SYM_FatArrow       ||])
  , ("#"        , [|| SYM_Pound          ||])
  , ("$"        , [|| SYM_Dollar         ||])
  , ("?"        , [|| SYM_Question       ||])

  , ("{"        , [|| DELIM_OpenBrace    ||])
  , ("}"        , [|| DELIM_CloseBrace   ||])
  , ("["        , [|| DELIM_OpenBracket  ||])
  , ("]"        , [|| DELIM_CloseBracket ||])
  , ("("        , [|| DELIM_OpenParen    ||])
  , (")"        , [|| DELIM_CloseParen   ||])
  ]
  <>
  [ [re| {{ident}} |]
      `onMatch` [|| emitTokenData TOK_Ident ||]

  , [re| r#{{ident}} |]
      `onMatch` [|| emitTokenData (TOK_Ident . BS.drop 2) ||]

  , [re| '{{ident}} |]
      `onMatch` [|| emitTokenData (TOK_Lifetime . Just . BS.drop 1) ||]

  , [re| '_ |]
      `onMatch` [|| emitTokenData (const (TOK_Lifetime Nothing)) ||]
  ]

stringTokens :: [(String, SpliceQ TokenData)] -> [Rule]
stringTokens = map stringTokenToRule
  where
    stringTokenToRule (str, token) =
      RE.string str `onMatch` [|| emitTokenData (const $$token) ||]

emitTokenData :: (ByteString -> TokenData) -> Action
emitTokenData f span str = emitToken (Token (f str) span)

beginString :: Int -> Mode -> Action
beginString hashCountOffset mode (Span start _) str = do
  pushMode mode
  modifyUserState \s -> s
    { rawStringHashCount = BS.length str - hashCountOffset
    , stringStartPos = start
    , stringBuilder = mempty
    }

endString :: (ByteString -> TokenData) -> Action
endString f (Span _ end) _ = do
  popMode
  s <- getUserState
  let str = BS.toStrict (BS.toLazyByteString (stringBuilder s))
  let start = stringStartPos s
  emitToken (Token (f str) (Span start end))

appendToStringBuilder :: Action
appendToStringBuilder _ str = modifyUserState \s ->
  s {stringBuilder = stringBuilder s <> BS.byteString str}

appendCharToStringBuilder :: (ByteString -> Char) -> Action
appendCharToStringBuilder toChar _ str = modifyUserState \s ->
  s {stringBuilder = stringBuilder s <> BS.charUtf8 (toChar str)}

isRawStringEnd :: Condition
isRawStringEnd _ str state = BS.length str - 1 == rawStringHashCount (userState state)

lexerError :: (Position -> LexerError) -> Action
lexerError mkError (Span start _) _ = throwError (mkError start)

basicEscapeToChar :: ByteString -> Char
basicEscapeToChar bs =
  case bsHeadToChar bs of
    'n'  -> '\n'
    't'  -> '\t'
    'r'  -> '\r'
    '0'  -> '\0'
    '"'  -> '"'
    '\'' -> '\''
    '\\' -> '\\'
    _    -> error "unknown escape code"

charToByte :: Char -> Word8
charToByte = fromIntegral . fromEnum

hexCodeToChar :: ByteString -> Char
hexCodeToChar bs
  | charCode <= 0x10FFFF = toEnum (fromIntegral charCode)
  | otherwise = error "hex code out of bounds"
  where charCode = digitsToInt 16 (Char8.unpack bs)

-- | Convert a list of digits in the given base to an Integer.
digitsToInt :: Integer -> [Char] -> Integer
digitsToInt base = foldl' go 0 . map (toInteger . digitToInt)
  where go acc digit = acc * base + digit

bsSlice :: Int -> Int -> ByteString -> ByteString
bsSlice front back bs = BS.drop front (BS.take (BS.length bs - back) bs)

bsHeadToChar :: ByteString -> Char
bsHeadToChar bs = case Char8.uncons bs of
  Just (c, _) -> c
  Nothing -> error "empty bytestring"

runLexer :: Lexer -> ByteString -> Either LexerError [Token]
runLexer lexer str
  = flip evalState initState
  $ runExceptT
  $ Thexa.runLexer lexer 4 onFinish onError
  where
    initState = initLexerState str $ RLState
      { rawStringHashCount = 0
      , stringStartPos = Position 0 0 0 0
      , stringBuilder = mempty
      }

    onError = do
      pos <- getPosition
      throwError (InvalidInput pos)

    onFinish = do
      finalMode <- getMode

      when (finalMode `elem` [StringMode, ByteStringMode, RawStringMode, RawByteStringMode]) do
        s <- getUserState
        throwError (UnfinishedString (stringStartPos s))

      when (finalMode == BlockCommentMode) do
        throwError UnfinishedBlockComment

      toList <$> getTokens

-- | Render a 'Position' to a 'String' in a @line:column@ format.
displayPosition :: Position -> String
displayPosition (Position line col _ _) = show line<>":"<>show col

-- | Render a 'Span' to a 'String' in a @line:column-line:column@ format.
displaySpan :: Span -> String
displaySpan (Span start end) = displayPosition start<>"-"<>displayPosition end

displayLexerError :: LexerError -> String
displayLexerError = \case
  InvalidInput pos -> "unrecognized input at "<>displayPosition pos
  NonAsciiChar pos -> "non-ASCII character in byte string literal at "<>displayPosition pos
  InvalidEscape span -> "invalid character escape at "<>displaySpan span
  UnfinishedString pos -> "input ended before string beginning at "<>displayPosition pos<>" was closed"
  UnfinishedBlockComment -> "input ended before block comment was closed"

nonAsciiCharError :: Action
nonAsciiCharError (Span start _) _ = throwError (NonAsciiChar start)

invalidEscapeError :: Action
invalidEscapeError span _ = throwError (InvalidEscape span)
