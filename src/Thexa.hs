module Thexa
( module Thexa.Rule

, Lexer
, Action
, Condition
, makeLexer

, Position(..)
, Span(..)

, LexerState(..)
, initLexerState

, MonadLexer
, runLexer

, emitToken
, setMode
, getMode
, pushMode
, popMode
, getPosition
, getRemainingInput
, getUserState
, setUserState
, modifyUserState

, LexerInput(..)
) where

import PreludePrime

import Control.Monad.State (MonadState)
import Control.Monad.State qualified as State
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Language.Haskell.TH (TExpQ)

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.UTF8 qualified as UTF8

import Thexa.Core (MatchResult(..), EvalCondition)
import Thexa.Core qualified as Core
import Thexa.Position
import Thexa.Rule

type Lexer str mode userState token m = Core.Lexer mode (Condition str mode userState token) (Action str m)

-- | The type of lexer actions in the given monad.
type Action str m = Span -> str -> m ()

-- | The type of lexer conditions with the given user state.
type Condition str mode userState token = Span -> str -> LexerState str mode userState token -> Bool

-- | Construct a lexer at compile-time from the list of rules it should match.
--
-- The order of the rules in the list is important; the resulting lexer will always prefer the
-- longest match, but in the case that multiple rules match the same length of input, the rule that
-- appears earliest in the list will be chosen.
makeLexer :: LexerMode mode
  => [Rule mode (Condition str mode userState token) (Action str m)]
  -> TExpQ (Lexer str mode userState token m)
makeLexer = Core.makeLexer

data LexerState str mode userState token = LexerState
  { inputState :: !(InputState str)
  , activeMode :: mode
  , modeStack  :: [mode]
  , tabSize    :: !Int
  , userState  :: !userState
  , tokens     :: !(Seq token)
  }

initLexerState :: (LexerInput input, LexerMode mode)
  => input     -- ^ The input to match against.
  -> Int       -- ^ The number of columns a tab should count as.
  -> userState -- ^ The initial user state.
  -> LexerState input mode userState token
initLexerState input tabSize userState = LexerState
  { inputState = initInputState input
  , activeMode = minBound
  , modeStack = []
  , tabSize = tabSize
  , userState = userState
  , tokens = Seq.empty
  }

{-# INLINABLE initLexerState #-}

type MonadLexer input mode userState token m =
  ( LexerInput input
  , LexerMode mode
  , MonadState (LexerState input mode userState token) m
  )

runLexer :: forall str mode userState token m
  .  MonadLexer str mode userState token m
  => Lexer str mode userState token m
  -> m () -- ^ Action to run when the lexer fails to match anything.
  -> m ()
runLexer lexer onError = loop
  where
    loop = do
      ls@(LexerState {inputState=input, activeMode=mode, tabSize}) <- State.get
      case Core.nextMatch lexer (nextInputByte @str tabSize) (evalCond ls) mode input of
        MatchAction input' action -> do
          State.put (ls {inputState = input'})
          let (span, str) = inputSpan input input'
          action span str
          loop

        MatchSkip input' -> do
          State.put (ls {inputState = input'})
          loop

        MatchError -> onError
        MatchEOF -> pure ()

    evalCond :: LexerState str mode userState token
      -> EvalCondition (InputState str) (Condition str mode userState token)
    evalCond ls startInput endInput condFn =
      let (span, str) = inputSpan startInput endInput
      in condFn span str (ls {inputState = endInput})

{-# INLINABLE runLexer #-}

emitToken :: MonadLexer input mode userState token m => token -> m ()
emitToken token = State.modify' \ls -> ls {tokens = tokens ls Seq.|> token}

setMode :: MonadLexer input mode userState token m => mode -> m ()
setMode mode = State.modify' \ls -> ls {activeMode = mode}

getMode :: MonadLexer input mode userState token m => m mode
getMode = State.gets activeMode

pushMode :: MonadLexer input mode userState token m => mode -> m ()
pushMode mode = State.modify' \ls -> ls
  { activeMode = mode
  , modeStack = activeMode ls : modeStack ls
  }

popMode :: (Partial, MonadLexer input mode userState token m) => m ()
popMode = State.modify' \ls -> case modeStack ls of
  (m:ms) -> ls {activeMode = m, modeStack = ms}
  [] -> error "no lexer modes on the stack"

getPosition :: forall input mode userState token m. MonadLexer input mode userState token m => m Position
getPosition = State.gets (inputPosition @input . inputState)

getRemainingInput :: MonadLexer input mode userState token m => m input
getRemainingInput = State.gets (remainingInput . inputState)

getUserState :: MonadLexer str mode userState token m => m userState
getUserState = State.gets userState

setUserState :: MonadLexer str mode userState token m => userState -> m ()
setUserState userState = State.modify' \ls -> ls {userState = userState}

modifyUserState :: MonadLexer str mode userState token m => (userState -> userState) -> m ()
modifyUserState f = State.modify' \ls -> ls {userState = f (userState ls)}

--------------------------
-- LexerInput Instances --
--------------------------

-- | Class to abstract over string types which can be used as lexer input.
class LexerInput str where
  -- | State to track the remaining input and current position
  type InputState str
  -- | Initialize the input state with the given input.
  initInputState :: str -> InputState str
  -- | Extract the position from the input state.
  inputPosition :: InputState str -> Position
  -- | Extract the remaining input from the input state.
  remainingInput :: InputState str -> str
  -- | Given start and end input states, extract the input between them.
  inputSpan :: InputState str -> InputState str -> (Span, str)
  -- | Get the next byte of input and update the state, given the tab size.
  nextInputByte :: Int -> GetNextByte (InputState str)

data BSInput = BSInput
  {-# UNPACK #-} !BS.ByteString
  {-# UNPACK #-} !PosState

instance LexerInput BS.ByteString where
  type InputState BS.ByteString = BSInput
  initInputState bs = BSInput bs (initPosState BS.uncons bs)
  inputPosition (BSInput _ pos) = posStatePosition pos
  remainingInput (BSInput bs _) = bs

  inputSpan (BSInput bs (posStatePosition -> startPos)) (BSInput _ (posStatePosition -> endPos))
    = (Span startPos endPos, BS.take (posOffset endPos - posOffset startPos) bs)

  nextInputByte tabSize (BSInput bs pos) = BS.uncons bs <&>
    \(byte, bs') -> (byte, BSInput bs' (updatePosState BS.uncons tabSize byte bs' pos))

data BSLInput = BSLInput
  !BSL.ByteString
  {-# UNPACK #-} !PosState

instance LexerInput BSL.ByteString where
  type InputState BSL.ByteString = BSLInput
  initInputState bs = BSLInput bs (initPosState BSL.uncons bs)
  inputPosition (BSLInput _ pos) = posStatePosition pos
  remainingInput (BSLInput bs _) = bs

  inputSpan (BSLInput bs (posStatePosition -> startPos)) (BSLInput _ (posStatePosition -> endPos))
    = (Span startPos endPos, BSL.take (fromIntegral (posOffset endPos - posOffset startPos)) bs)

  nextInputByte tabSize (BSLInput bs pos) = BSL.uncons bs <&>
    \(byte, bs') -> (byte, BSLInput bs' (updatePosState BSL.uncons tabSize byte bs' pos))

-- | Converts between 'String' and UTF-8-encoded lazy 'BSL.ByteString'.
instance LexerInput String where
  type InputState String = InputState BSL.ByteString
  initInputState = initInputState @BSL.ByteString . UTF8.fromString
  inputPosition = inputPosition @BSL.ByteString
  remainingInput = UTF8.toString . remainingInput @BSL.ByteString
  nextInputByte = nextInputByte @BSL.ByteString

  inputSpan start end = (span, UTF8.toString bs)
    where (span, bs) = inputSpan @BSL.ByteString start end

