-- | This module provides a "batteries-included" interface to the lexer, allowing stateful monadic
-- lexer actions and automatically managing the remaining input, character positions, lexer modes,
-- and a list of tokens produced by your lexer actions. It is built as a layer on top of the more
-- primitive interface provided by "Lexer.Core", so if the interface here doesn't meet your
-- requirements, you can build one specialized to your application by importing that instead.
module Thexa
( module Thexa.Rule
, Lexer
, Action
, Condition
, makeLexer

-- * Positions
, Position(..)
, posLineOffset
, Span(..)

-- * Lexer state
, LexerState(..)
, initLexerState

-- * Running the lexer
, MonadLexer
, runLexer

-- * Lexer state manipulation
, emitToken
, getTokens
, setMode
, getMode
, pushMode
, popMode
, getPosition
, getRemainingInput
, getUserState
, setUserState
, modifyUserState

-- * Lexer input
, LexerInput(..)
) where

import Control.Monad.State (MonadState)
import Control.Monad.State qualified as State
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.UTF8 qualified as UTF8
import Data.String.UTF8 (UTF8)
import Data.String.UTF8 qualified as UTF8 (fromRep, toRep)

import Thexa.Core (MatchResult(..), EvalCondition)
import Thexa.Core qualified as Core
import Thexa.Position
import Thexa.Rule

-- | The type of the precompiled lexer generated with 'makeLexer'.
--
-- The arguments are as follows:
--
-- * @str@: the type of the input string, which must be an instance of 'LexerInput'.
-- * @mode@: the type of the lexer modes, which must be an instance of 'LexerMode'.
-- * @userState@: additional state to be included in the 'LexerState'.
-- * @token@: the type of tokens which are produced by the lexer.
-- * @m@: the monad in which lexer actions are run, which must satisfy the 'MonadLexer' constraint.
type Lexer str mode userState token m = Core.Lexer mode (Condition str mode userState token) (Action str m)

-- | The type of lexer actions which are executed after the lexer matches a rule.
--
-- The action is provided the span of input that was matched.
type Action str m = Span -> str -> m ()

-- | The type of lexer conditions which can be used to restrict when a rule matches.
--
-- The condition is provided the span of input that was matched and the 'LexerState' at the end of
-- the match.
type Condition str mode userState token = Span -> str -> LexerState str mode userState token -> Bool

-- | Construct a lexer at compile-time from the list of rules it should match.
--
-- The order of the rules in the list is important; the resulting lexer will always prefer the
-- longest match, but in the case that multiple rules match the same length of input, the rule that
-- appears earliest in the list will be chosen.
makeLexer :: LexerMode mode
  => [Rule mode (Condition str mode userState token) (Action str m)]
  -> SpliceQ (Lexer str mode userState token m)
makeLexer = Core.makeLexer

-- | The state required to run the lexer.
data LexerState str mode userState token = LexerState
  { inputState :: !(InputState str)
  -- ^ State to track the remaining input string and the current position.
  , activeMode :: !mode
  -- ^ The currently active lexer mode. It is initialized to the default mode (i.e., 'minBound') by
  -- 'initLexerState'.
  , modeStack  :: ![mode]
  -- ^ Frequently, it is useful to enter a new lexer mode and then return to the previous mode once
  -- the current construct has been parsed. To enable this, the lexer state contains a stack of
  -- modes which can be manipulated with 'pushMode' and 'popMode'. When a mode is pushed, it becomes
  -- the currently active mode, and the previously active mode is added to the stack. When a mode is
  -- popped, it is removed from the top of the stack and becomes the new active mode.
  , userState  :: !userState
  -- ^ Additional state for the lexer provided by the user.
  , tokens     :: !(Seq token)
  -- ^ The tokens which have been emitted by the lexer thus far. Uses 'Seq' rather than @[]@ to
  -- provide efficient access to the back of the list.
  }

-- | Initialize the lexer state.
initLexerState :: (LexerInput str, LexerMode mode)
  => str       -- ^ The input to match against.
  -> userState -- ^ The initial user state.
  -> LexerState str mode userState token
initLexerState str userState = LexerState
  { inputState = initInputState str
  , activeMode = minBound
  , modeStack = []
  , userState = userState
  , tokens = Seq.empty
  }

{-# INLINABLE initLexerState #-}

-- | Constraints required for the monad in which lexer actions are run.
type MonadLexer str mode userState token m =
  ( LexerInput str
  , LexerMode mode
  , MonadState (LexerState str mode userState token) m
  )

-- | Repeatedly match the remaining input against the given lexer until all input has been consumed
-- or the lexer fails to match. For each match, execute the associated action.
--
-- Generally, this would be used as part of a function that sets up the initial state and runs the
-- associated monad as well. For example, this function that returns the list of tokens produced or
-- the position at which an error was encountered:
--
-- @
-- runMyLexer :: String -> Either 'Position' [Token]
-- runMyLexer str = 'State.evalState' ('runLexer' myLexer 4 onSuccess onError) initState
--   where
--     initState = 'initLexerState' str myInitState
--     onSuccess = fmap (Right . toList) 'getTokens'
--     onError   = fmap Left 'getPosition'
-- @
runLexer :: forall str mode userState token m a
  .  MonadLexer str mode userState token m
  => Lexer str mode userState token m
  -> Int -- ^ Tab size: the number of columns a tab character should count as.
  -> m a -- ^ Action to run when the lexer has consumed all input.
  -> m a -- ^ Action to run when the lexer fails to match the remaining input.
  -> m a
runLexer lexer tabSize onEOF onError = loop
  where
    loop = do
      ls@(LexerState {inputState=input, activeMode=mode}) <- State.get
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
        MatchEOF -> onEOF

    evalCond :: LexerState str mode userState token
      -> EvalCondition (InputState str) (Condition str mode userState token)
    evalCond ls startInput endInput condFn =
      let (span, str) = inputSpan startInput endInput
      in condFn span str (ls {inputState = endInput})

{-# INLINABLE runLexer #-}

-- | Append a token to the end of 'tokens'. Strict in its argument.
emitToken :: MonadLexer str mode userState token m => token -> m ()
emitToken !token = State.modify' \ls -> ls {tokens = tokens ls Seq.|> token}

-- | Get the tokens that have been emitted thus far.
getTokens :: MonadLexer str mode userState token m => m (Seq token)
getTokens = State.gets tokens

-- | Replace the currently active mode without affecting the mode stack.
setMode :: MonadLexer str mode userState token m => mode -> m ()
setMode mode = State.modify' \ls -> ls {activeMode = mode}

-- | Get the currently active mode.
getMode :: MonadLexer str mode userState token m => m mode
getMode = State.gets activeMode

-- | Set the active mode and push the previous mode onto the mode stack.
pushMode :: MonadLexer str mode userState token m => mode -> m ()
pushMode mode = State.modify' \ls -> ls
  { activeMode = mode
  , modeStack = activeMode ls : modeStack ls
  }

-- | Remove the mode on top of the mode stack and make it the active mode.
--
-- Throws an exception if there are no modes on the mode stack.
popMode :: (Partial, MonadLexer str mode userState token m) => m ()
popMode = State.modify' \ls -> case modeStack ls of
  (m:ms) -> ls {activeMode = m, modeStack = ms}
  [] -> error "no lexer modes on the stack"

-- | Get the current input position.
getPosition :: forall str mode userState token m. MonadLexer str mode userState token m => m Position
getPosition = State.gets (inputPosition @str . inputState)

-- | Get the remaining unconsumed input.
getRemainingInput :: MonadLexer str mode userState token m => m str
getRemainingInput = State.gets (remainingInput . inputState)

-- | Get the current user-provided state.
getUserState :: MonadLexer str mode userState token m => m userState
getUserState = State.gets userState

-- | Set the user-provided state.
setUserState :: MonadLexer str mode userState token m => userState -> m ()
setUserState userState = State.modify' \ls -> ls {userState = userState}

-- | Strictly modify the user-provided state.
modifyUserState :: MonadLexer str mode userState token m => (userState -> userState) -> m ()
modifyUserState f = State.modify' \ls -> ls {userState = f (userState ls)}

--------------------------
-- LexerInput Instances --
--------------------------

-- | Class to abstract over string types which can be used as lexer input.
--
-- Note that these functions assume that any input state occurs at a code point boundary (with the
-- exception of 'nextInputByte'). This is generally safe because the lexer will never match only
-- part of a code point. This assumption allows the 'String' instance to use the 'BSL.ByteString'
-- instance internally and decode back into a 'String' to implement 'remainingInput' or 'inputSpan'.
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
    = (Span startPos endPos, BS.take (posFileOffset endPos - posFileOffset startPos) bs)

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
    = (Span startPos endPos, BSL.take (fromIntegral (posFileOffset endPos - posFileOffset startPos)) bs)

  nextInputByte tabSize (BSLInput bs pos) = BSL.uncons bs <&>
    \(byte, bs') -> (byte, BSLInput bs' (updatePosState BSL.uncons tabSize byte bs' pos))

-- | Converts between 'String' and UTF-8 encoded lazy 'BSL.ByteString'.
instance LexerInput String where
  type InputState String = InputState BSL.ByteString
  initInputState = initInputState @BSL.ByteString . UTF8.fromString
  inputPosition = inputPosition @BSL.ByteString
  remainingInput = UTF8.toString . remainingInput @BSL.ByteString
  nextInputByte = nextInputByte @BSL.ByteString

  inputSpan start end = (span, UTF8.toString bs)
    where (span, bs) = inputSpan @BSL.ByteString start end

instance LexerInput str => LexerInput (UTF8 str) where
  {-# SPECIALIZE instance LexerInput (UTF8 BS.ByteString) #-}
  {-# SPECIALIZE instance LexerInput (UTF8 BSL.ByteString) #-}

  type InputState (UTF8 str) = InputState str
  initInputState = initInputState @str . UTF8.toRep
  inputPosition = inputPosition @str
  remainingInput = UTF8.fromRep . remainingInput @str
  nextInputByte = nextInputByte @str

  inputSpan start end = (span, UTF8.fromRep bs)
    where (span, bs) = inputSpan @str start end
