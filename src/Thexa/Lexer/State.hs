module Thexa.Lexer.State where

import PreludePrime

import Control.Monad.State (MonadState, get, modify)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

import Thexa.Lexer.Core (LexerMode, MatchResult(..))
import Thexa.Lexer.Core qualified as Core
import Thexa.Lexer.Position

data LexerState mode userState token = LexerState
  { lsInput :: !Input
  , lsMode :: mode
  , lsTabSize :: !Int
  , lsUserState :: userState
  , lsTokens :: !(Seq token)
  }

data Input = Input
  {-# UNPACK #-} !ByteString
  {-# UNPACK #-} !PosState

type Lexer mode userState m = Core.Lexer mode
  (Span -> ByteString -> userState -> Bool)
  (Span -> ByteString -> m ())

initState :: LexerMode mode => ByteString -> Int -> userState -> LexerState mode userState token
initState bs tabSize userState = LexerState
  { lsInput = Input bs (initPosState BS.uncons bs)
  , lsMode = toEnum 0
  , lsTabSize = tabSize
  , lsUserState = userState
  , lsTokens = Seq.empty
  }

emitToken :: MonadState (LexerState mode userState token) m => token -> m ()
emitToken token = modify \ls -> ls {lsTokens = lsTokens ls Seq.|> token}

setMode :: MonadState (LexerState mode userState token) m => mode -> m ()
setMode mode = modify \ls -> ls {lsMode = mode}

runLexer :: (LexerMode mode, MonadState (LexerState mode userState token) m)
  => Lexer mode userState m
  -> (forall e. m e)
  -> m ()
runLexer lexer onError = loop
  where
    loop = do
      LexerState input mode tabSize userState _ <- get
      case Core.nextMatch lexer (getNextByte tabSize) (evalCond userState) mode input of
        MatchAction input' action -> do
          modify \ls -> ls {lsInput = input'}
          let (span, bs) = inputSpan input input'
          action span bs
          loop

        MatchSkip input' -> do
          modify \ls -> ls {lsInput = input'}
          loop

        MatchError -> onError
        MatchEOF -> pure ()

    getNextByte :: Int -> Input -> Maybe (Word8, Input)
    getNextByte tabSize (Input bs pos) = BS.uncons bs <&>
      \(byte, bs') -> (byte, Input bs' (updatePosState BS.uncons tabSize byte bs' pos))

    evalCond :: userState -> Input -> Input -> (Span -> ByteString -> userState -> Bool) -> Bool
    evalCond userState startInput endInput condFn =
      let (span, bs) = inputSpan startInput endInput
      in condFn span bs userState

    inputSpan :: Input -> Input -> (Span, ByteString)
    inputSpan (Input bs (getPosition -> startPos)) (Input _ (getPosition -> endPos)) =
      (Span startPos endPos, BS.take (posByteOffset endPos - posByteOffset startPos) bs)
