module Text.Parsing.Parser where

import Prelude

import Data.Either
import Data.Identity
import Data.Tuple

import Control.Alt
import Control.Alternative
import Control.Lazy
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans (MonadTrans)
import Control.MonadPlus
import Control.Plus

import Text.Parsing.Parser.Pos

-- | A parsing error, consisting of a message and position information.
data ParseError = ParseError
  { message  :: String
  , position :: Position
  , fatal    :: Boolean
  }

instance showParseError :: Show ParseError where
  show (ParseError e) = "ParseError { message: "  <> e.message
                                <> ", position: " <> show e.position
                                <> ", fatal: "    <> show e.fatal
                                <> " }"

-- | `PState` contains the remaining input and current position.
data PState s = PState
  { input    :: s
  , position :: Position
  }

type Step s a = { input    :: s
                , result   :: Either ParseError a
                , consumed :: Boolean
                , position :: Position
                }

-- | The Parser monad transformer.
-- |
-- | The first type argument is the stream type. Typically, this is either
-- | `String`, or some sort of token stream.
newtype ParserT s m a = ParserT (PState s -> m (Step s a))

-- | Apply a parser by providing an initial state.
unParserT :: forall m s a. ParserT s m a -> PState s -> m (Step s a)
unParserT (ParserT p) = p

-- | Apply a parser, keeping only the parsed result.
runParserT
  :: forall m s a
   . (Monad m)
  => PState s
  -> ParserT s m a
  -> m (Either ParseError a)
runParserT s p = _.result <$> unParserT p s

-- | The `Parser` monad is a synonym for the parser monad transformer applied to
-- | the `Identity` monad.
type Parser s a = ParserT s Identity a

-- | Apply a parser, keeping only the parsed result.
runParser :: forall s a. s -> Parser s a -> Either ParseError a
runParser s = runIdentity <<< runParserT (PState { input: s
                                                 , position: initialPos
                                                 })

instance functorParserT :: (Functor m) => Functor (ParserT s m) where
  map f p = ParserT $ \s -> f' <$> unParserT p s
    where
    f' o = { input:    o.input
           , result:   f <$> o.result
           , consumed: o.consumed
           , position: o.position
           }

instance applyParserT :: (Monad m) => Apply (ParserT s m) where
  apply = ap

instance applicativeParserT :: (Monad m) => Applicative (ParserT s m) where
  pure a = ParserT $ \(PState { input: s, position: pos }) -> do
    pure { input:    s
         , result:   Right a
         , consumed: false
         , position: pos
         }

instance altParserT :: (Monad m) => Alt (ParserT s m) where
  alt p1 p2 = ParserT $ \s -> unParserT p1 s >>= \o ->
    case o.result of
      Left (ParseError { fatal: true }) -> pure o
      Left _ | not o.consumed           -> unParserT p2 s
      otherwise                         -> pure o

instance plusParserT :: (Monad m) => Plus (ParserT s m) where
  empty = fail "No alternative"

instance alternativeParserT :: (Monad m) => Alternative (ParserT s m)

instance bindParserT :: (Monad m) => Bind (ParserT s m) where
  bind p f = ParserT $ \s -> unParserT p s >>= \o ->
    case o.result of
      Left err -> pure $ o { result = Left err }
      Right a  -> updateConsumedFlag o.consumed <$> do
                    unParserT (f a) (PState { input:    o.input
                                            , position: o.position
                                            })
    where
    updateConsumedFlag c o = o { consumed = o.consumed || c }

instance monadParserT :: (Monad m) => Monad (ParserT s m)

instance monadPlusParserT :: (Monad m) => MonadPlus (ParserT s m)

instance monadTransParserT :: MonadTrans (ParserT s) where
  lift m = ParserT $ \(PState { input: s, position: pos }) ->
            (\a -> { input:    s
                   , consumed: false
                   , result:   Right a
                   , position: pos
                   }) <$> m

instance monadStateParserT :: (Monad m) => MonadState s (ParserT s m) where
  state f = ParserT $ \(PState { input: s, position: pos }) ->
    pure $ case f s of
      Tuple a s' -> { input:    s'
                    , consumed: false
                    , result:   Right a
                    , position: pos
                    }

instance lazyParserT :: Lazy (ParserT s m a) where
  defer f = ParserT $ \s -> unParserT (f unit) s

-- | Set the consumed flag.
consume :: forall s m. (Monad m) => ParserT s m Unit
consume = ParserT $ \(PState { input: s, position: pos }) ->
            pure { consumed: true
                 , input:    s
                 , result:   Right unit
                 , position: pos
                 }

-- | Fail with a message.
fail :: forall m s a. (Monad m) => String -> ParserT s m a
fail message = ParserT $ \(PState { input: s, position: pos }) ->
                  pure $ parseFailed s pos message

-- | Fail fatally with a message.
fatal :: forall m s a. (Monad m) => String -> ParserT s m a
fatal message = ParserT $ \(PState { input: s, position: pos }) ->
                  pure $ parseFailedFatal s pos message

-- | Creates a failed parser state for the remaining input `s` and current position
-- | with an error message.
-- |
-- | Most of the time, `fail` should be used instead.
parseFailed :: forall s a. s -> Position -> String -> Step s a
parseFailed = parseFailed' false

parseFailedFatal :: forall s a. s -> Position -> String -> Step s a
parseFailedFatal = parseFailed' true

parseFailed' :: forall s a. Boolean -> s -> Position -> String -> Step s a
parseFailed' isFatal s pos message
  = { input: s
    , consumed: false
    , result: Left (ParseError {
        message:  message
      , position: pos
      , fatal:    isFatal
      })
    , position: pos
    }
