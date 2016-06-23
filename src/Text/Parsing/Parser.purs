module Text.Parsing.Parser where

import Prelude

import Control.Lazy (class Lazy)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans (class MonadTrans)
import Control.MonadPlus (class MonadPlus, class MonadZero, class Alternative)
import Control.Plus (class Plus, class Alt)
import Data.Either (Either(..))
import Data.Identity (Identity, runIdentity)
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser.Pos (Position, initialPos)

-- | A parsing error, consisting of a message and position information.
data ParseError = ParseError String Position Boolean
  -- { message :: String
  -- , position :: Position
  -- , fatal :: Boolean
  -- }

instance showParseError :: Show ParseError where
  show (ParseError msg pos fatal) = "ParseError { message: " <> msg <> ", position: " <> show pos <> ", fatal: " <> show fatal <> " }"

instance eqParseError :: Eq ParseError where
  eq (ParseError m1  p1 f1) (ParseError m2 p2 f2) = m1 == m2 && p1 == p2 && f1 == f2

-- | `PState` contains the remaining input and current position.
data PState s = PState s Position

-- | The Parser monad transformer.
-- |
-- | The first type argument is the stream type. Typically, this is either `String`, or some sort of token stream.
newtype ParserT s m a = ParserT (PState s -> m { input :: s, result :: Either ParseError a, consumed :: Boolean, position :: Position })

-- | Apply a parser by providing an initial state.
unParserT :: forall m s a. ParserT s m a -> PState s -> m { input :: s, result :: Either ParseError a, consumed :: Boolean, position :: Position }
unParserT (ParserT p) = p

-- | Apply a parser, keeping only the parsed result.
runParserT :: forall m s a. Monad m => PState s -> ParserT s m a -> m (Either ParseError a)
runParserT s p = do
  o <- unParserT p s
  pure o.result

-- | The `Parser` monad is a synonym for the parser monad transformer applied to the `Identity` monad.
type Parser s a = ParserT s Identity a

-- | Apply a parser, keeping only the parsed result.
runParser :: forall s a. s -> Parser s a -> Either ParseError a
runParser s = runIdentity <<< runParserT (PState s initialPos)

instance functorParserT :: (Functor m) => Functor (ParserT s m) where
  map f p = ParserT $ \s -> f' <$> unParserT p s
    where
    f' o = { input: o.input, result: f <$> o.result, consumed: o.consumed, position: o.position }

instance applyParserT :: Monad m => Apply (ParserT s m) where
  apply = ap

instance applicativeParserT :: Monad m => Applicative (ParserT s m) where
  pure a = ParserT $ \(PState s pos) -> pure { input: s, result: Right a, consumed: false, position: pos }

instance altParserT :: Monad m => Alt (ParserT s m) where
  alt p1 p2 = ParserT $ \s -> unParserT p1 s >>= \o ->
    case o.result of
      Left (ParseError _ _ true) -> pure o
      Left _ | not o.consumed -> unParserT p2 s
      _ -> pure o

instance plusParserT :: Monad m => Plus (ParserT s m) where
  empty = fail "No alternative"

instance alternativeParserT :: Monad m => Alternative (ParserT s m)

instance bindParserT :: Monad m => Bind (ParserT s m) where
  bind p f = ParserT $ \s -> unParserT p s >>= \o ->
    case o.result of
      Left err -> pure { input: o.input, result: Left err, consumed: o.consumed, position: o.position }
      Right a -> updateConsumedFlag o.consumed <$> unParserT (f a) (PState o.input o.position)
    where
    updateConsumedFlag c o = { input: o.input, consumed: c || o.consumed, result: o.result, position: o.position }

instance monadParserT :: Monad m => Monad (ParserT s m)

instance monadZeroParserT :: Monad m => MonadZero (ParserT s m)

instance monadPlusParserT :: Monad m => MonadPlus (ParserT s m)

instance monadTransParserT :: MonadTrans (ParserT s) where
  lift m = ParserT $ \(PState s pos) -> (\a -> { input: s, consumed: false, result: Right a, position: pos }) <$> m

instance monadStateParserT :: Monad m => MonadState s (ParserT s m) where
  state f = ParserT $ \(PState s pos) ->
    pure $ case f s of
      Tuple a s' -> { input: s', consumed: false, result: Right a, position: pos }

instance lazyParserT :: Lazy (ParserT s m a) where
  defer f = ParserT $ \s -> unParserT (f unit) s

-- | Set the consumed flag.
consume :: forall s m. Monad m => ParserT s m Unit
consume = ParserT $ \(PState s pos) -> pure { consumed: true, input: s, result: Right unit, position: pos }

-- | Fail with a message.
fail :: forall m s a. (Monad m) => String -> ParserT s m a
fail message = ParserT $ \(PState s pos) ->
                  pure $ parseFailed s pos message

-- | Fail fatally with a message.
fatal :: forall m s a. (Monad m) => String -> ParserT s m a
fatal message = ParserT $ \(PState s pos) ->
                  pure $ parseFailedFatal s pos message

-- | Creates a failed parser state for the remaining input `s` and current position
-- | with an error message.
-- |
-- | Most of the time, `fail` should be used instead.
parseFailed :: forall s a. s -> Position -> String -> { input :: s, result :: Either ParseError a, consumed :: Boolean, position :: Position }
parseFailed = parseFailed' false

parseFailedFatal :: forall s a. s -> Position -> String -> { input :: s, result :: Either ParseError a, consumed :: Boolean, position :: Position }
parseFailedFatal = parseFailed' true

parseFailed' :: forall s a. Boolean -> s -> Position -> String -> { input :: s, result :: Either ParseError a, consumed :: Boolean, position :: Position }
parseFailed' isFatal s pos message
  = { input: s
    , consumed: false
    , result: Left (ParseError message pos isFatal)
    , position: pos
    }
