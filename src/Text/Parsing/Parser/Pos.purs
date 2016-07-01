module Text.Parsing.Parser.Pos where

import Prelude

import Data.String (split)
import Data.Foldable (foldl)

-- | `Position` represents the position of the parser in the input.
-- |
-- | - `line` is the current line in the input
-- | - `column` is the column of the next character in the current line that will be parsed
data Position = Position Int Int

derive instance eqPosition :: Eq Position

instance showPosition :: Show Position where
  show (Position line col) = "Position " <> show line <> " " <> show col

-- | The `Position` before any input has been parsed.
initialPos :: Position
initialPos = Position 1 1

-- | Updates a `Position` by adding the columns and lines in `String`.
updatePosString :: Position -> String -> Position
updatePosString pos str = foldl updatePosChar pos (split "" str)
  where
  updatePosChar (Position line col) c = case c of
    "\n" -> Position (line + 1) 1
    "\r" -> Position (line + 1) 1
    "\t" -> Position line       (col + 8 - ((col - 1) `mod` 8))
    _    -> Position line       (col + 1)
