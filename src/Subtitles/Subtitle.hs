module Subtitles.Subtitle where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

-- * Definitions

-- Timecode represent a time value in the video.
-- The time consits of an hour, a minute, a second,
-- and a millisecond.
data TimeCode = TimeCode {
    hour :: !Int
  , min  :: !Int
  , sec  :: !Int
  , msec :: !Int
  } deriving (Eq, Ord, Show, Read)

timeCodeCata f (TimeCode hour min sec msec) = f hour min sec msec

-- Dialog is a small text shown on the screen in a
-- period of a time, with some properties. The fields
-- are self explanatories. The text is separeted into lines
-- Supposing that the start show earlier
-- time event than the end.
data Dialog = Dialog {
    startTime :: TimeCode
  , endTime   :: TimeCode
  , lines     :: [Text]
  -- Properties are not included in this version
  } deriving (Eq, Ord, Show, Read)

dialogCata f (Dialog start end text) = f start end text

-- A subtitle is a set of dialogs,
-- represents all the dialogs occur during
-- the video
newtype Subtitle = Subtitle [Dialog]
  deriving (Eq, Show)

subtitleCata d f (Subtitle dialogs) = f (map d dialogs)

-- * Operations

-- Conversion represents a conversion from the input to
-- output, represented are conversational functions
data Conversion i o = Conversion {
    readInput :: String -> [i]
  , dialogInput :: i -> Dialog
  , writeOutput :: [o] -> String
  , dialogOutput :: Dialog -> o
  }

identityConv :: Conversion Dialog Dialog
identityConv = Conversion {
    readInput = read
  , dialogInput = id
  , writeOutput = show
  , dialogOutput = id
  }

convert :: Conversion i o -> String -> String
convert c = (writeOutput c) . map (dialogOutput c) . map (dialogInput c) . (readInput c)

-- Dialog input that converts a given value into a dialog
class InputPlugin i where
  inputPlugin :: Conversion a o -> Conversion i o

-- Converts a dialog into some other representation
class OutputPlugin o where
  outputPlugin :: Conversion i a -> Conversion i o

