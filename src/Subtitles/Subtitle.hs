{-# LANGUAGE OverloadedStrings #-}
module Subtitles.Subtitle where

import Control.Monad ((>=>), (<=<))

import Data.Function (on)
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import Subtitles.Test

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

-- Produces True if the first timecode occurs before than
-- the second one
before :: TimeCode -> TimeCode -> Bool
before = (<)

-- Produces True if the first timecode occurs later than
-- the second one
after :: TimeCode -> TimeCode -> Bool
after = (>)

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

-- Produces True if the first dialog starts before the second one
-- otherwise False
startsBefore :: Dialog -> Dialog -> Bool
startsBefore = on before startTime

-- Produces True if the first dialog ends before the second one
-- otherwise False
endsAfter :: Dialog -> Dialog -> Bool
endsAfter = on after endTime

-- Produces the comparisation of start times
startCompare :: Dialog -> Dialog -> Ordering
startCompare = on compare startTime

-- Produces the comparisation of end times
endCompare :: Dialog -> Dialog -> Ordering
endCompare = on compare endTime

-- * Operations

-- Effect is an IO computation form Dialog list to Dialog list
type Effect = [Dialog] -> IO [Dialog]

effectCata :: (([Dialog] -> IO [Dialog]) -> a) -> Effect -> a
effectCata f x = f x

-- The empty transformation of the dialog list
noEffect :: Effect
noEffect = return

-- Sorts the dialog incrementally based on their starting time
incrementalTime :: Effect
incrementalTime = return . sortBy startCompare

infixr 2 <>

-- Effect compostion the right effect is used first, than
-- the left one
(<>) :: Effect -> Effect -> Effect
(<>) = (<=<)

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

convert :: Conversion i o -> Effect -> String -> IO String
convert c e i =
  let dialogs = map (dialogInput c) . (readInput c) $ i
  in fmap ((writeOutput c) . map (dialogOutput c)) (e dialogs)

-- Dialog input that converts a given value into a dialog
class InputPlugin i where
  inputPlugin :: Conversion a o -> Conversion i o

-- Converts a dialog into some other representation
class OutputPlugin o where
  outputPlugin :: Conversion i a -> Conversion i o

-- * Tests

tests = do
  test $ Equals True (before (TimeCode 1 40 0 0) (TimeCode 2 30 0 0)) "Timecode #1"
  test $ Equals False (before (TimeCode 2 10 0 0) (TimeCode 1 50 0 0)) "Timecode #2"
  test $ Equals False (after (TimeCode 1 40 0 0) (TimeCode 2 30 0 0)) "Timecode #3"
  test $ Equals True (after (TimeCode 2 10 0 0) (TimeCode 1 50 0 0)) "Timecode #4"
  test $ Equals True (startsBefore (Dialog (TimeCode 1 40 0 0) (TimeCode 1 45 0 0) [""])
                                   (Dialog (TimeCode 1 50 0 0) (TimeCode 1 55 0 0) [""])) "Timecode #5"
  test $ Equals True (endsAfter (Dialog (TimeCode 1 50 0 0) (TimeCode 1 55 0 0) [""])
                                (Dialog (TimeCode 1 40 0 0) (TimeCode 1 45 0 0) [""])) "Timecode #6"
