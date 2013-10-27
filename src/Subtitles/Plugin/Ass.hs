{-# LANGUAGE OverloadedStrings #-}
module Subtitles.Plugin.Ass where

import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T

import Subtitles.Subtitle
import Subtitles.Test

newtype AssDialog = AssDialog Dialog
  deriving (Show)

assDialogCata f (AssDialog x) = f x

-- Reads the content and throws away the unnecessary paragraphs
-- and reads out only the dialog lines. Filters out the only
-- the dialogs that contains visible texts, and converts them
-- into a multiline format
readAssFile :: String -> [AssDialog]
readAssFile = map readDialog . visibleDialogs . keepDialogLines . keepEventsSection . T.lines . T.pack

keepEventsSection :: [Text] -> [Text]
keepEventsSection = emptyOrNonEmpty [] tail . dropWhile (not . startsWith "[Events]")

-- If a given list is empty produces the given
-- value corresponds to the empty list, or
-- produces a value applying the given function
emptyOrNonEmpty :: b -> ([a] -> b) -> [a] -> b
emptyOrNonEmpty e f xs = case xs of
  [] -> e
  ys -> f ys

-- Keeps only the lines that starts with the "Dialogue: " string
keepDialogLines :: [Text] -> [Text]
keepDialogLines = filter (startsWith "Dialogue:")

visibleDialogs :: [Text] -> [Text]
visibleDialogs = filter (not . startsWith "Dialogue: 0,")

readDialog :: Text -> AssDialog
readDialog t =
  case T.split (==',') t of
    (layer:start:end:style:name:marginl:marginr:marginv:effect:texts) ->
      AssDialog (Dialog
        (timecode start)
        (timecode end)
        (multiline . removeCommands . join $ texts))
    _ -> error $ "invalid dialog line: " ++ T.unpack t
  where
    join = T.concat . intersperse ","

timecode :: Text -> TimeCode
timecode t = case (map T.unpack $ T.split (==':') t) of
  [hour,min,secmsec] ->
    let (sec,msec) = splitAt 2 secmsec
    in TimeCode (read hour) (read min) (read sec) (10 * (read $ tail msec))
  _ -> error $ "invalid timecode: " ++ T.unpack t

-- Joins the separated values
multiline :: Text -> [Text]
multiline = filter (not . T.null) . T.splitOn "\\N"

-- Removes the command sequences from a dialog e.g. "{...}"
removeCommands :: Text -> Text
removeCommands = remove False
  where
    remove r t = case (r,T.uncons t) of
      (_, Nothing) -> T.empty
      (True,  Just ('}',t')) -> remove False t'
      (True,  Just ( c ,t')) -> remove True  t'
      (False, Just ('{',t')) -> remove True  t'
      (False, Just ( c ,t')) -> T.cons c (remove False t')

instance InputPlugin AssDialog where
  inputPlugin c = c {
      readInput = readAssFile
    , dialogInput = assDialogCata id
    }

assInputPlugin :: Conversion i o -> Conversion AssDialog o
assInputPlugin = inputPlugin

startsWith :: Text -> Text -> Bool
startsWith t = (t==) . T.take (T.length t)

-- * Tests

tests = do
  test $ Equals (TimeCode 0 0 0 500) (timecode "00:00:00.50") "Timecode #1"
  test $ Equals "axeh" (removeCommands "ax{\\valami}e{gtz}h") "RemoveCommands #1"
