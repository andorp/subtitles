{-# LANGUAGE OverloadedStrings #-}
module Subtitles.Plugin.Srt where

import Data.Function
import Data.List (sortBy)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)

import Subtitles.Subtitle

data SrtDialog = SrtDialog Dialog

srtDialogCata f (SrtDialog x) = f x

dialog = srtDialogCata id

srtOutputPlugin :: Conversion i o -> Conversion i SrtDialog
srtOutputPlugin c = c { writeOutput = srtOutput , dialogOutput = SrtDialog }

-- Produces an output string that contains the srt formatted subtitles
-- ordering the given dialog list in a time incremental list, and
-- numbers, numbers the dialogs, print the dialog in srt form with
-- a newline character and concatenates to each other
srtOutput :: [SrtDialog] -> String
srtOutput = T.unpack . T.unlines . concatMap printDialog . numbering

numbering :: [SrtDialog] -> [(Int, SrtDialog)]
numbering = zip [1..]

printDialog :: (Int, SrtDialog) -> [Text]
printDialog (n,d) = srtDialogCata (dialogCata srtLines) d where
  srtLines start end lines = [
      T.pack $ show n
    , T.concat [srtTimeCode start, " --> ", srtTimeCode end]
    ] ++ lines ++ [T.empty]

srtTimeCode :: TimeCode -> Text
srtTimeCode = timeCodeCata $ \hour min sec msec ->
  T.concat $ map T.pack [
      printf "%02d" hour, ":"
    , printf "%02d" min , ":"
    , printf "%02d" sec , ","
    , printf "%03d" msec
    ]

instance OutputPlugin SrtDialog where
  outputPlugin = srtOutputPlugin
