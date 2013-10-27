module Main where

import Subtitles.Subtitle
import Subtitles.Plugin.Ass
import Subtitles.Plugin.Srt

import System.Environment (getArgs)

main = do
  args <- getArgs
  case args of
    [inputFile, outputFile] -> do
      let c = srtOutputPlugin $ assInputPlugin identityConv
      input <- readFile inputFile
      let output = convert c input
      writeFile outputFile output
      return ()
    _ -> putStrLn "Usage: input.ass output.srt"
