module Subtitles.Effect.Overlapping where

import Control.Monad (when)

import Subtitles.Subtitle hiding (tests, min)
import Subtitles.Test

-- Time period from start to end
newtype Period = Period (TimeCode,TimeCode)
  deriving (Show, Eq)

periodCata f (Period (start, end)) = f start end

periodCata2 f (Period (s1,e1)) (Period (s2,e2)) = f s1 e1 s2 e2

periodStart = periodCata $ \s _ -> s
periodEnd   = periodCata $ \_ e -> e

dialogToPeriod = dialogCata $ \start end _ -> Period (start, end)

-- Joins periods into a distinvt period list
periods :: [Period] -> [Period]
periods [] = []
periods [p] = [p]
periods (p:q:ps)
  | overlaps p q = periods ((joinPeriod p q):ps)
  | otherwise = p:periods (q:ps)

overlaps :: Period -> Period -> Bool
overlaps p q = periodEnd p `after` periodStart q

joinPeriod = periodCata2 $ \s1 e1 s2 e2 -> Period (min s1 s2, max e1 e2)

-- Prints a warning message if founds some of the overlapping dialogs
-- assuming thath the given dialog is incrementally ordered by starting
-- time
warnOverlapping :: Effect
warnOverlapping ds = do
  when (length ds /= (length . periods $ map dialogToPeriod ds)) $
    putStrLn "There are overlapping dialogs"
  return ds

-- * Tests

-- Produces a Timecode with a given minute value
mnt m = TimeCode 0 0 m 0

tests = do
  let p0 = Period (mnt 0, mnt 2)
      p1 = Period (mnt 3, mnt 4)
      p2 = Period (mnt 1, mnt 4)
      p4 = Period (mnt 5, mnt 6)
      p5 = Period (mnt 8, mnt 11)
      p6 = Period (mnt 10, mnt 12)

      po1 = Period (mnt 0, mnt 4)
      po2 = Period (mnt 8, mnt 12)

  test $ Equals True (overlaps p0 p2) "Overlaps"
  test $ Equals [] (periods []) "Periods: Empty list"
  test $ Equals [p0] (periods [p0]) "Periods: One period"
  test $ Equals [p0,p1] (periods [p0,p1]) "Periods: Non overlapping periods"
  test $ Equals [po1] (periods [p0,p2]) "Periods: One overlapping period"
  test $ Equals [po1,p4,po2] (periods [p0,p2,p4,p5,p6]) "Periods: Two overlapping and one distinct"
