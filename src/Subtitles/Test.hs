{-# LANGUAGE ExistentialQuantification #-}
module Subtitles.Test where

data Assertion a
  = Equals a a String

assertionCata f a = case a of
  Equals found expected msg -> f found expected msg

test :: (Eq a, Show a) => Assertion a -> IO ()
test = assertionCata equals
  where
    equals found expected testName
      | found == expected = putStrLn $ "[ OK ] " ++ testName
      | otherwise         = putStrLn $ "[FAIL] " ++ testName

