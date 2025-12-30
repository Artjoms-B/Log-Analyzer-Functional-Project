{-# LANGUAGE DeriveGeneric #-}

module Main where

import Test.QuickCheck
import LogAnalyzer (LogEntry(..), countStatuses, errorsOnly)
import qualified Data.Map.Strict as Map

instance Arbitrary LogEntry where
  arbitrary = LogEntry
    <$> elements ["1.1.1.1","2.2.2.2","3.3.3.3"]
    <*> elements ["GET","POST"]
    <*> elements ["/","/login","/admin"]
    <*> chooseInt (100, 599)

prop_statusCountMatchesTotal :: [LogEntry] -> Bool
prop_statusCountMatchesTotal es =
  let m = countStatuses es
  in sum (Map.elems m) == length es

prop_errorsOnlyCorrect :: [LogEntry] -> Bool
prop_errorsOnlyCorrect es =
  all (\e -> status e >= 400) (errorsOnly es)

main :: IO ()
main = do
  quickCheck prop_statusCountMatchesTotal
  quickCheck prop_errorsOnlyCorrect
