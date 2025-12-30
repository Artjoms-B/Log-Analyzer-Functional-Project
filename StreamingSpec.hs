module Main where

import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import Streaming (Agg(..), emptyAgg, stepLine, mergeAgg)

genValidLine :: Gen T.Text
genValidLine = do
  ip <- elements ["1.1.1.1","2.2.2.2","10.0.0.1"]
  m  <- elements ["GET","POST"]
  ep <- elements ["/","/login","/admin"]
  st <- chooseInt (100,599)
  pure $ T.pack (unwords [ip,m,ep,show st])

genAnyLine :: Gen T.Text
genAnyLine =
  frequency
    [ (8, genValidLine)
    , (2, elements (T.pack <$> ["broken line","1.1.1.1 GET /x abc","","too few"]))
    ]


prop_mergeEquivalent :: Property
prop_mergeEquivalent =
  forAll (listOf genAnyLine) $ \ls ->
    let a1 = foldl stepLine emptyAgg ls
        (l1,l2) = splitAt (length ls `div` 2) ls
        a2 = mergeAgg (foldl stepLine emptyAgg l1) (foldl stepLine emptyAgg l2)
    in a1 == a2

prop_countsConsistent :: Property
prop_countsConsistent =
  forAll (listOf genAnyLine) $ \ls ->
    let a = foldl stepLine emptyAgg ls
    in parsedOk a + skippedLines a == totalLines a

main :: IO ()
main = do
  quickCheck prop_mergeEquivalent
  quickCheck prop_countsConsistent
