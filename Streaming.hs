{-# LANGUAGE DeriveGeneric #-}

module Streaming
  ( Agg(..)
  , BadReason(..)
  , ErrorSample(..)    
  , emptyAgg
  , stepLine
  , mergeAgg
  , runStreamingAgg
  , topIPsFromAgg
  , badLimit
  , sampleLimit
  ) where


import Conduit
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (sortOn)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

badLimit :: Int
badLimit = 20

sampleLimit :: Int
sampleLimit = 20

data BadReason
  = BadFormatR T.Text
  | BadStatusR T.Text
  deriving (Show, Eq, Generic)

instance ToJSON BadReason

data ErrorSample = ErrorSample
  { errIP     :: !T.Text
  , errStatus :: !Int
  } deriving (Show, Eq, Generic)

instance ToJSON ErrorSample

data Agg = Agg
  { totalLines    :: !Int
  , parsedOk      :: !Int
  , skippedLines  :: !Int
  , statusMap     :: !(Map Int Int)
  , ipMap         :: !(Map T.Text Int)

  , badSamples    :: ![BadReason]    
  , badFormatCnt  :: !Int
  , badStatusCnt  :: !Int

  , httpErrCount  :: !Int            
  , httpErrSample :: ![ErrorSample]  
  } deriving (Show, Eq, Generic)

instance ToJSON Agg

emptyAgg :: Agg
emptyAgg = Agg 0 0 0 Map.empty Map.empty [] 0 0 0 []

parseLine :: T.Text -> Either BadReason (T.Text, Int)
parseLine line =
  case T.words line of
    (ip:_:_:code:_) ->
      case TR.decimal code of
        Right (s, _) -> Right (ip, s)
        _            -> Left (BadStatusR line)
    _ -> Left (BadFormatR line)

stepLine :: Agg -> T.Text -> Agg
stepLine agg line =
  let agg1 = agg { totalLines = totalLines agg + 1 }

      addBadSample a reason =
        if length (badSamples a) < badLimit
          then a { badSamples = badSamples a ++ [reason] }
          else a

      incBadCounters a reason =
        case reason of
          BadFormatR _ -> a { badFormatCnt = badFormatCnt a + 1 }
          BadStatusR _ -> a { badStatusCnt = badStatusCnt a + 1 }

      addHttpErr a ip st =
        let a1 = a { httpErrCount = httpErrCount a + 1 }
        in if length (httpErrSample a1) < sampleLimit
             then a1 { httpErrSample = httpErrSample a1 ++ [ErrorSample ip st] }
             else a1

  in case parseLine line of
       Left reason ->
         let a2 = agg1 { skippedLines = skippedLines agg1 + 1 }
             a3 = incBadCounters a2 reason
         in addBadSample a3 reason

       Right (ip, st) ->
         let agg2 =
               agg1
                 { parsedOk  = parsedOk agg1 + 1
                 , statusMap = Map.insertWith (+) st 1 (statusMap agg1)
                 , ipMap     = Map.insertWith (+) ip 1 (ipMap agg1)
                 }
         in if st >= 400 then addHttpErr agg2 ip st else agg2

mergeAgg :: Agg -> Agg -> Agg
mergeAgg a b =
  Agg
    { totalLines    = totalLines a + totalLines b
    , parsedOk      = parsedOk a + parsedOk b
    , skippedLines  = skippedLines a + skippedLines b
    , statusMap     = Map.unionWith (+) (statusMap a) (statusMap b)
    , ipMap         = Map.unionWith (+) (ipMap a) (ipMap b)

    , badSamples    = take badLimit (badSamples a ++ badSamples b)
    , badFormatCnt  = badFormatCnt a + badFormatCnt b
    , badStatusCnt  = badStatusCnt a + badStatusCnt b

    , httpErrCount  = httpErrCount a + httpErrCount b
    , httpErrSample = take sampleLimit (httpErrSample a ++ httpErrSample b)
    }

topIPsFromAgg :: Int -> Agg -> [(T.Text, Int)]
topIPsFromAgg n =
  take n
  . sortOn (negate . snd)
  . Map.toList
  . ipMap

runStreamingAgg :: FilePath -> IO Agg
runStreamingAgg file =
  runConduitRes $
    sourceFile file
      .| decodeUtf8C
      .| linesUnboundedC
      .| foldlC stepLine emptyAgg