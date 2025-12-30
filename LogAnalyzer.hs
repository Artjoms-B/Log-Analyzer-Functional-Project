{-# LANGUAGE DeriveGeneric #-}

module LogAnalyzer
  ( LogEntry(..)
  , Report(..)
  , ParseError(..)
  , HttpErrorSample(..)
  , parseLogE
  , filterEntries
  , countStatuses
  , errorsOnly
  , toReport
  ) where


import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Text.Read (readMaybe)
import Data.Aeson (ToJSON)

-- Domain

data LogEntry = LogEntry
  { ip       :: String
  , method   :: String
  , endpoint :: String
  , status   :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON LogEntry

data ParseError
  = BadFormat String
  | BadStatus String
  deriving (Show, Eq, Generic)

instance ToJSON ParseError

data HttpErrorSample = HttpErrorSample
  { errIP     :: String
  , errStatus :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON HttpErrorSample

data Report = Report
  { totalLines       :: Int
  , parsedOk         :: Int
  , skippedLines     :: Int
  , statuses         :: [(Int, Int)]
  , topIPs           :: [(String, Int)]
  , errors           :: [LogEntry]          
  , parseErrors      :: [ParseError]        

  , badFormatTotal   :: Int
  , badStatusTotal   :: Int
  , httpErrorsTotal  :: Int                
  , httpErrorSamples :: [HttpErrorSample]  
  } deriving (Show, Eq, Generic)

instance ToJSON Report

-- Parsing

parseLineE :: String -> Either ParseError LogEntry
parseLineE line =
  case words line of
    [a,b,c,d] ->
      case readMaybe d of
        Just code -> Right (LogEntry a b c code)
        Nothing   -> Left (BadStatus line)
    _ -> Left (BadFormat line)

parseLogE :: String -> ([LogEntry], [ParseError], Int)
parseLogE content =
  let ls = lines content
      step ln (oks, errs) =
        case parseLineE ln of
          Right e -> (e:oks, errs)
          Left er -> (oks, er:errs)
      (oksRev, errsRev) = foldr step ([], []) ls
  in (oksRev, errsRev, length ls)

-- Filtering

filterEntries :: Bool -> Int -> [LogEntry] -> [LogEntry]
filterEntries onlyErrors minStatus =
  filter (\e ->
     status e >= minStatus &&
     (not onlyErrors || status e >= 400)
  )

-- Aggregations

countStatuses :: [LogEntry] -> Map Int Int
countStatuses =
  foldl (\acc e -> Map.insertWith (+) (status e) 1 acc) Map.empty

topIps :: Int -> [LogEntry] -> [(String, Int)]
topIps n =
  take n
  . sortOn (negate . snd)
  . Map.toList
  . foldl (\acc e -> Map.insertWith (+) (ip e) 1 acc) Map.empty

errorsOnly :: [LogEntry] -> [LogEntry]
errorsOnly = filter (\e -> status e >= 400)

countParseErrors :: [ParseError] -> (Int, Int)
countParseErrors =
  foldl step (0,0)
  where
    step (bf, bs) pe = case pe of
      BadFormat _ -> (bf + 1, bs)
      BadStatus _ -> (bf, bs + 1)

mkHttpErrorSamples :: Int -> [LogEntry] -> [HttpErrorSample]
mkHttpErrorSamples n =
  take n . map (\e -> HttpErrorSample (ip e) (status e)) . errorsOnly

toReport :: Int -> Int -> Bool -> Int -> [LogEntry] -> [ParseError] -> Report
toReport topN total onlyErrors minStatus entries errs =
  let filtered      = filterEntries onlyErrors minStatus entries
      (bfTot, bsTot) = countParseErrors errs
      httpTotal     = length (errorsOnly filtered)
  in Report
      { totalLines       = total
      , parsedOk         = length entries
      , skippedLines     = total - length entries
      , statuses         = Map.toList (countStatuses filtered)
      , topIPs           = topIps topN filtered
      , errors           = errorsOnly filtered
      , parseErrors      = take 20 errs

      , badFormatTotal   = bfTot
      , badStatusTotal   = bsTot
      , httpErrorsTotal  = httpTotal
      , httpErrorSamples = mkHttpErrorSamples 20 filtered
      }