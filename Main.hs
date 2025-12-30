module Main where

import Options.Applicative
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import System.Exit (die)
import Text.Printf (printf)

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import LogAnalyzer
  ( parseLogE
  , toReport
  , Report(..)
  , ParseError(..)
  , HttpErrorSample(..)
  )
import qualified Streaming as S

data Mode = TextMode | JsonMode deriving (Eq, Show)

data Args = Args
  { filePath   :: FilePath
  , topN       :: Int
  , mode       :: Mode
  , outFile    :: Maybe FilePath
  , onlyErrors :: Bool
  , minStatus  :: Int
  , stream     :: Bool
  } deriving (Show)

main :: IO ()
main = do
  args <- execParser opts

  case (mode args, outFile args) of
    (TextMode, Just _) -> die "Error: --out can be used only with --json"
    _                  -> pure ()

  if stream args
    then runStreamMode args
    else runNormalMode args

-- Normal mode
runNormalMode :: Args -> IO ()
runNormalMode args = do
  content <- readFile (filePath args)
  let (entries, errs, total) = parseLogE content
  let report = toReport (topN args) total (onlyErrors args) (minStatus args) entries errs

  case mode args of
    JsonMode -> outputJson (outFile args) report
    TextMode -> outputText report

-- Streaming mode
runStreamMode :: Args -> IO ()
runStreamMode args = do
  agg <- S.runStreamingAgg (filePath args)

  let stsFiltered =
        filter (\(s,_) -> s >= minStatus args && (not (onlyErrors args) || s >= 400))
        (Map.toList (S.statusMap agg))

  let topIps =
        take (topN args)
        . map (\(ip,c) -> (T.unpack ip, c))
        $ S.topIPsFromAgg (topN args) agg

  let convertReason br =
        case br of
          S.BadFormatR t -> BadFormat (T.unpack t)
          S.BadStatusR t -> BadStatus (T.unpack t)

  let httpSamples =
        take 20
        $ map (\(S.ErrorSample ip st) -> (ip, st)) (S.httpErrSample agg)

  let report =
        Report
          { totalLines       = S.totalLines agg
          , parsedOk         = S.parsedOk agg
          , skippedLines     = S.skippedLines agg
          , statuses         = stsFiltered
          , topIPs           = topIps
          , errors           = [] 
          , parseErrors      = map convertReason (S.badSamples agg)

          , badFormatTotal   = S.badFormatCnt agg
          , badStatusTotal   = S.badStatusCnt agg
          , httpErrorsTotal  = S.httpErrCount agg
          , httpErrorSamples = map (\(ip,st) -> HttpErrorSample (T.unpack ip) st) httpSamples
          }

  case mode args of
    JsonMode -> outputJson (outFile args) report
    TextMode -> outputText report

-- Output

outputJson :: (ToJSON a) => Maybe FilePath -> a -> IO ()
outputJson mPath x =
  case mPath of
    Nothing   -> BL.putStrLn (encodePretty x)
    Just path -> BL.writeFile path (encodePretty x)

pct :: Int -> Int -> Double
pct part whole =
  if whole <= 0 then 0 else (fromIntegral part * 100.0) / fromIntegral whole

outputText :: Report -> IO ()
outputText r = do
  let t  = totalLines r
      ok = parsedOk r
      sk = skippedLines r

  putStrLn $ "Total lines:     " ++ show t
  putStrLn $ "Parsed OK:       " ++ show ok ++ printf " (%.2f%%)" (pct ok t)
  putStrLn $ "Skipped/bad:     " ++ show sk ++ printf " (%.2f%%)" (pct sk t)

  putStrLn "\nStatuses:"
  mapM_ (\(s,c) -> putStrLn $ "  " ++ show s ++ " -> " ++ show c) (statuses r)

  putStrLn "\nTop IPs:"
  mapM_ (\(ip,c) -> putStrLn $ "  " ++ ip ++ " -> " ++ show c) (topIPs r)

  putStrLn $ "\nHTTP errors (>=400) total: " ++ show (httpErrorsTotal r)
           ++ printf " (%.2f%% of parsed)" (pct (httpErrorsTotal r) ok)

  putStrLn $ "BadFormat total: " ++ show (badFormatTotal r)
           ++ printf " (%.2f%% of total)" (pct (badFormatTotal r) t)

  putStrLn $ "BadStatus total: " ++ show (badStatusTotal r)
           ++ printf " (%.2f%% of total)" (pct (badStatusTotal r) t)

  putStrLn $ "\nParse errors kept (first N): " ++ show (length (parseErrors r))
  putStrLn $ "HTTP error samples kept:     " ++ show (length (httpErrorSamples r))

-- CLI Parser

opts :: ParserInfo Args
opts = info (argsParser <**> helper)
  ( fullDesc
 <> progDesc "Functional Log Analyzer (FP Project)"
 <> header "fp-log-analyzer" )

argsParser :: Parser Args
argsParser =
  Args
    <$> strOption
          ( long "file" <> short 'f'
         <> metavar "PATH"
         <> value "log.txt"
         <> showDefault
         <> help "Path to log file" )
    <*> option auto
          ( long "top" <> short 't'
         <> metavar "N"
         <> value 5
         <> showDefault
         <> help "Top N IPs" )
    <*> modeOption
    <*> optional (strOption
          ( long "out" <> short 'o'
         <> metavar "PATH"
         <> help "Write JSON output to file (JSON mode only)" ))
    <*> switch
          ( long "only-errors"
         <> help "Keep only HTTP errors (status >= 400)" )
    <*> option auto
          ( long "min-status"
         <> metavar "CODE"
         <> value 100
         <> showDefault
         <> help "Filter: keep entries with status >= CODE" )
    <*> switch
          ( long "stream"
         <> help "Streaming mode (for very large files)" )

modeOption :: Parser Mode
modeOption =
  flag TextMode JsonMode
    ( long "json"
   <> help "Output JSON (pretty)" )
