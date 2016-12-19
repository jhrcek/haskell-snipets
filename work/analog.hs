-- Utility script for parsing UI test's framework.log and looking for log lines 
-- that are followed by too large a delay.
-- Usage: runghc analog <delay-in-seconds> </path/to/framework.log>
import System.Locale (defaultTimeLocale)
import System.Environment (getArgs)
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Data.Time (NominalDiffTime, UTCTime)
import Data.Time.Format (readTime)
import Data.Time.Clock (diffUTCTime)
import Data.Function (on)

type Line = String

main :: IO ()
main = do
 args <- getArgs
 case args of
   [delay, logfile] | all isDigit delay -> analyzeLog (read delay) logfile
   _                                    -> putStrLn "Usage: analog <delay-in-seconds> </path/to/framework.log>"

analyzeLog :: Int -> FilePath -> IO ()
analyzeLog delay logfile = do
     logtext <- readFile logfile
     putStrLn $ unlines . linesWithDelayGreaterThan delay . calculateDelays . filter hasTime . lines $ logtext

hasTime :: Line -> Bool
hasTime = isPrefixOf "["

linesWithDelayGreaterThan :: Int -> [(NominalDiffTime, Line)] -> [Line]
linesWithDelayGreaterThan minDelay =
  map (\(d,l)-> "delay " ++ show d ++ ": " ++ l)
  . filter (\(d,_) -> d > fromIntegral minDelay)

-- For each line in the list calculates time it took for the following line to appear
calculateDelays :: [Line] -> [(NominalDiffTime, Line)]
calculateDelays ls = scanr getDiff seed ls
  where getDiff l1 (_, l2) = ((diffUTCTime `on` extractTime) l2 l1, l1)
        seed = (0 :: NominalDiffTime, head ls) --head ls = arbitrary line as a seed to start scanr

-- Line with time has the form of "[14-10-20 08:21:59,812] DEBUG..."
-- Need to parse the whole date for cases that log includes midnight
extractTime :: Line -> UTCTime
extractTime = readTime defaultTimeLocale "%y-%m-%d %H:%M:%S,%q"
     . (++ "000000000") --apend 9 zeroes & parse as nanoseconds
     . take 21 . tail --extract part between '[' ']'
