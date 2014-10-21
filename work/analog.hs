-- Utility script for parsing UI test's framework.log and looking for log lines 
-- that are followed by too large a delay.
-- Usage: runghc analog < /path/to/framework.log
import System.Locale (defaultTimeLocale)
import System.Environment (getArgs)
import Data.List (isPrefixOf)
import Data.Time (NominalDiffTime, UTCTime)
import Data.Time.Format (readTime)
import Data.Time.Clock (diffUTCTime)

type Line = String

main = do
 args <- getArgs
 if null args
   then putStrLn "Usage: runghc analog <delay> < /path/to/framework.log"
   else do
     let delay = read $ head args 
     interact $ unlines . linesWithDelayGreaterThan delay . filter lineHasTime . lines

lineHasTime :: Line -> Bool
lineHasTime = isPrefixOf "["

linesWithDelayGreaterThan :: Int -> [Line] -> [Line]
linesWithDelayGreaterThan minDelay (l1:l2:rest) 
  | l2 `timeDiff` l1 > fromIntegral minDelay = ("delay " ++ show (l2 `timeDiff` l1) ++ ": " ++ l1) : linesWithDelayGreaterThan minDelay (l2:rest) 
  | otherwise                                = linesWithDelayGreaterThan minDelay (l2:rest)
linesWithDelayGreaterThan _  _               = []

timeDiff :: Line -> Line -> NominalDiffTime
timeDiff line1 line2 = diffUTCTime (extractTime line1) (extractTime line2)

extractTime :: Line -> UTCTime
extractTime = readHMS . take 8 . drop 10

readHMS :: String -> UTCTime
readHMS = readTime defaultTimeLocale "%H:%M:%S"
