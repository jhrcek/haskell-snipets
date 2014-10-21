-- Utility script for parsing UI test's framework.log and looking for log lines 
-- that are followed by too large a delay.
-- Usage: runghc analog < /path/to/framework.log
import System.Locale (defaultTimeLocale)
import System.Environment (getArgs)
import Data.List (isPrefixOf)
import Data.Time (NominalDiffTime, UTCTime)
import Data.Time.Format (readTime)
import Data.Time.Clock (diffUTCTime)
import Data.Function (on)

type Line = String

main = do
 args <- getArgs
 if null args
   then putStrLn "Usage: runghc analog <delay> < /path/to/framework.log"
   else do
     let delay = read $ head args 
     interact $ unlines . linesWithDelayGreaterThan delay . addDelays . filter lineHasTime . lines

lineHasTime :: Line -> Bool
lineHasTime = isPrefixOf "["

linesWithDelayGreaterThan :: Int -> [(NominalDiffTime, Line)] -> [Line]
linesWithDelayGreaterThan minDelay = 
  map (\(d,l)-> "delay " ++ show d ++ ": " ++ l) 
  . filter (\(d,l) -> d > fromIntegral minDelay)

addDelays :: [Line] -> [(NominalDiffTime, Line)]
addDelays = twoMap (\l1 l2 -> ((diffUTCTime `on` extractTime) l2 l1, l1))

-- Walks through the list applying f between consecutive pairs of elements.
-- In our case to compute time difference of concesutive lines.
twoMap :: (a -> a -> b) -> [a] -> [b]
twoMap f xs = zipWith f xs (tail xs)

extractTime :: Line -> UTCTime
extractTime = parseTime . take 8 . drop 10
  where parseTime = readTime defaultTimeLocale "%H:%M:%S"
