-- Script to answer "Which parts of Errai are most commonly used in droolsjbpm?"
-- Run the following in droolsjbpm:
-- grep --recursive 'import org.jboss.errai' --no-filename | sort | uniq -c | sort -rn > /tmp/erraiClassUsage

import Data.Bifunctor (first)
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)

main = do
  clsCounts <- parseClassCounts <$> readFile "/tmp/erraiClassUsage"
  mapM_ print $ classCountsToErraiModuleCounts clsCounts

parseClassCounts :: String -> [(String, Int)]
parseClassCounts = map (parseLine . words) . lines
  where
    parseLine [count, _import, pkg] = (pkg, read count)

classCountsToErraiModuleCounts :: [(String, Int)] -> [(String, Int)]
classCountsToErraiModuleCounts =
    sortBy (flip (comparing snd)) .
    map (\xs -> (fst $ head xs, sum $ map snd xs)) .
    groupBy ((==) `on` fst) .
    sortBy (comparing fst) .
    map (first getErraiModule)
  where
    getErraiModule pkg = splitOn "." pkg !! 3
