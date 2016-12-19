{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.List (sortBy)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Text (Text ())
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Prelude hiding (lookup)
import System.Directory
import System.Environment (getArgs)
import Text.HTML.TagSoup
import Text.Parsec
import Text.Parsec.Text

main :: IO ()
main = getReports >>= either showErr showDiff >> putStrLn "DONE"
  where showErr = putStrLn

showDiff :: (FilePath, FilePath) -> IO ()
showDiff (f1, f2) = do
   html1 <- TIO.readFile f1
   html2 <- TIO.readFile f2
   diffHtmls html1 html2

diffHtmls :: Text -> Text -> IO ()
diffHtmls html1 html2 = do
  let durMap1 = parseData html1
      durMap2 = parseData html2
      differenceMap = M.unionWith diff durMap1 durMap2
      testIds = M.keys differenceMap
      durationDifferencesData = map (\tid -> (tid, getDuration tid durMap1, getDuration tid durMap2, getDuration tid differenceMap)) testIds
  mapM_ print $ sortBy (comparing (\(_,_,_,difference) -> abs difference)) durationDifferencesData
  return ()
  where
    getDuration testId = fromMaybe 0 . M.lookup testId
--    diff = subtract
    diff t1 t2 = if t1 == 0 || t2 == 0 then (-1) else ceiling (100 * fromIntegral t2 / fromIntegral t1)

type DurationMap = M.Map Text Int --map testId durationSeconds
type Duration = (Text, Int) -- (testId, durationSeconds)

parseData :: Text -> DurationMap
parseData = foldl insertDur M.empty . map parseLine . filter (T.isInfixOf "tr class") . T.lines
  where
    parseLine :: Text -> Duration
    parseLine = toDuration . map fromTagText . filter isTagText . parseTags

    toDuration :: [Text] -> Duration
    toDuration [_,className, methodName, durStr] = (T.concat [className,"#",methodName], parseDuration durStr)
    toDuration something                         = error $ "Unexpected table row format: " ++ show something

    parseDuration = either (const 0) id . parse timeParser ""
    timeParser = (\x y -> 60 *x + y) <$> (intParser <* char ':') <*> intParser
    intParser = read <$> many1 digit :: Parser Int

    insertDur m (tid,d) = M.insert tid d m

getReports :: IO (Either String (FilePath, FilePath))
getReports = do
    args <- getArgs
    case args of
        (f1:f2:_) -> do
            bothExist <- (&&) <$> doesFileExist f1 <*> doesFileExist f2
            return $
                if bothExist
                    then Right (f1, f2)
                    else Left $ "at least one of the files does not exist: " <> f1 <> ", " <> f2
        _         -> return $ Left "usage: reportDiff <new-report1.html> <new-report2.html>"
