module Main where

import Parse
import Text.Printf (printf)
import Control.Applicative ((<$>))
import Control.Monad (forM_)

type DayLine = String

toCsvLine :: Int -> Day -> String
toCsvLine y Day {monthNum = m, dayNum = d, wordsAdded = w, revCnt = r, xCnt = x, oCnt = o} = printf "'%02d-%02d-%d', '%d', '%d', '%d', '%d'" d m y w r x o

main = do
  putStrLn "'date', 'words added', 'review count', 'x count', 'o count'"
  forM_ [2013, 2014, 2015,2016] $ \year -> do
    eitherDayList <- getData year
    case eitherDayList of
        Right dl -> do
            let csvLines = map (toCsvLine year) dl
            mapM_ putStrLn csvLines
        Left err -> print err
