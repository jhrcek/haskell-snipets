{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad (forM_)
import Parse
import Text.Printf (printf)

type DayLine = String

toCsvLine :: Int -> Day -> String
toCsvLine year Day{..} =
    printf "'%02d-%02d-%d', '%d', '%d', '%d', '%d'" dayNum monthNum year wordsAdded revCnt xCnt oCnt

main :: IO ()
main = do
  putStrLn "'date', 'words added', 'review count', 'x count', 'o count'"
  forM_ [2013 .. 2019] $ \year -> do
    eitherDayList <- getData year
    case eitherDayList of
        Right dl -> do
            let csvLines = map (toCsvLine year) dl
            mapM_ putStrLn csvLines
        Left err -> print err
