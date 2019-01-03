{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
module Parse
    ( Day(..)
    , getData
    ) where

import Control.Applicative ((<$>), (<*))
import Data.List (elemIndex)
import Text.Parsec (char, count, digit, many, many1, newline, noneOf, oneOf,
                    optional, skipMany, string, upper, (<|>))
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (unexpected)
import Text.Parsec.String

getData :: Int -> IO (Either ParseError [Day])
getData year = parseFromFile parseYear $ "DiaryLangs" ++ show year ++".txt"

data Day = Day {monthNum, dayNum, wordsAdded, revCnt, xCnt, oCnt :: Int} deriving Show

parseYear :: Parser [Day]
parseYear = concat <$> count 12 parseMonth

parseMonth :: Parser [Day]
parseMonth = do
    monthIdx <- parseMonthHeader <* newline
    many1 (parseDay monthIdx) <* newline

-- Parse number of month ("----- LEDEN -----" -> 1)
parseMonthHeader :: Parser Int
parseMonthHeader = do
    _ <- string "----- "
    monthStr <- many upper
    _ <- string " -----"
    case elemIndex monthStr monthNames of
      Just idx -> return $ idx + 1
      Nothing  -> unexpected $ "month String: " ++ monthStr
    where monthNames = ["LEDEN", "UNOR", "BREZEN", "DUBEN", "KVETEN", "CERVEN", "CERVENEC", "SRPEN", "ZARI", "RIJEN", "LISTOPAD", "PROSINEC"]

parseDay :: Int -> Parser Day
parseDay monthNum = do
    dayNum <- read @Int <$> count 2 digit
    _ <-  string ". "
    (wordsAdded, revCnt) <- languageRecord
    _ <-  many $ char ' '
    (xCnt, oCnt) <- parseXXX
    optional eolComment
    _ <- newline
    return Day{..}
--  where eolComment = char '#' >> many $ noneOf '\n'

eolComment :: Parser ()
eolComment = many (char ' ') >> char '#' >> skipMany (noneOf "\n")

languageRecord :: Parser (Int, Int)
languageRecord = noLang <|> didLang
  where
    noLang = char '-' >> return (0,0)
    didLang = do
      _ <- string "D+"
      wordsAdded <- read <$> many1 digit
      _ <- char '('
      revCnt <- read <$> many1 digit
      _ <- char ')'
      return (wordsAdded, revCnt)

parseXXX :: Parser (Int, Int)
parseXXX = do
    xos <- many $ oneOf "xo"
    let xs = length . filter (== 'x') $ xos
        os = length . filter (== 'o') $ xos
    return (xs, os)
