module Parse (
  Day(..),
  getData
  ) where

import Control.Applicative ((<$>), (<*))
import Data.List (elemIndex)
import Text.Parsec (char, count, digit, many, many1, newline, oneOf, noneOf, string, upper, (<|>), optional, skipMany)
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
  string "----- "
  monthStr <- many upper
  string " -----"
  case elemIndex monthStr monthNames of
    Just idx -> return $ idx + 1
    Nothing  -> unexpected $ "month String: " ++ monthStr
  where monthNames = ["LEDEN", "UNOR", "BREZEN", "DUBEN", "KVETEN", "CERVEN", "CERVENEC", "SRPEN", "ZARI", "RIJEN", "LISTOPAD", "PROSINEC"]

parseDay :: Int -> Parser Day
parseDay month = do
  day <- read <$> count 2 digit
  string ". "
  (words, revCnt) <- languageRecord
  many $ char ' '
  (xCnt, oCnt) <- parseXXX
  optional eolComment
  newline
  return $ Day month day words revCnt xCnt oCnt
--  where eolComment = char '#' >> many $ noneOf '\n'

eolComment :: Parser ()
eolComment = many (char ' ') >> char '#' >> skipMany (noneOf "\n")

languageRecord :: Parser (Int, Int)
languageRecord = noLang <|> didLang
  where 
    noLang = char '-' >> return (0,0)
    didLang = do
      string "D+"
      wordsAdded <- read <$> many1 digit
      char '('
      revCnt <- read <$> many1 digit
      char ')'
      return (wordsAdded, revCnt)

parseXXX :: Parser (Int, Int)
parseXXX = do
  xos <- many $ oneOf "xo"
  let xs = length . filter (== 'x') $ xos
      os = length . filter (== 'o') $ xos
  return (xs, os)

