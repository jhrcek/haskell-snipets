import Parse
import Text.Printf (printf)
import Control.Applicative ((<$>))
import Control.Monad (forM_)

type DayLine = String

toCsvLine :: Int -> Day -> String
toCsvLine y Day {monthNum = m, dayNum = d, wordsAdded = w, revCnt = r, xCnt = x, oCnt = o} = printf "'%02d-%02d-%d', '%d', '%d', '%d', '%d'" d m y w r x o

fromRight :: Either a b -> b
fromRight (Right r) = r

main = do
  putStrLn "'date', 'words added', 'review count', 'x count', 'o count'"
  forM_ [2013, 2014] $ \year -> do
    dayList <-fromRight <$> getData year
    let csvLines = map (toCsvLine year) dayList
    mapM_ putStrLn csvLines
