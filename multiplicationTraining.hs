import Control.Monad (replicateM, replicateM_)
import System.IO (BufferMode (NoBuffering), hFlush, hSetBuffering, stdin,
                  stdout)
import System.Random (randomRIO)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  putStrLn "How many exercises?"
  numAttempts <- getLine
  replicateM_ (read numAttempts) exercise

exercise :: IO ()
exercise = do
  [a,b] <- genAssignment
  putStr $ show a ++ " * " ++ show b ++ " = "
  hFlush stdout --is this necessary?
  let resStr = show (a * b)
  answer <- replicateM (length resStr) getChar
  putStrLn $ if answer == resStr
    then "\nRight"
    else "\nWrong, it's " ++ resStr

genAssignment :: IO [Int]
genAssignment = replicateM 2 $ randomRIO (1,10)
