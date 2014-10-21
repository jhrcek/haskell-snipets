import System.Random
import Control.Monad (unless)

main = do
  gen <- getStdGen
  askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
  let (num, gen') = randomR (1, 10) gen :: (Int, StdGen)
  putStrLn "I'm thinking of a number between 1 and 10. Guess"
  guessStr <- getLine
  unless (null guessStr) $ do
    let guess = read guessStr
    putStrLn $ if guess == num then "Yes, you got it!" else "Nay, it was " ++ show num
    askForNumber gen'
