module Main where

import System.IO
import System.Random

main = do
    num <- randomRIO (1 :: Int, 100)
    putStrLn "I'm thinking of a number between 1 and 100"
    doGuessing num

doGuessing :: (Ord a, Read a) => a -> IO ()
doGuessing num = do
    putStrLn "Enter your guess:"
    guess <- getLine
    let guessNum = read guess
    if guessNum < num
        then do
            putStrLn "Too low!"
            doGuessing num
        else
            if guessNum > num
                then do
                    putStrLn "Too high!"
                    doGuessing num
                else putStrLn "You win!"
