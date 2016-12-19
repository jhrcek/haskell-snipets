module Main where

import Control.Monad
import Data.Maybe (mapMaybe)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.FilePath (addExtension, dropExtension, takeExtension)

main :: IO ()
main = do
    infile <- parseArgs
    indata <- readFile infile
    let outfile = addExtension (dropExtension infile) "hs"
    writeFile outfile (convert indata)

parseArgs :: IO FilePath
parseArgs = do
    args <- getArgs
    when (length args /= 1) die
    let infile =  head args
    exists <- doesFileExist infile
    when (takeExtension infile /= ".lhs" || not exists) die
    return infile
  where
    die = error "Usage: lhs2hs <path-to-lhs-file>"

convert :: String -> String
convert = unlines . mapMaybe f . lines
  where
    f line = case line of
        ('>' : ' ' : rest) -> Just rest
        ('>' : rest)       -> Just rest
        _ -> Nothing
