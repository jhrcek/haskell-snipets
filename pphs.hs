{- Pretty print haskell source code -}
module Main (main) where

import Language.Haskell.Exts
import System.Environment (getArgs)

main :: IO ()
main = do
    [infile] <- getArgs
    putStrLn . formatCode =<< readFile infile

formatCode :: String -> String
formatCode = check . fmap prettyPrint . parseModule
  where
    check r = case r of
        ParseOk a -> a
        ParseFailed location err -> error $ show (location, err)
