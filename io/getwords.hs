module Main where
main = getWords

-- example recursive action producing [String]
getWords :: IO [String]
getWords = do
    putStrLn "Please enter a word:"
    word <- getLine
    if null word
        then return []
        else do
            rest <- getWords
            return (word : rest)
