import Control.Monad.Writer.Lazy

countPartitions :: Int -> Int -> Int
countPartitions n 1 = 1
countPartitions n k
    | k == n = 1
    | otherwise = k * countPartitions (n - 1) k + countPartitions (n - 1) (k - 1)

-- Count Partitions Monad - using writer to trace recursive calls
cpm :: Int -> Int -> Writer [String] Int
cpm n 1 = return 1
cpm n k
    | k == n = return 1
    | otherwise = do
        tell
            [ showCall n k (n - 1) k
            , showCall n k (n - 1) (k - 1)
            ]
        a <- cpm (n - 1) k
        b <- cpm (n - 1) (k - 1)
        return $ k * a + b
  where
    showCall n k n' k' = pair n k ++ " -> " ++ pair n' k'
    pair a b = show $ "(" ++ show a ++ " " ++ show b ++ ")"
