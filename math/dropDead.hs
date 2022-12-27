import Test.QuickCheck

{-Based on "Henk Tijms: Understanding Probability", Exercise 2.18 -}

main = do
    simulatedGames <- generate . vectorOf 100000 $ gameGen 6 []
    return $
        let gameLenghts = map length simulatedGames
         in fromIntegral (sum gameLenghts) / fromIntegral (length gameLenghts)

die :: Gen Int
die = choose (1, 6)

nextRoll :: [Int] -> [Int]
nextRoll = filter (`notElem` [2, 5])

score :: [Int] -> Int
score xs
    | any (`elem` xs) [2, 5] = 0
    | otherwise = sum xs

gameGen :: Int -> [[Int]] -> Gen [[Int]]
gameGen diesCount previousRolls = do
    roll <- vectorOf diesCount die
    let nextDiesCount = length $ nextRoll roll
        nextRolls = roll : previousRolls
    if nextDiesCount == 0
        then return $ reverse nextRolls
        else gameGen nextDiesCount nextRolls
