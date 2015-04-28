import Data.Char (isAlpha, isSpace, toLower)
import Data.Foldable (find)
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)
import Data.Tree
import Text.Printf (printf)

main :: IO ()
main = do
    input <- readFile "words.test"
    let charFreqs = countsToFreqs . buildCharCounts2 . map (++"$") . words . normalizeInput $ input
    printForest charFreqs
    loop charFreqs

loop :: CharFreqs -> IO ()
loop charFreqs = do
    putStr "Enter prefix: "
    prefix <- getLine
    let maybeSubforest = selectSubforest prefix charFreqs
    case maybeSubforest of
        Nothing -> print $ prefix ++ " not found"
        Just sf -> mapM_ (printWord prefix) . reverse . sortByFrequency . wordsFromForest $ sf
    loop charFreqs
        where printWord prefix (suffix,freq) = putStrLn $ printf "%s (%.2f%%)" (prefix ++ init suffix) freq

-- | Words represented by characters along paths from roots to leaves. Each char has number of its ocurrences associated
type CharCounts = Forest (Char, Int)
-- | Words represented by characters along paths from roots to leaves. Number associated with char represents the probability (0,1] that it will be the next letter of the word
type CharFreqs = Forest (Char, Double)

-- | Build CharCounts from a list of words
buildCharCounts :: [String] -> CharCounts
buildCharCounts = foldr insertWord []

insertWord :: String -> CharCounts -> CharCounts
insertWord [] f = f
insertWord (x:xs) [] = [Node (x,1) (insertWord xs [])]
insertWord (x:xs)(t@(Node (chr, count) subforest):ts)
    | chr == x     = Node (chr, count + 1) (insertWord xs subforest) : ts
    | otherwise    = t : insertWord (x:xs) ts

countsToFreqs :: CharCounts -> CharFreqs
countsToFreqs forest = map normalizeTree forest
  where
    normalizeTree :: Tree (Char, Int) -> Tree (Char, Double)
    normalizeTree (Node (chr, count) subforest) =
                   Node (chr, fromIntegral count / fromIntegral rootSum) (countsToFreqs subforest)
    rootSum = sumOfRoots forest

    sumOfRoots :: CharCounts -> Int
    sumOfRoots = sum . map (snd . rootLabel)

--2 functions return list of pairs (word, probability)
wordsFromForest :: CharFreqs -> [(String, Double)]
wordsFromForest [] = [("", 1)]
wordsFromForest forest = concatMap wordsFromTree forest

wordsFromTree :: Tree (Char, Double) -> [(String, Double)]
wordsFromTree (Node (chr, chrProb) subforest) = map addChrToSuffix (wordsFromForest subforest)
  where
    addChrToSuffix (suffix, suffixProb) = (chr:suffix, chrProb*suffixProb)

findTreeWithRoot :: Char -> Forest (Char, a) -> Maybe (Tree (Char, a))
findTreeWithRoot c = find (\tree -> fst (rootLabel tree) == c)

selectSubforest :: String -> Forest (Char, a) -> Maybe (Forest (Char, a))
selectSubforest "" f = Just f
selectSubforest (x:xs) f = findTreeWithRoot x f >>= \tree -> selectSubforest xs (subForest tree)

sortByFrequency :: [(String, Double)] -> [(String, Double)]
sortByFrequency = sortBy (comparing snd)

printForest :: Show a => Forest a -> IO ()
printForest = putStrLn . drawForest . map (fmap show)

--leave only letters and spaces and convert all spaces to lowercase
normalizeInput :: String -> String
normalizeInput = map toLower . filter (\c -> isAlpha c || isSpace c)


-- (maybe ? )  better way to build forest using unfoldForest: The idea is to use unfoldForest, with unfolding function taking
-- list of words starting with the same letter like ["cat", "cut", "catalogue"] and returns (('c', 3), ["at", "ut", "atalogue"])
buildCharCounts2 :: [String] -> CharCounts
buildCharCounts2 = unfoldForest myUnfolder . sortAndGroup
  where
    myUnfolder :: [String] -> ((Char, Int), [[String]])
    myUnfolder ws = ((firstLetter ws, length ws), sortAndGroup . map tail $ ws)

    sortAndGroup :: [String] -> [[String]]
    sortAndGroup = groupBy firstCharEqual . sortBy (comparing head) . filter (not . null)

    firstLetter :: [String] -> Char
    firstLetter = head . head

    firstCharEqual :: String -> String -> Bool
    firstCharEqual (c:_) (d:_) = c == d
