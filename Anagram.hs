import Control.Monad (forever)
import Data.List (sort)
import qualified Data.Map as M

{-  Based on https://www.youtube.com/watch?v=5crTdwaje4c

ideas: inserting items from a list into Map using foldr

-}

-- Mapping sorted list of letters (key) to list of words whose letters sort to that key, e.g. "loop" -> ["loop", "pool", "polo"]
type Anagrams = M.Map String [String]

main :: IO ()
main = buildAnagrams "/home/jhrcek/Temp/hs/10000en.txt" >>= loop
  where
    loop :: Anagrams -> IO ()
    loop anagrams = forever $ do
      putStrLn "Anagrams of ..."
      getLine >>= print . lookupAnagram anagrams

lookupAnagram :: Anagrams -> String -> [String]
lookupAnagram anagrams w = M.findWithDefault [] (sort w) anagrams

buildAnagrams :: FilePath -> IO Anagrams
buildAnagrams = fmap (foldr addWord M.empty . words) . readFile 
  where
    addWord :: String -> Anagrams -> Anagrams
    addWord w = M.insertWith (++) (sort w) [w]
