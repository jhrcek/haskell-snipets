import System.Environment   (getArgs)
import System.FilePath      (replaceExtension)
import Data.Char            (isSpace)
main :: IO ()
main = getArgs >>= mapM_ lhsFile2hsFile

lhsFile2hsFile :: FilePath -> IO ()
lhsFile2hsFile f = do 
  contents <- readFile f
  writeFile (replaceExtension f ".hs") $ lhs2hs contents

lhs2hs :: String -> String
lhs2hs = unlines . map (transformLine . trimSpaces) . lines 

trimSpaces :: String -> String
trimSpaces = reverse . dropWhile isSpace . reverse 

transformLine :: String -> String
transformLine ('>' : ' ' : l) = l
transformLine ('>' : l) = l
transformLine l = "--" ++ l
