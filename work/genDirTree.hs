#!/usr/bin/env stack
{- stack --resolver lts-7.14 --install-ghc runghc --package mtl --package containers -}

--import Control.Monad.State
import Data.Char (chr, ord)
import Data.Foldable (traverse_)
import Data.Tree (Forest, Tree (..), drawForest)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

main :: IO ()
main = generate defaultConfig


data Config = Config
    { cRulesPerPackage :: Int
    , cMaxDepth        :: Int
    , cSubdirsPerDir   :: Int
    }


defaultConfig :: Config
defaultConfig = Config
    { cRulesPerPackage = 2
    , cMaxDepth = 2
    , cSubdirsPerDir = 2
    }

generate :: Config -> IO ()
generate Config{cRulesPerPackage = rs, cMaxDepth = d, cSubdirsPerDir = s} = do
    let dirTree = genForest d s
    putStrLn $ drawForest dirTree
    traverse_ (createPkgWithRules rs) $ forestToPaths dirTree


genForest :: Int -> Int -> Forest String
genForest maxDepth subdirsPerDir =
    genForest' 0
  where
    genForest' :: Int -> Forest String
    genForest' curDepth
        | curDepth == maxDepth =  []
        | otherwise = [Node (dirName curDepth idx) (genForest' (curDepth + 1)) | idx <- [1..subdirsPerDir]]


createPkgWithRules :: Int -> FilePath -> IO ()
createPkgWithRules rulesPerPkg pkgPath  = do
    putStrLn $ "Creating directory " ++ pkgPath
    createDirectoryIfMissing True pkgPath
    traverse_
        (createRule pkgPath)
        ['r': show idx ++ ".drl" | idx <- [1..rulesPerPkg]]


createRule :: FilePath -> FilePath -> IO ()
createRule dir fname = do
  let ruleName = dir </> fname
      ruleContents = "package " ++ toPkg dir ++ ";\n"
  putStrLn $ "  Creating rule " ++ ruleName
  writeFile ruleName ruleContents


toPkg :: FilePath -> String
toPkg = map (\c -> if c == '/' then '.' else c)


forestToPaths :: Forest String -> [FilePath]
forestToPaths [] = [""]
forestToPaths xs = concatMap treeToPaths xs


treeToPaths :: Tree String -> [FilePath]
treeToPaths (Node root subforest) = map (root </>) $ forestToPaths subforest


dirName :: Int -> Int -> String
dirName depth index = chr (ord 'a' + depth) : show index
