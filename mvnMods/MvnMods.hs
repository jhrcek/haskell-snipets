module Main where

import Control.Monad (unless)
import Data.Tree (Tree(Node, rootLabel), unfoldTreeM)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.FilePath ((</>))
import Text.XML.HXT.Core (runX, (/>), (//>), readDocument, hasName, getText)
import Text.Printf (printf)

import Showdot

main :: IO ()
main = do
   args <- getArgs
   case args of
       [] -> putStrLn "usage: mvnProjModules <path to maven project's root dir>"
       (dir:_) -> showModuleStructure dir

type ArtifactId = String
type DotSource = String

-- | Display module structure as a graphviz-generated image
showModuleStructure :: FilePath -> IO ()
showModuleStructure mvnProjRoot =
    parseModuleStructure mvnProjRoot >>= showDot . toDotSource

parseModuleStructure :: FilePath -- ^ Root directory of maven project
                     -> IO (Tree ArtifactId) -- ^ Tree representing module dependencies extracted from pom.xml files
parseModuleStructure = unfoldTreeM getModuleNames

-- | Helper for tree unfolding
getModuleNames :: FilePath -- ^ Root directory of maven project
               -> IO (ArtifactId, [FilePath]) -- ^ (artifactId, [directories containing pom.xml of submodules])
getModuleNames dir = do
    pomExists <- doesFileExist pom
    unless pomExists (error $ pom ++ " does not exist")
    -- parse artifact id and modules from given pom
    let doc = readDocument [] pom
    [artifactId] <- runX $ doc /> artifactIdArr
    modules <- runX $ doc //> modulesArr
    return (artifactId, map (dir </> ) modules)
  where
    pom = dir </> "pom.xml"
    artifactIdArr = hasName "project" /> hasName "artifactId" //> getText
    modulesArr = hasName "modules" /> hasName "module" //> getText

-- | Conversion to dot source
toDotSource :: Tree ArtifactId -> DotSource
toDotSource = ("strict digraph {\nrankdir=LR\n" ++ ) . (++"}") . unlines . map showEdge . treeToEdgeList
    where showEdge (from, to) = printf "\"%s\" -> \"%s\"" from to

treeToEdgeList :: Tree a -> [(a,a)]
treeToEdgeList (Node root subforest) =
    map (\subtree -> (root, rootLabel subtree)) subforest ++
    concatMap treeToEdgeList subforest
