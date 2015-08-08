module Main where

import Control.Monad (unless)
import Data.GraphViz (runGraphvizCanvas', setStrictness)
import Data.GraphViz.Attributes.Complete (RankDir(FromLeft), Attribute(RankDir))
import Data.GraphViz.Commands (GraphvizCanvas(Xlib), isGraphvizInstalled)
import Data.GraphViz.Types.Generalised (DotGraph)
import Data.GraphViz.Types.Monadic ((-->), digraph', graphAttrs)
import Data.Tree (Tree(Node, rootLabel), unfoldTreeM)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.FilePath ((</>))
import Text.XML.HXT.Core (runX, (/>), (//>), readDocument, hasName, getText)
import Text.Printf (printf)

import Showdot (showDot)

main :: IO ()
main = do
    gvInstalled <- isGraphvizInstalled
    if gvInstalled
      then putStrLn "Graphviz not installed. Please install it to proceed (http://www.graphviz.org/)"
      else getArgs >>= main'

main' :: [String] -> IO ()
main' args = case args of
    [] -> putStrLn "usage: mvnMods <path to maven project's root dir> [gv]"
    (mvnRootDir:"gv":_) -> parseModuleStructure mvnRootDir >>= displayTreeGraphviz
    (mvnRootDir:_)      -> parseModuleStructure mvnRootDir >>= displayTreeShowdot
       
type ArtifactId = String

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

-- | Conversion to dot source -- using graphviz library canvas
displayTreeGraphviz :: Tree ArtifactId -> IO ()
displayTreeGraphviz t = runGraphvizCanvas' (renderDotGraph t) Xlib

renderDotGraph :: Tree ArtifactId -> DotGraph String
renderDotGraph tree = setStrictness True . digraph' $ do
    graphAttrs [RankDir FromLeft]
    mapM renderEdge $ treeToEdgeList tree
  where renderEdge (from, to) = from --> to

-- | Conversion to dot source -- plain strings
toDotSource :: Tree ArtifactId -> String
toDotSource = ("strict digraph {\nrankdir=LR\n" ++ ) . (++"}") . unlines . map showEdge . treeToEdgeList
    where showEdge (from, to) = printf "\"%s\" -> \"%s\"" from to

displayTreeShowdot :: Tree ArtifactId -> IO ()
displayTreeShowdot = showDot . toDotSource

-- | Utility
treeToEdgeList :: Tree a -> [(a,a)]
treeToEdgeList (Node root subforest) =
    map (\subtree -> (root, rootLabel subtree)) subforest ++
    concatMap treeToEdgeList subforest


