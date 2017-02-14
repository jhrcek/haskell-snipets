module Main where

import Control.Monad (unless)
import Data.GraphViz (runGraphvizCanvas', setStrictness)
import Data.GraphViz.Attributes.Complete (Attribute (RankDir),
                                          RankDir (FromLeft))
import Data.GraphViz.Commands (GraphvizCanvas (Xlib), quitWithoutGraphviz)
import Data.GraphViz.Types.Generalised (DotGraph)
import Data.GraphViz.Types.Monadic (digraph', graphAttrs, (-->))
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Tree (Tree (Node, rootLabel), unfoldTreeM)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import Text.Printf (printf)
import Text.XML.HXT.Core (getText, hasName, readDocument, runX, (//>), (/>))

import Showdot (showDot)

main :: IO ()
main = do
    quitWithoutGraphviz "Graphviz not installed. Please install it to proceed (http://www.graphviz.org/)"
    parseArgs >>= mainWithGraphviz

data Opts = Opts
  { dirToScan     :: FilePath
  , useCanvas     :: Bool
  , prefixToStrip :: Maybe String
  }

parseArgs :: IO Opts
parseArgs = do
  args <- getArgs
  case args of
    [] -> usage >> exitFailure
    xs -> let dir = head args
              canvas =  "gv" `elem` args
              prefix = if "-p" `elem` xs then Just (last xs) else Nothing
          in return Opts
               { dirToScan = dir
               , useCanvas = canvas
               , prefixToStrip = prefix
               }

usage :: IO ()
usage = putStrLn "usage: mvnMods <path to maven project's root dir> [gv] [-p prefix-to-strip]"

mainWithGraphviz :: Opts -> IO ()
mainWithGraphviz o = do
    tree <- parseModuleStructure (dirToScan o)
    let tree' = fmap (maybe id strip (prefixToStrip o)) tree
    if useCanvas o
        then displayTreeGraphviz tree'
        else displayTreeShowdot tree'

strip :: String -> String -> String
strip p s = fromMaybe s $ stripPrefix p s

type ArtifactId = String

parseModuleStructure :: FilePath -- ^ Root directory of maven project
                     -> IO (Tree ArtifactId) -- ^ Tree representing module dependencies extracted from pom.xml files
parseModuleStructure = unfoldTreeM getModuleNames

-- | Helper for tree unfolding
getModuleNames :: FilePath -- ^ Root directory of maven project
               -> IO (ArtifactId, [FilePath]) -- ^ (artifactId, [directories containing pom.xml of submodules])
getModuleNames dir = do
    pomExists <- doesFileExist pom
    unless pomExists (error $ "There is no pom.xml in " ++ dir)
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
