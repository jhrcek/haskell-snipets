{-# LANGUAGE FlexibleContexts #-}
module GvRender (
  renderGraph,
  printGraph,
  displayGraph,

  Construct(..),
  ConstructType(..),
  Edge
 ) where

import Data.GraphViz.Algorithms (transitiveReduction)
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Attributes.Colors.X11 (X11Color(LightSalmon, LightBlue))
import Data.GraphViz.Commands (runGraphvizCanvas, GraphvizCanvas(Xlib))
import Data.GraphViz.Types (PrintDotRepr, printDotGraph)
import Data.GraphViz.Types.Monadic (Dot, nodeAttrs, edgeAttrs, graphAttrs, node, digraph', (-->))
import Data.GraphViz.Types.Canonical (DotGraph)
import Data.Text.Lazy (pack)
import qualified Data.Text.Lazy.IO as T


data Construct = Construct {
    ctype :: ConstructType,
    cpkg :: String,
    cname :: String,
    cid :: Int
  } deriving Show

data ConstructType = Annotation | Class | Enum | Interface deriving (Show, Eq)

type Edge = (Int, Int)


------------------- Nodes -------------------

renderNodes :: Shape -> X11Color -> [Construct] -> Dot Int
renderNodes shape color constructs = do
    nodeAttrs [
        Shape shape,
        Style [SItem Filled []],
        FillColor [toWC $ X11Color color]
      ]
    mapM_ renderNode constructs
  where renderNode construct = node (cid construct) [Label (StrLabel $ pack (cname construct))]

classNodes, interfaceNodes :: [Construct] -> Dot Int
classNodes = renderNodes BoxShape LightSalmon
interfaceNodes = renderNodes Ellipse LightBlue

------------------- Edges -------------------

renderEdges :: [Edge] -> Dot Int
renderEdges pairs = edgesWithOpenArrow >> mapM_ (uncurry (-->)) pairs
  where 
    edgesWithOpenArrow = edgeAttrs [ArrowHead (AType [(ArrMod OpenArrow BothSides, Normal)])] 

------------------- Graph -------------------

renderGraph :: [Construct] -> [Construct] -> [Edge] -> DotGraph Int
renderGraph clss ifs inheritancePairs = transitiveReduction . digraph' $ do
    graphAttrs [RankDir FromBottom]
    classNodes clss
    interfaceNodes ifs
    renderEdges inheritancePairs

------------------- Test -------------------

printGraph, displayGraph :: PrintDotRepr gr Int => gr Int -> IO ()
printGraph = T.putStrLn . printDotGraph
displayGraph g = runGraphvizCanvas Dot g Xlib

{-iprintTest, displayTest :: IO ()
printTest = printGraph testGraph
displayTest = displayGraph testGraph

testGraph :: DotGraph String
testGraph = renderGraph
  ["ArrayList", "AbstractList", "AbstractSequentialList", "Object", "AbstractCollection", "LinkedList"]
  ["List", "Iterable", "Queue"]
  [("List", "Collection"), ("ArrayList", "AbstractList"), ("AbstractList", "AbstractCollection"),
   ("AbstractCollection", "Object"), ("LinkedList", "AbstractSequentialList"), ("AbstractSequentialList", "AbstractList"),
   ("Queue", "Collection"), ("Collection", "Iterable"), ("ArrayList","List"), ("LinkedList", "Queue"),
   ("LinkedList", "List"), ("AbstractCollection", "Collection"), ("AbstractList", "List"),
   ("ArrayList", "Collection"{-transitive edge to test tr. edge removal-})]
-}
