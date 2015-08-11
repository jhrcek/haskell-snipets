{-# LANGUAGE FlexibleContexts #-}
module GvRender (
  renderGraph,
  printGraph,
  displayGraph
 ) where

import Data.GraphViz.Algorithms (transitiveReduction)
import Data.GraphViz.Commands (runGraphvizCanvas, GraphvizCanvas(Xlib))
import Data.GraphViz.Types
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Types.Canonical (DotGraph)
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Attributes.Colors.X11
import Data.Text.Lazy (unpack)

------------------- Nodes -------------------

renderNodes :: Shape -> X11Color -> [String] -> Dot String
renderNodes shape color nodes = do
    nodeAttrs [
        Shape shape,
        Style [SItem Filled []],
        FillColor [toWC $ X11Color color]
      ]
    mapM_ node' nodes

classNodes, interfaceNodes :: [String] -> Dot String
classNodes = renderNodes BoxShape LightSalmon
interfaceNodes = renderNodes Ellipse LightBlue

------------------- Edges -------------------

renderEdges :: ArrowFill -> [(String, String)] -> Dot String
renderEdges arr pairs = do
    arrowHead arr
    mapM_ (uncurry (-->)) pairs
  where 
    -- shortcut for creating edge arrowhead attribute (OpenArrow / FilledArrow)
    arrowHead :: ArrowFill -> Dot String
    arrowHead fill = edgeAttrs [ArrowHead (AType [(ArrMod fill BothSides, Normal)])]

extendsPairs, implementsPairs :: [(String, String)] -> Dot String
extendsPairs = renderEdges OpenArrow
implementsPairs = renderEdges FilledArrow

------------------- Graph -------------------

renderGraph :: [String] -> [String] -> [(String, String)] -> [(String, String)] -> DotGraph String
renderGraph clss ifs expairs implpairs = transitiveReduction . digraph' $ do
    graphAttrs [RankDir FromBottom]
    classNodes clss
    interfaceNodes ifs
    extendsPairs expairs
    implementsPairs implpairs

------------------- Test -------------------

printGraph, displayGraph :: PrintDotRepr gr String => gr String -> IO ()
printGraph = putStrLn . unpack . printDotGraph
displayGraph g = runGraphvizCanvas Dot g Xlib

printTest, displayTest :: IO ()
printTest = printGraph testGraph
displayTest = displayGraph testGraph

testGraph :: DotGraph String
testGraph = renderGraph
  ["ArrayList", "AbstractList", "AbstractSequentialList", "Object", "AbstractCollection", "LinkedList"]
  ["List", "Iterable", "Queue"]
  [("List", "Collection"), ("ArrayList", "AbstractList"), ("AbstractList", "AbstractCollection"),
   ("AbstractCollection", "Object"), ("LinkedList", "AbstractSequentialList"), ("AbstractSequentialList", "AbstractList"),
   ("Queue", "Collection"), ("Collection", "Iterable")]
  [("ArrayList","List"), ("LinkedList", "Queue"), ("LinkedList", "List"), ("AbstractCollection", "Collection"), ("AbstractList", "List"),
   ("ArrayList", "Collection"{-transitive edge to test tr. edge removal-})]


