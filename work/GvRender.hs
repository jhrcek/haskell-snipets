{-# LANGUAGE FlexibleContexts #-}
module GvRender (
  renderGraph,
  printGraph,
  displayGraph,

  Construct(..),
  ConstructType(..),
  Edge,

  testGraph, printTest, displayTest
 ) where

import Data.GraphViz.Algorithms (transitiveReduction)
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Attributes.Colors.X11 (X11Color(LightSalmon, LightBlue, Green, Red))
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

data ConstructType = Annotation | Class | Enum | Interface deriving (Show, Eq, Ord)

type Edge = (Int, Int)

------------------- Nodes -------------------

renderNodes :: [Construct] -> Dot Int
renderNodes = mapM_ renderNode 
  where
     renderNode c = node (cid c) [ Label . StrLabel . pack $ cname c
                                 , FillColor [toWC $ X11Color ncolor]
                                 , Shape nshape
                                 ]
       where (nshape, ncolor) = attrs $ ctype c

-- color and shape assignment common for all constructs of given type
attrs :: ConstructType -> (Shape, X11Color)
attrs ct = case ct of 
    Annotation -> (Ellipse, Red)
    Class      -> (BoxShape, LightSalmon)
    Enum       -> (BoxShape, Green)
    Interface  -> (Ellipse, LightBlue)

------------------- Edges -------------------

renderEdges :: [Edge] -> Dot Int
renderEdges pairs = mapM_ (uncurry (-->)) pairs

------------------- Graph -------------------

renderGraph :: [Construct] -> [Edge] -> DotGraph Int
renderGraph constructs inheritancePairs = transitiveReduction . digraph' $ do
    graphAttrs [ RankDir FromBottom 
               , Overlap (PrismOverlap Nothing) --avoid overlapping nodes
               ]
    nodeAttrs [ Style [SItem Filled []] ] --needed for all nodes so that fill color is displayed
    edgeAttrs [ ArrowHead (AType [(ArrMod OpenArrow BothSides, Normal)]) ] -- all arrows with open head
    renderNodes constructs
    renderEdges inheritancePairs
------------------- Test -------------------

printGraph, displayGraph :: PrintDotRepr gr Int => gr Int -> IO ()
printGraph = T.putStrLn . printDotGraph
displayGraph g = runGraphvizCanvas Dot g Xlib

printTest = printGraph testGraph
displayTest = displayGraph testGraph

testGraph :: DotGraph Int
testGraph = renderGraph 
    [ Construct Class "java.lang" "Object" 1
    , Construct Class "java.lang" "String" 2
    , Construct Interface "java.util" "List" 3
    , Construct Class "java.util" "ArrayList" 4
    ]
    [(2,1), (4,3), (4,1)]
