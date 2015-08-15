import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Colors --X11Color
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types.Generalised
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Commands

main  = mapM_ display  [nodeShapesDemo, nodeColorsDemo]
    where display gr = runGraphvizCanvas' gr Xlib

nodeShapesDemo :: DotGraph String
nodeShapesDemo = graph' $ mapM_ mkNode allShapes
  where 
    mkNode :: Shape -> Dot String 
    mkNode shape = node (show shape) [Shape shape]
    
    allShapes = [minBound..] :: [Shape]

nodeColorsDemo :: DotGraph String
nodeColorsDemo = graph' $ mapM_ mkNode allColors
  where
    mkNode :: X11Color -> Dot String
    mkNode color = node (show color) [
        Style [SItem Filled []],
        FillColor [toWC $ X11Color color]
      ]

    allColors = [minBound..] :: [X11Color]
