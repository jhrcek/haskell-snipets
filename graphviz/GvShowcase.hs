import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Colors
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Commands
import Data.GraphViz.Types.Generalised
import Data.GraphViz.Types.Monadic

main :: IO ()
main  = mapM_ display [nodeShapesDemo, nodeColorsDemo]
    where display gr = runGraphvizCanvas' gr Xlib

nodeShapesDemo :: DotGraph String
nodeShapesDemo = graph' $ mapM_ mkNode allShapes
  where
    mkNode :: Shape -> Dot String
    mkNode s = node (show s) [Shape s]

    allShapes = [minBound..] :: [Shape]

nodeColorsDemo :: DotGraph String
nodeColorsDemo = graph' $ mapM_ mkNode allColors
  where
    mkNode :: X11Color -> Dot String
    mkNode col = node (show col) [
        Style [SItem Filled []],
        FillColor [toWC $ X11Color col]
      ]

    allColors = [minBound..] :: [X11Color]
