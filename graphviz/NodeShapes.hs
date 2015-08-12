import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Commands

main  = runGraphvizCanvas' gr Xlib
  where gr = graph' $ mapM_ mkShapedNode allShapes

mkShapedNode :: Shape -> Dot String 
mkShapedNode shape = nodeAttrs [Shape shape] >> node' (show shape)

allShapes :: [Shape]
allShapes = [minBound .. ] -- functionally equivalent to listing all data constructors like (but requires type to be instance of Bounded & Enum: [BoxShape,  Polygon, Ellipse, Circle, PointShape, Egg, Triangle, PlainText, DiamondShape, Trapezium, Parallelogram, House, Pentagon, Hexagon, Septagon, Octagon, DoubleCircle, DoubleOctagon, TripleOctagon, InvTriangle, InvTrapezium, InvHouse, MDiamond, MSquare, MCircle, Square, Star, Underline, Note, Tab, Folder, Box3D, Component, Promoter, CDS, Terminator, UTR, PrimerSite, RestrictionSite, FivePovOverhang, ThreePovOverhang, NoOverhang, Assembly, Signature, Insulator, Ribosite, RNAStab, ProteaseSite, ProteinStab, RPromoter, RArrow, LArrow, LPromoter, Record, MRecord]
