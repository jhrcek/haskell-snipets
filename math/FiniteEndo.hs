module FiniteEndo where

import Control.Monad (replicateM, zipWithM_)
import Data.Functor ((<$>))
import Data.List (elemIndex, nub, sort)
import Data.Maybe (mapMaybe)

-- TODO move endo visualization stuff to separate module
import Data.GraphViz (runGraphvizCanvas)
import Data.GraphViz.Attributes.Complete
    ( Attribute (FixedSize, Height, Width)
    , NodeSize (SetNodeSize)
    )
import Data.GraphViz.Commands (GraphvizCanvas (Xlib), GraphvizCommand (Neato))
import Data.GraphViz.Types (printDotGraph)
import Data.GraphViz.Types.Generalised (DotGraph)
import Data.GraphViz.Types.Monadic (Dot, digraph', nodeAttrs, (-->))
import Data.Text.Lazy (unpack)

-- Endomorphisms on finite set [1..n] represented as list
-- e.g. [1,3,2] represents endo e, such that e(1) = 1, e(2) = 3 and e(3) = 2
type FEndo = [Int]

generateAll :: Int -> [FEndo]
generateAll n = replicateM n [1 .. n]

domain :: FEndo -> [Int]
domain e = [1 .. length e]

codomain :: FEndo -> [Int]
codomain = sort . nub

isIdentity :: FEndo -> Bool
isIdentity e = e == domain e

isIsomorphism :: FEndo -> Bool
isIsomorphism e = length e == length (nub e)

-- f `compose` g is like f . g
compose :: FEndo -> FEndo -> FEndo
f `compose` g = map (\gx -> f !! (gx - 1)) g

-- idempotent is endomorphism that when composed with itself results in itself
isIdempotent :: FEndo -> Bool
isIdempotent e = e `compose` e == e

-- involution is endomorphism that is its own inverse (https://oeis.org/A000085)
isInvolution :: FEndo -> Bool
isInvolution e = isIdentity $ e `compose` e

isRetractionOf :: FEndo -> FEndo -> Bool
r `isRetractionOf` f = isIdentity $ r `compose` f

isSectionOf :: FEndo -> FEndo -> Bool
s `isSectionOf` f = isIdentity $ f `compose` s

fixedPointCount :: FEndo -> Int
fixedPointCount = length . filter id . zipWith (==) [1 ..]

inverseOf :: FEndo -> Maybe FEndo
inverseOf e
    | isIsomorphism e = Just $ invert e
    | otherwise = Nothing
  where
    invert e = mapMaybe (\x -> (+ 1) <$> elemIndex x e) [1 .. length e]

eval :: FEndo -> Int -> Int -- TODO should return Maybe?
eval e x = e !! (x + 1)

-- Endo visualization using graphviz
display :: FEndo -> IO ()
display e = runGraphvizCanvas Neato (renderGraph e) Xlib

renderGraph :: FEndo -> DotGraph Int
renderGraph e = digraph' $ nodeAttrs attrs >> renderEdges e
  where
    renderEdges :: FEndo -> Dot Int
    renderEdges endo = zipWithM_ (-->) (domain endo) endo
    attrs = [FixedSize SetNodeSize, Width 0.25, Height 0.25]

dotSource :: DotGraph Int -> String
dotSource = unpack . printDotGraph
