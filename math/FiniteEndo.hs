module FiniteEndo where

import Control.Monad (zipWithM_, replicateM)
import Data.List (nub, sort)
import Data.GraphViz.Types.Monadic
import Data.GraphViz

-- Endomorphisms on finite set [1..n] represented as list
-- e.g. [1,3,2] represents endo e, such that e(1) = 1, e(2) = 3 and e(3) = 2
type FEndo = [Int]

generateAll :: Int -> [FEndo]
generateAll n = replicateM n [1..n]

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
f `compose` g = map (\gx -> f !! (gx-1)) g

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

eval :: FEndo -> Int -> Int --TODO should return Maybe?
eval e x = e !! (x+1)

-- Endo visualization using graphviz
display :: FEndo -> IO ()
display e  = runGraphvizCanvas' gr Xlib
  where gr = digraph' $ renderEdges e

renderEdges :: FEndo -> Dot Int
renderEdges e = zipWithM_ (-->) (domain e) e
