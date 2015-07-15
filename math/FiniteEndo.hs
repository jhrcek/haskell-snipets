module FiniteEndo where

import Control.Applicative
import Data.List (nub)
import Data.Maybe (fromJust)

-- Endomorphisms on finite domain [1..n] represented as list
type FEndo = [(Int, Int)]

generateAll :: Int -> [FEndo]
generateAll n = mapM (possibleMappings n) [1..n]
  where
    --all possible ways that x can be mapped to [1..n]
    possibleMappings n x = (,) <$> [x] <*> [1..n]

range :: FEndo -> [Int]
range = nub . map snd

isBijective :: FEndo -> Bool
isBijective e = length e == length (range e)

compose :: FEndo -> FEndo -> FEndo
compose e1 e2 = map (\(a,b) -> (a, fromJust $ lookup b e2)) e1

isIdempotent :: FEndo -> Bool
isIdempotent e = e `compose` e == e
