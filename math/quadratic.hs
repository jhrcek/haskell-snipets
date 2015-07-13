{- Simple example of using HUnit -}
import Test.HUnit


{-- Simple code to test in the same file for completeness --}
data QE a = QE a a a

instance (Ord a, Num a, Show a) => Show (QE a) where 
  show = showQE

showQE :: (Num a, Ord a, Show a) => QE a -> String
showQE (QE a b c) = fixCorners terms
  where 
    terms = term a "x^2" ++ term b "x" ++ term c ""
    fixCorners [] = "0"
    fixCorners (' ':'+':' ':rest) = rest
    fixCorners (' ':rest) = rest
    fixCorners str = str
    term c symbol | c == 0 = ""
                  | abs c == 1 && not (null symbol) = sign c ++ symbol
                  | otherwise = sign c ++ show (abs c) ++ symbol
    sign x | x >= 0    = " + "
           | otherwise = " - "

data QESol a = NoSol | SingleSol a | TwoSols a a deriving (Show)

solve :: (Floating a, Ord a) => QE a -> QESol a
solve qe@(QE a b c) = case d `compare` 0 of
    LT -> NoSol
    EQ -> SingleSol (-b / (2 * a))
    GT -> TwoSols ((-b + sqrt d) / (2 * a)) ((-b - sqrt d) / (2 * a))
  where d = discriminant qe

discriminant :: Num a => QE a -> a
discriminant (QE a b c) = b^2 - 4*a*c

eval :: Num a => QE a -> a -> a
eval (QE a b c) x = a*x^2 + b*x + c

vertex :: Fractional t => QE t -> (t, t)
vertex qe@(QE a b c) = let x = -b / (2*a) in (x, eval qe x)

{-- example HUnit tests --}

 -- run with: runTestTT toStringTests
runtests = runTestTT showTests

showTests = TestList 
  [ show (QE 0 0 0) ~?= "0"
  , show (QE 0 0 1) ~?= "1"
  , show (QE 0 1 0) ~?= "x"
  , show (QE 1 0 0) ~?= "x^2"
  , show (QE 1 1 1) ~?= "x^2 + x + 1"
  , show (QE 2 2 2) ~?= "2x^2 + 2x + 2"
  , show (QE 0 0 (-1)) ~?= "- 1"
  , show (QE 0 1 (-1)) ~?= "x - 1"
  , show (QE 0 (-1) 0) ~?= "- x"
  , show (QE 5 (-3) 1) ~?= "5x^2 - 3x + 1"
  , show (QE (-1) (-1) (-1)) ~?= "- x^2 - x - 1"
  , show (QE (-3) (-2) (-1)) ~?= "- 3x^2 - 2x - 1"
  ]
  
