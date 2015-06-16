-- Based on chapter 9.1 of Haskell Road to Math, Logic and Programming
-- Difference Analysis of Polynomial Sequences

-- Given sequence of integers [f x | x <- [0..n]] generated by polynomial f
-- returns list of successive differences [f 1 - f0, f2 - f1 ..]
difs :: [Integer] -> [Integer]
difs [] = []
difs [x] = []
difs (x:y:ys) = (y-x) : difs (y:ys)

--Given polynomial sequence compute recursively its difs, difs of difs, ... until we get constant sequence (corresponding to constant polynomial)
difLists :: [[Integer]] -> [[Integer]]
difLists [] = []
difLists lists@(xs:xss) = if constant xs then lists else difLists $ difs xs : lists
  where
    constant (n:m:ms) = all (==n) (m:ms)
    constant _        = error "constant: lack of data or not a polynomial function"

-- Given polynomial sequence s return list of last elements of difference sequences generated from s
genDifs :: [Integer] -> [Integer]
genDifs xs = map last $ difLists [xs]

nextD :: [Integer] -> [Integer]
nextD [] = error "nextD: no data"
nextD [n] = [n]
nextD (x:y:ys) = x : nextD (x+y : ys) --Migt be easier to implement as scanl1 (+)

-- Compute next element of the polynomial sequence
next :: [Integer] -> Integer
next = last . nextD . genDifs

-- Continues given sequence as if they were generated by the same polynomial
continue :: [Integer] -> [Integer]
continue xs = map last $ iterate nextD differences
  where differences = nextD (genDifs xs)

-- Given first few integers of polynomial-generated sequence, find the degree of the generating polynomial
degree :: [Integer] -> Int
degree xs = length (difLists [xs]) - 1
