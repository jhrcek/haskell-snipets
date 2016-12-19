import Math.Combinat.Partitions.Integer
import qualified Math.Combinat.Numbers.Primes as P
import Data.List

primeFactors :: Integer -> [Integer]
primeFactors n = pf n P.primes []
  where
    pf 1 _ divisors = divisors
    pf n (p:ps) divisors
        | n `mod` p == 0 = pf (n `div` p) (p:ps) (p:divisors)
        | otherwise      = pf n ps divisors

data FiniteAbelianGroup = FAG [Integer] deriving Show

-- Enumerates all possible abelian groups of given order up to isomorphism.
-- e.g.
-- > allAbelianGroups 12
-- [FAG [3,2,2],FAG [3,4]]  -- means that there are 2 isomorphism classes of groups of order 12: Z3xZ2xZ2 and Z3xZ4
allAbelianGroups :: Integer -> [FiniteAbelianGroup]
allAbelianGroups order = map FAG possibleFactorizations
  where
    pf = map (\xs -> (head xs, length xs)) . group $ primeFactors order
    powerPartitions = map fromPartition . partitions -- the ways in which p^r can be partitioned: e.g 2^3 can be partitioned 2x2x2, 2x4 or 8
    possibleFactorizations = map concat . sequence $ map (\(prime, power) -> map (map (prime^)) $ powerPartitions power) pf

orderOfElement :: Integer -> Integer -> Integer
orderOfElement grOrder elm
    | 0 <= elm && elm < grOrder = grOrder `div` gcd elm grOrder
    | otherwise                 = error $ "Group Z_" ++ show grOrder ++ " can only have elements [0.." ++ show (grOrder -1) ++ "]. You tried " ++ show elm


-- > orderOfProductElement (FAG [3, 4]) [1,1]  -- order of element [1,1] of the direct product of Z3xZ4
-- 12
orderOfProductElement :: FiniteAbelianGroup -> [Integer] -> Integer
orderOfProductElement (FAG subgrOrders) elm
    | length subgrOrders == length elm =  let llcm = foldl1 lcm in  llcm $ zipWith orderOfElement subgrOrders elm
    | otherwise = error $ "group " ++ show (FAG subgrOrders) ++ " doesn't have element " ++ show elm

allElements :: FiniteAbelianGroup -> [[Integer]]
allElements (FAG ords) = mapM (enumFromTo 0 . subtract 1) ords 
