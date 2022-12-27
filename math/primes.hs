import Math.Combinat.Numbers.Primes (primes)

primeDivs :: Integer -> [Integer]
primeDivs n = pd n primes
  where
    pd 1 _ = []
    pd n (p : ps)
        | n `mod` p == 0 = p : pd (n `div` p) (p : ps)
        | otherwise = pd n ps

primes2 = sieve [2 ..]
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]

primes3 = 2 : [x | x <- [3, 5 ..], isprime x]
  where
    isprime x = all (\p -> x `mod` p > 0) (factorsToTry x)
    factorsToTry x = takeWhile (\p -> p * p <= x) primes3
