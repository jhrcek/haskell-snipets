import Math.Combinat.Numbers.Primes (primes)

primeDivs :: Integer -> [Integer]
primeDivs n  = pd n primes
  where 
    pd 1 _ = []
    pd n (p:ps) | n `mod` p == 0  = p : pd (n `div` p) (p:ps)
                | otherwise       = pd n ps


