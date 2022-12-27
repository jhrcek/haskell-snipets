data Wheel = Wheel Integer [Integer] deriving (Show)

roll :: Wheel -> [Integer]
roll (Wheel n rs) = [n * k + r | k <- [0 ..], r <- rs]

w0 :: Wheel
w0 = Wheel 1 [1]

nextSize :: Wheel -> Integer -> Wheel
nextSize (Wheel n rs) p =
    Wheel
        (p * n)
        [ r2 | k <- [0 .. (p - 1)], r <- rs, let r2 = n * k + r, r2 `mod` p /= 0
        ]

mkWheel :: [Integer] -> Wheel
mkWheel ds = foldl nextSize w0 ds

primes :: [Integer]
primes = small ++ large
  where
    1 : p : candidates = roll $ mkWheel small
    small = [2, 3, 5, 7]
    large = p : filter isPrime candidates
    isPrime n =
        all (not . divides n) $
            takeWhile (\p -> p * p <= n) large
    divides n p = n `mod` p == 0
