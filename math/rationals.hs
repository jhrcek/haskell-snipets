import Data.List (elemIndex)
import Data.Ratio ((%), numerator, denominator)

--return (potentially infinite) list of digits forming decimal expansion of given rational
decimalExpansion :: Rational -> [Integer]
decimalExpansion x
    | x < 0 = error "negative argument"
    | r == 0   = [q]
    | otherwise = q : decimalExpansion ((r * 10) % d)
  where
    (q, r) = quotRem n d
    n = numerator x
    d = denominator x


-- return (Integral part, non-repeating decimal part, periodically repeating part of decimal expansion)
decForm :: Rational -> (Integer, [Int], [Int])
decForm x
    | x < 0 = error "negative argument"
    | otherwise = (q, ys, zs)
  where
    (q, r) = quotRem n d
    n = numerator x
    d = denominator x
    (ys, zs) = decF (r*10) d []

decF :: Integer -> Integer -> [(Int, Integer)] -> ([Int], [Int])
decF n d xs
    | r == 0 = (reverse (q:(map fst xs)), [])
    | elem (q, r) xs = (ys, zs)
    | otherwise = decF (r*10) d ((q,r):xs)
  where
    (q', r) = quotRem n d
    q       = fromIntegral q'
    xs' = reverse xs
    Just k = elemIndex (q, r) xs'
    (ys, zs) = splitAt k (map fst xs')
