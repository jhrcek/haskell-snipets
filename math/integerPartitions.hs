type Part = [Int]
type CmprPart = (Int, Part) -- partition compressed in the form of (# of 1s, rest), e.g. [1,1,1,2,4] -> (3,[2,4])

expand :: CmprPart -> Part
expand (n, rest) = replicate n 1 ++ rest

nextPart :: CmprPart -> CmprPart
nextPart (k, x : xs) = pack (x - 1) (k + x, xs)

pack :: Int -> CmprPart -> CmprPart
pack 1 (m, xs) = (m, xs)
pack k (m, xs)
    | k > m = pack (k - 1) (m, xs)
    | otherwise = pack k (m - k, k : xs)

generatePs :: CmprPart -> [Part]
generatePs p@(n, []) = [expand p]
generatePs p@(n, x : xs) = expand p : generatePs (nextPart p)

part :: Int -> [Part]
part n
    | n < 1 = error "part: argument <= 0"
    | n == 1 = [[1]]
    | otherwise = generatePs (0, [n])
