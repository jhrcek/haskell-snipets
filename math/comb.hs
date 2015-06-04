-- Subfactorial n  = number of Derangements on n element set
subfactorial :: Integral a => a -> a
subfactorial 0 = 0
subfactorial 1 = 0
subfactorial 2 = 1
subfactorial n = n * subfactorial (n - 1) + (-1)^n
