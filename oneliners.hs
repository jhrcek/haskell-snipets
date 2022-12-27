pascalRow n = scanl (\a x -> a * (n - x + 1) `div` x) 1 [1 .. n]
