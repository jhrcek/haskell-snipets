import Data.DList (empty, toList, snoc)
-- Difference lists are a list-like type supporting O(1) append (snoc)

-- unusable - quadratic time complexity
revSlow :: [a] -> [a]
revSlow [] = []
revSlow (x:xs) = revSlow xs ++ [x]

-- Using list accumulator - 3rd fastest
revAcc :: [a] -> [a]
revAcc li = r li []
  where r [] acc = acc
        r (x:xs) acc = r xs (x:acc)

-- Based on Difference list - 2nd fastest
revDList :: [a] -> [a]
revDList = toList . r
  where r []     = empty
        r (x:xs) = snoc (r xs) x 

-- From prelude - fastest
revPrelude :: [a] -> [a]
revPrelude = foldl (flip (:)) []
--
