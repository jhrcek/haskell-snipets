-- Permutations of given list
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concatMap (ins x) (perms xs)
  where
    -- ins x ys: inserts an element x at all possible positions in ys
    ins :: a -> [a] -> [[a]]
    ins x [] = [[x]]
    ins x (y:ys) = (x:y:ys) : (map (y:) (ins x ys))
