module SetEq where

import Data.List (delete)

newtype Set a = Set [a]

instance Eq a => Eq (Set a) where
    set1 == set2 = set1 `subSet` set2 && set2 `subSet` set1

instance (Show a) => Show (Set a) where
    showsPrec _ (Set s) = showSet s

instance Functor Set where
    fmap f (Set s) = Set $ map f s

showSet :: Show a => [a] -> ShowS
showSet [] = showString "0"
showSet (x : xs) = showChar '{' . shows x . showl xs
  where
    showl [] = showChar '}'
    showl (x : xs) = showChar ',' . shows x . showl xs

emptySet :: Set a
emptySet = Set []

isEmpty :: Set a -> Bool
isEmpty (Set []) = True
isEmpty _ = False

inSet :: Eq a => a -> Set a -> Bool
inSet x (Set s) = x `elem` s

subSet :: Eq a => Set a -> Set a -> Bool
subSet (Set []) _ = True
subSet (Set (x : xs)) set = (x `inSet` set) && subSet (Set xs) set

insertSet :: Eq a => a -> Set a -> Set a
insertSet x (Set ys)
    | x `inSet` Set ys = Set ys
    | otherwise = Set (x : ys)

deleteSet :: Eq a => a -> Set a -> Set a
deleteSet x (Set xs) = Set (delete x xs)

list2set :: Eq a => [a] -> Set a
list2set = foldr insertSet emptySet

powerSet :: Eq a => Set a -> Set (Set a)
powerSet (Set xs) = Set . map Set $ powerList xs

powerList :: [a] -> [[a]]
powerList = foldr f [[]]
  where
    f x pl = pl ++ map (x :) pl

takeSet :: Eq a => Int -> Set a -> Set a
takeSet n (Set xs) = Set (take n xs)

infixl 9 !!!
(!!!) :: Set a -> Int -> a
(Set xs) !!! n = xs !! n

----- Exercises -----
unionSet :: Eq a => Set a -> Set a -> Set a
unionSet (Set ls1) s2 = foldr insertSet s2 ls1

intersectSet :: Eq a => Set a -> Set a -> Set a
intersectSet (Set ls1) s2 = Set $ filter (`inSet` s2) ls1

differenceSet :: Eq a => Set a -> Set a -> Set a
differenceSet (Set ls1) s2 = Set $ filter (\x -> not $ x `inSet` s2) ls1

-- Stirling number : stirling n k = number of partitions of set with n elements into k partitions
stirling :: Integral a => a -> a -> a
stirling _ 1 = 1
stirling n k
    --  | k > n = 0
    | k == n = 1
    | otherwise = k * stirling (n - 1) k + stirling (n - 1) (k - 1)

-- Bell number: bell n = number of different partitions that can be made from a set with n elements
bell :: Integral a => a -> a
bell n = sum [stirling n k | k <- [1 .. n]]
