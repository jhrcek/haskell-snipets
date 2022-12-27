{-# LANGUAGE BangPatterns #-}

import Data.Function (fix)

f :: (Integer -> Integer) -> Integer -> Integer
f mf 0 = 0
f mf n =
    max n $
        mf (div n 2)
            + mf (div n 3)
            + mf (div n 4)

-- Memoizing using a list

-- The memoizing functionality depends on this being in eta reduced form!
memoList :: ((Integer -> Integer) -> Integer -> Integer) -> Integer -> Integer
memoList f = memoList_f
  where
    memoList_f = (memo !!) . fromInteger
    memo = map (f memoList_f) [0 ..]

faster_f :: Integer -> Integer
faster_f = memoList f

-- Memoizing using a tree

data Tree a = Tree (Tree a) a (Tree a)

data FTree a = Node (FTree a) a (FTree a) | Leaf a deriving (Show)

takeNLevels :: Tree a -> Int -> FTree a
takeNLevels (Tree l x r) 0 = Leaf x
takeNLevels (Tree l x r) n = Node (takeNLevels l (n - 1)) x (takeNLevels r (n - 1))

instance Functor Tree where
    fmap f (Tree l m r) = Tree (fmap f l) (f m) (fmap f r)

index :: Tree a -> Integer -> a
index (Tree _ m _) 0 = m
index (Tree l _ r) n = case (n - 1) `divMod` 2 of
    (q, 0) -> index l q
    (q, 1) -> index r q

nats :: Tree Integer
nats = go 0 1
  where
    go !n !s = Tree (go l s') n (go r s')
      where
        l = n + s
        r = l + s
        s' = s * 2

toList :: Tree a -> [a]
toList as = map (index as) [0 ..]

-- The memoizing functionality depends on this being in eta reduced form!
memoTree :: ((Integer -> Integer) -> Integer -> Integer) -> Integer -> Integer
memoTree f = memoTree_f
  where
    memoTree_f = index memo
    memo = fmap (f memoTree_f) nats

fastest_f :: Integer -> Integer
fastest_f = memoTree f
