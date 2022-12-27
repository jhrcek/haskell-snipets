main :: IO ()
main = print $ pqSort [2, 1, 3]

data PairingHeap a = Empty | Node a [PairingHeap a]

findFirst :: PairingHeap a -> Maybe a
findFirst Empty = Nothing
findFirst (Node n _) = Just n

merge :: Ord a => PairingHeap a -> PairingHeap a -> PairingHeap a
merge Empty p = p
merge p Empty = p
merge m@(Node x ps) n@(Node y qs)
    | y < x = Node y (m : qs)
    | otherwise = Node x (n : ps)

insert :: Ord a => a -> PairingHeap a -> PairingHeap a
insert x = merge (Node x [])

deleteFirst :: Ord a => PairingHeap a -> PairingHeap a
deleteFirst Empty = Empty
deleteFirst (Node _ ps) = foldr merge Empty $ pair ps
  where
    pair (a : b : xs) = merge a b : pair xs
    pair xs = xs

fromList :: Ord a => [a] -> PairingHeap a
fromList = foldr insert Empty

toList :: Ord a => PairingHeap a -> [a]
toList Empty = []
toList n@(Node x _) = x : toList (deleteFirst n)

pqSort :: Ord a => [a] -> [a]
pqSort = toList . fromList
