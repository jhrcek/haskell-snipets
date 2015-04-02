data Tree a = Empty | Node a (Tree a) (Tree a)

-- | /O(n)/. Show the tree that implements the set. The tree is shown in a compressed, hanging format.
showTree :: Show a => Tree a -> String
showTree = showTreeWith True False
{- | /O(n)/. The expression (@showTreeWith hang wide map@) shows
the tree that implements the Tree. If @hang@ is
@True@, a /hanging/ tree is shown otherwise a rotated tree is shown. If
@wide@ is 'True', an extra wide version is shown.
> Tree> putStrLn $ showTreeWith True False $ fromDistinctAscList [1..5]
> 4
> +--2
> |  +--1
> |  +--3
> +--5
>
> Tree> putStrLn $ showTreeWith True True $ fromDistinctAscList [1..5]
> 4
> |
> +--2
> |  |
> |  +--1
> |  |
> |  +--3
> |
> +--5
>
> Tree> putStrLn $ showTreeWith False True $ fromDistinctAscList [1..5]
> +--5
> |
> 4
> |
> |  +--3
> |  |
> +--2
>    |
>    +--1
-}
showTreeWith :: Show a => Bool -> Bool -> Tree a -> String
showTreeWith hang wide t
  | hang = showsTreeHang wide [] t ""
  | otherwise = showsTree wide [] [] t ""

showsTree :: Show a => Bool -> [String] -> [String] -> Tree a -> ShowS
showsTree wide lbars rbars t = case t of 
  Empty -> showsBars lbars . showString "|\n"
  Node x Empty Empty
        -> showsBars lbars . shows x . showString "\n"
  Node x l r
        -> showsTree wide (withBar rbars) (withEmpty rbars) r .
           showWide wide rbars .
           showsBars lbars . shows x . showString "\n" .
           showWide wide lbars .
           showsTree wide (withEmpty lbars) (withBar lbars) l

showsTreeHang :: Show a => Bool -> [String] -> Tree a -> ShowS
showsTreeHang wide bars t = case t of
  Empty -> showsBars bars . showString "|\n"
  Node x Empty Empty 
        -> showsBars bars . shows x . showString "\n"
  Node x l r
        -> showsBars bars . shows x . showString "\n" .
           showWide wide bars .
           showsTreeHang wide (withBar bars) l .
           showWide wide bars .
           showsTreeHang wide (withEmpty bars) r

showWide :: Bool -> [String] -> String -> String
showWide wide bars
  | wide = showString (concat (reverse bars)) . showString "|\n"
  | otherwise = id

showsBars :: [String] -> ShowS
showsBars bars = case bars of
  [] -> id
  _ -> showString (concat (reverse (tail bars))) . showString node

node :: String
node = "+--"

withBar, withEmpty :: [String] -> [String]
withBar bars = "| ":bars
withEmpty bars = " ":bars
