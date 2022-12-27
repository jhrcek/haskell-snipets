data Tree a
    = Empty
    | Node a (Tree a) (Tree a)
    deriving (Show, Eq)

data TreeCrumb a
    = LeftCrumb a (Tree a)
    | RightCrumb a (Tree a)
    deriving (Show, Eq)

type TreeZipper a = (Tree a, [TreeCrumb a])

tr :: Tree Int
tr =
    Node
        2
        (Node 1 Empty Empty)
        ( Node
            4
            (Node 3 Empty Empty)
            (Node 5 Empty Empty)
        )

goLeft :: TreeZipper a -> TreeZipper a
goLeft (Node x l r, bs) = (l, LeftCrumb x r : bs)

goRight :: TreeZipper a -> TreeZipper a
goRight (Node x l r, bs) = (r, LeftCrumb x l : bs)

goUp :: TreeZipper a -> TreeZipper a
goUp (r, RightCrumb x l : bs) = (Node x l r, bs)
goUp (l, LeftCrumb x r : bs) = (Node x l r, bs)

decrumb :: TreeZipper a -> TreeZipper a
decrumb = until (null . snd) goUp
