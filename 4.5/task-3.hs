data Tree a = Leaf a
            | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Node l r) = (max (height l) (height r)) + 1
height (Leaf _) = 0

size :: Tree a -> Int
size (Node l r) = size l + size r + 1
size (Leaf _) = 1
