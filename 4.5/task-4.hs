data Tree a = Leaf a
            | Node (Tree a) (Tree a)

avg :: Tree Int -> Int
avg t = let (c, s) = go t in s `div` c

  where go :: Tree Int -> (Int, Int)
        go (Leaf v) = (1, v)
        go (Node l r) = sum (go l) (go r)
        sum (l, r) (ll, rr) = (l + ll, r + rr)
