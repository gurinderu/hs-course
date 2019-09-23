groupElems :: Eq a => [a] -> [[a]]
groupElems x = helper x []
  where helper :: Eq a => [a] -> [[a]] -> [[a]]
        helper [] acc = reverse acc
        helper (x : xs) [] = helper xs [[x]]
        helper (x : xs) ((y : ys) : zs)
          | x == y = helper xs ((:) (x : y : ys) zs)
          | otherwise = helper xs ((:) [x] ((:) ((:) y ys) zs))
