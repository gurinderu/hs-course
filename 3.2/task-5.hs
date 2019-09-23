perms :: [a] -> [[a]]
perms [] = [[]]
perms (x : xs) = concatMap (helper x) (perms xs)
  where helper x [] = [[x]]
        helper x (y : ys) = (x : y : ys) : map (y :) (helper x ys)
