qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = lower ++ middle ++ greater
  where lower = qsort $ filter (< x) xs
        middle = x : filter (== x) xs
        greater = qsort $ filter (> x) xs
