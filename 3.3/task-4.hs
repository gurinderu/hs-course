change :: (Ord a, Num a) => a -> [[a]]
change a
  | a > 0 = [x : y | x <- coins, (a - x) >= 0, y <- change (a - x)]
  | otherwise = [[]]

