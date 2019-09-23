sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 xs ys zs = xs `sum2` ys `sum2` zs
  where sum2 :: Num a => [a] -> [a] -> [a]
        sum2 xs [] = xs
        sum2 [] ys = ys
        sum2 (x : xs) (y : ys) = (x + y) : (sum2 xs ys)
