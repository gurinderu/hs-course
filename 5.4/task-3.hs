pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x
  | x <= 0 = []
  | otherwise =
    do c <- [1 .. x]
       b <- [1 .. c - 1]
       a <- [1 .. b - 1]
       True <- return (a ^ 2 + b ^ 2 == c ^ 2)
       return (a, b, c)

