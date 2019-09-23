meanList = someFun . foldr (\ x (s, c) -> (x + s, c + 1)) (0, 0)
  where someFun (s, c) = s / c
