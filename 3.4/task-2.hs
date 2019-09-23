lengthList :: [a] -> Int
lengthList = foldr (\ _ x -> x + 1) 0
