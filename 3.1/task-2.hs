nTimes :: a -> Int -> [a]
nTimes x 0 = []
nTimes x n = x : (nTimes x $ n - 1)
