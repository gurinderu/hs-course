integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = h * ((f (a) + f (b)) / 2 + (inner 0 (a + h) (n - 1)))
  where inner :: Double -> Double -> Int -> Double
        inner acc point n
          | n > 0 = inner (acc + (f point)) (point + h) (n - 1)
          | n == 0 = acc
        n = 1000
        h = (b - a) / (fromIntegral n)
