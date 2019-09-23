import Data.Function

multSecond = g `on` h

g :: Int -> Int -> Int
g a b = a * b

h :: (t, Int) -> Int
h pair = snd pair
