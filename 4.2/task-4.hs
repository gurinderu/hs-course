data Shape = Circle Double
           | Rectangle Double Double

square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Rectangle x y)
  | x == y = True
  | otherwise = False
isSquare _ = False
