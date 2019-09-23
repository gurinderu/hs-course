data Coord a = Coord a a

getCenter :: Double -> Coord Int -> Coord Double
getCenter size (Coord x y)
  = (Coord ((fromIntegral x) * size + half) ((fromIntegral y) * size + half))
  where half = size / 2

getCell :: Double -> Coord Double -> Coord Int
getCell size (Coord x y) = (Coord (floor $ x / size) (floor $ y / size))
