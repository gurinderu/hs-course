data Coord a = Coord a a

distance :: Coord Double -> Coord Double -> Double
distance (Coord xa ya) (Coord xb yb) = sqrt (((xa - xb) ^ 2) + ((ya - yb) ^ 2))

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord xa ya) (Coord xb yb) = (abs (xa - xb)) + (abs (ya - yb))
