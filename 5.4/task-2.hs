data Board = Board Int deriving (Eq,Show)

nextPositions :: Board -> [Board]
nextPositions (Board i) = 
  [
  Board (i + 1),
  Board (i + 2),
  Board (i + 3) 
  ]

nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN b n pred | n < 0     = []
                        | n == 0    = filter pred [b]
                        | otherwise = do 
                                  next <- nextPositions b
                                  nextPositionsN next (n - 1) pred

f:: Board->Bool
f (Board i) = odd i
                                         

