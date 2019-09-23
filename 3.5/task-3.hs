evenOnly :: [a] -> [a]
evenOnly
  = (foldr (\ (pos, el) acc -> if odd pos then acc else el : acc) []) .
      zip [1 ..]
