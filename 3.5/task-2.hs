evenOnly :: [a] -> [a]
evenOnly
  = reverse .
      snd . foldl (\ (p, xs) x -> (p + 1, if odd p then x : xs else xs)) (0, [])

