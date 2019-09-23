seqA :: Integer -> Integer
seqA 0 = 1
seqA 1 = 2
seqA 2 = 3
seqA n
  = let kCalc :: Integer -> Integer -> Integer -> Integer
        kCalc kv kv1 kv2 = kv2 + kv1 - 2 * kv

        inner :: Integer -> Integer -> Integer -> Integer -> Integer
        inner kv kv1 kv2 k
          | k == n = kv2
          | otherwise = inner kv1 kv2 (kCalc kv kv1 kv2) (k + 1)
      in inner 1 2 3 2

