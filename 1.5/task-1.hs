doubleFact :: Integer -> Integer
doubleFact 1 = 1
doubleFact 2 = 2
doubleFact n = if n == 0 then 1 else n * doubleFact (n - 2)
