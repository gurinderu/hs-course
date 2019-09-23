fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci n
  | n >= 0 = inner 0 1 n
  | n < 0 = inner2 0 1 n

inner :: Integer -> Integer -> Integer -> Integer
inner prev cur 1 = cur
inner prev cur n = inner cur (cur + prev) (n - 1)

inner2 :: Integer -> Integer -> Integer -> Integer
inner2 prev cur (-1) = cur
inner2 prev cur n = inner2 cur (prev - cur) (n + 1)
