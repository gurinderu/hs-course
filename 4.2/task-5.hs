data Bit = Zero
         | One

data Sign = Minus
          | Plus

data Z = Z Sign [Bit]

add :: Z -> Z -> Z
add (Z Minus b) (Z Minus b2) = (Z Minus (add' b b2))
add (Z Plus b) (Z Plus b2) = (Z Plus (add' b b2))
add l@(Z s b) r@(Z s1 b2)
  | compareBits b b2 == LT = add r l
  | otherwise = (Z s (removeZeros $ result))
  where padded = zipWithPad b b2
        subtrahend = map snd padded
        minuend = map fst padded
        result = take (length minuend) tmp
        tmp = (add' minuend . add' [One] . map inverseBit) subtrahend

zipWithPad :: [Bit] -> [Bit] -> [(Bit, Bit)]
zipWithPad (x : xs) (y : ys) = (x, y) : (zipWithPad xs ys)
zipWithPad (x : xs) [] = (x, Zero) : (zipWithPad xs [])
zipWithPad [] (y : ys) = (Zero, y) : (zipWithPad [] ys)
zipWithPad [] [] = []

compareBits :: [Bit] -> [Bit] -> Ordering
compareBits [] [] = EQ
compareBits _ [] = GT
compareBits [] _ = LT
compareBits (x : xs) (y : ys) = cmp (compareBits xs ys) x y
  where cmp :: Ordering -> Bit -> Bit -> Ordering
        cmp EQ One One = EQ
        cmp EQ Zero Zero = EQ
        cmp EQ One Zero = GT
        cmp EQ Zero One = LT
        cmp p _ _ = p

removeZeros :: [Bit] -> [Bit]
removeZeros = foldr (f) []
  where f Zero [] = []
        f p xs = p : xs

add' :: [Bit] -> [Bit] -> [Bit]
add' b b2 = reverse $ addCarry $ foldl f (Zero, []) (zipWithPad b b2)
  where addCarry :: (Bit, [Bit]) -> [Bit]
        addCarry (Zero, bs) = bs
        addCarry (b, bs) = b : bs

        f :: (Bit, [Bit]) -> (Bit, Bit) -> (Bit, [Bit])
        f (carry, acc) (b, b1)
          = let sum = addBits b b1 carry in (fst sum, (snd sum) : acc)

addBits :: Bit -> Bit -> Bit -> (Bit, Bit)
addBits Zero Zero Zero = (Zero, Zero)
addBits One One One = (One, One)
addBits Zero One One = (One, Zero)
addBits One Zero One = (One, Zero)
addBits One One Zero = (One, Zero)
addBits _ _ _ = (Zero, One)

inverseZ :: Z -> Z
inverseZ (Z Minus b) = (Z Plus b)
inverseZ (Z Plus b) = (Z Minus b)

inverseBit :: Bit -> Bit
inverseBit One = Zero
inverseBit Zero = One

mul :: Z -> Z -> Z
mul l@(Z Minus _) r@(Z Plus _) = (inverseZ . mul r . inverseZ) l
mul l@(Z Plus _) r@(Z Minus _) = mul r l
mul (Z _ b) (Z _ b1) = (Z Plus ((foldr add' [] . map f . zip [0 ..]) b))
  where f :: (Int, Bit) -> [Bit]
        f (shift, Zero) = replicate (length b1 + shift) Zero
        f (shift, One) = (replicate shift Zero) ++ (map (mul' One) b1)
        mul' One One = One
        mul' _ _ = Zero
