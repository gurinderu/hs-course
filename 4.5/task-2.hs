data Nat = Zero
         | Suc Nat
             deriving (Eq, Show)

toNat :: Integer -> Nat
toNat 0 = Zero
toNat x = Suc $ toNat $ x - 1

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add Zero x = x
add x Zero = x
add x (Suc n) = add (Suc x) n

mul :: Nat -> Nat -> Nat
mul Zero _ = Zero
mul _ Zero = Zero
mul (Suc Zero) x = x
mul x (Suc Zero) = x
mul x (Suc n) = add (mul x n) x

fac :: Nat -> Nat
fac Zero = (Suc Zero)
fac x@(Suc n) = mul x $ fac n

