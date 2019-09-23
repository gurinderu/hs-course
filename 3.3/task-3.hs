-- data Odd = Odd Integer deriving (Eq,Show)
-- не убирайте комментарий с предыдущей строки
-- определение Odd уже присутствует в вызывающей программе
instance Enum Odd where
        toEnum x = Odd (toInteger x)
        fromEnum (Odd x) = fromInteger x
        succ (Odd x) = Odd (x + 2)
        pred (Odd x) = Odd (x - 2)
        enumFrom (Odd x) = map Odd [x, x + 2 ..]
        enumFromTo (Odd x) (Odd y) = map Odd [x, x + 2 .. y]
        enumFromThen (Odd x) (Odd y) = map Odd [x, y ..]
        enumFromThenTo (Odd x) (Odd y) (Odd z) = map Odd [x, y .. z]

