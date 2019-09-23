class Printable x where
        toString :: x -> [Char]

instance Printable Bool where
        toString True = "true"
        toString False = "false"

instance Printable () where
        toString _ = "unit type"
