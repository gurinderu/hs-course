data Person = Person{firstName :: String, lastName :: String, age :: Int}

abbrFirstName :: Person -> Person
abbrFirstName p@Person{firstName = fn}
  | length fn > 2 = p{firstName = head fn : "."}
  | otherwise = p
