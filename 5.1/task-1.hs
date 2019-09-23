newtype Xor = Xor{getXor :: Bool}
                deriving (Eq, Show)

instance Monoid Xor where
        mempty = Xor True

instance Semigroup Xor where
        (<>) a b
          | a == b = Xor False
          | otherwise = Xor True

