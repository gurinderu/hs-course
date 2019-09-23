newtype Xor = Xor{getXor :: Bool}
                deriving (Eq, Show)

instance Monoid Xor where
        mempty = Xor False
        mappend a b
          | a == b = Xor False
          | otherwise = Xor True
