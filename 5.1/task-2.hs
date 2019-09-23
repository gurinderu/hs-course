newtype Maybe' a = Maybe'{getMaybe :: Maybe a}
                     deriving (Eq, Show)

instance Monoid a => Monoid (Maybe' a) where
        mempty = Maybe' (Just mempty)

instance Semigroup a => Semigroup (Maybe' a) where
        (<>) (Maybe' Nothing) _ = Maybe' Nothing
        (<>) _ (Maybe' Nothing) = Maybe' Nothing
        (<>) (Maybe' x) (Maybe' y) = Maybe' (x `mappend` y)
