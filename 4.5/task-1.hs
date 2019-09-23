data List a = Nil
            | Cons a (List a)
                deriving (Eq, Show)

fromList :: List a -> [a]
fromList = foldr (:) []
  where foldr f z Nil = z
        foldr f z (Cons x xs) = f x (foldr f z xs)

toList :: [a] -> List a
toList = foldr Cons Nil
