data Reader r a = Reader { runReader :: (r -> a) }


instance Functor (Reader r) where
  fmap f a = undefined

instance Applicative (Reader r) where
  pure x = Reader $ \_ -> x

instance Monad (Reader r) where
  return x = Reader $ \_ -> x
  m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r

local' :: (r -> r') -> Reader r' a -> Reader r a
local' f m =  Reader $ (runReader m) . f


