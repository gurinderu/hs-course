import Control.Monad.Trans.State

data Tree a = Leaf a
            | Fork (Tree a) a (Tree a)
                deriving (Eq, Show)

numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (traversal tree) 1
  where traversal :: Tree () -> State Integer (Tree Integer)
        traversal (Fork l _ r)
          = do lr <- traversal l
               n <- get
               put (n + 1)
               rr <- traversal r
               return (Fork lr n rr)
        traversal (Leaf _)
          = do n <- get
               put (n + 1)
               return (Leaf n)

