data Log a = Log [String] a

returnLog :: a -> Log a
returnLog = Log []

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog l f = let 
    Log msgA a = l
    Log msgB b = f a
    in Log (msgA++msgB) b



instance Functor Log where
    fmap f (Log msg a) = Log msg (f a)

instance Applicative Log where
    pure = returnLog

instance Monad Log where
    (>>=) = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList = foldl (>>=) . pure
