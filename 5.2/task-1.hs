data Log a = Log [String] a
               deriving (Eq, Show)

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = Log [msg] . f

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = join g (f x)
  where join :: (a -> Log b) -> (Log a) -> Log b
        join f (Log msg a) = makeLog msg (f a)

        makeLog :: [String] -> Log a -> Log a
        makeLog log (Log msg c) = Log (log ++ msg) c

