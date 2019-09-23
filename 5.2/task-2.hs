data Log a = Log [String] a
               deriving (Eq, Show)

returnLog :: a -> Log a
returnLog = Log []

