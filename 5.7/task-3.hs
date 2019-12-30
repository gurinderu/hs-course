import Control.Monad.Writer
import Data.Monoid
import GHC.Integer

type Shopping = Writer ([(Integer, String)]) ()

purchase :: String -> Integer -> Shopping
purchase item cost = tell $ [(cost, item)]

total :: Shopping -> Integer
total s = sum $ fmap fst (execWriter s)

items :: Shopping -> [String]
items s = fmap snd (execWriter s)
