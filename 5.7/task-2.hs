import Control.Monad.Writer
import GHC.Integer

type Shopping = Writer (Sum Integer) () 

purchase :: String -> Integer -> Shopping
purchase item cost = tell (Sum cost) 

total :: Shopping -> Integer
total = getSum . execWriter
