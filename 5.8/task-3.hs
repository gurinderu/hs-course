import Control.Monad
import Control.Monad.Trans.State

fibStep :: State (Integer, Integer) ()
fibStep = state $ \ (l, r) -> ((), (r, l + r))

execStateN :: Int -> State s a -> s -> s
execStateN n m = execState $ replicateM n m

