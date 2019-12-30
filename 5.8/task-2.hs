import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

writerToState :: Monoid w => Writer w a -> State w a
writerToState m = let (a, w) = runWriter m in state $ \ e -> (a, e `mappend` w)
