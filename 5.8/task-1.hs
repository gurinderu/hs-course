import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

readerToState :: Reader r a -> State r a
readerToState m = state $ \ e -> (runReader m e, e)

