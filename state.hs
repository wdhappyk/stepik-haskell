import Control.Monad.State

fibStep :: State (Integer, Integer) ()
fibStep = do
    (a,b) <- get
    put (b, a+b)
