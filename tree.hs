import Control.Monad.State

data Tree a = Leaf a | Fork (Tree a) a (Tree a)
  deriving Show

t1 = Leaf ()
t2 = Fork (Fork (Leaf ()) () (Leaf ())) () (Leaf ())


numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (numberTree' tree) 0

inc :: State Integer ()
inc = modify (+1)

numberTree' :: Tree () -> State Integer (Tree Integer)
numberTree' (Leaf _) = do
  inc
  n <- get
  return (Leaf n)
numberTree' (Fork l _ r) = do
  left <- numberTree' l
  inc
  m <- get
  right <- numberTree' r
  return (Fork left m right)

