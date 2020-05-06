import Data.Char

data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show

instance Functor (Entry k1 k2) where
  fmap g (Entry p v) = Entry p (g v)

instance Functor (Map k1 k2) where
  fmap g (Map list) = Map (map (fmap g) list)