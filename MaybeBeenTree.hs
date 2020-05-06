data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

instance Functor Tree where
  fmap g (Leaf x) = Leaf (fmap g x)
  fmap g (Branch l x r) = Branch (fmap g l) (fmap g x) (fmap g r)