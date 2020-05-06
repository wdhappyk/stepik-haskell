module BeenTree where

data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf _)   = 0
height (Node l r) = 1 + max (height l) (height r)

size :: Tree a -> Int
size (Leaf _) = 1
size (Node l r) = 1 + size l + size r

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf x) = (1, x)
    go (Node l r) = (lc+rc, ls+rs) where
      (lc,ls) = go l
      (rc,rs) = go r
